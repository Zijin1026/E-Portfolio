# app.R
library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(DT)



sf::sf_use_s2(FALSE)

# -----------------------------
# 1) LOAD + PREP DATA
# -----------------------------
shp_path <- "GVan_DA_Final.shp"

gdf <- st_read(shp_path, quiet = TRUE) |>
  st_make_valid()

# drop heavy columns (optional)
drop_cols <- intersect(c("DGUID", "PRUID"), names(gdf))
if (length(drop_cols) > 0) gdf <- gdf |> select(-all_of(drop_cols))

# remove empties + keep only valid geometries for web mapping
gdf <- gdf[!st_is_empty(gdf$geometry), ]
gdf <- gdf[st_is_valid(gdf$geometry), ]

# simplify in projected CRS only (tolerance in map units)
if (!st_is_longlat(gdf)) {
  gdf <- st_simplify(gdf, dTolerance = 20, preserveTopology = TRUE)
}

# transform to WGS84 for leaflet
gdf <- st_transform(gdf, 4326)

# safe point for markers (centroid can fail / fall outside)
gdf$pt <- st_point_on_surface(gdf$geometry)

# map center via bbox (avoid union/centroid)
bb <- st_bbox(gdf)
center_lng <- as.numeric((bb["xmin"] + bb["xmax"]) / 2)
center_lat <- as.numeric((bb["ymin"] + bb["ymax"]) / 2)

# -----------------------------
# 2) CONFIG: FIELDS
# -----------------------------
ID <- "DAUID"

criteria <- c(
  "CBDDis", "StopDis", "popdensity", "H_maintain", "H_rent", "green_dens",
  "H_edu", "Age_", "Unemploy_R", "MedianInc", "DIST_Coast", "dis_scho", "dis_medi"
)

cost_criteria <- c("StopDis", "H_maintain", "H_rent", "Unemploy_R", "dis_medi", "dis_scho")
unknown_criteria <- c("CBDDis", "popdensity", "Age_", "MedianInc", "DIST_Coast")

labels <- c(
  "Distance to CBD",
  "Distance to Public Transportation Stations",
  "Population Density",
  "Owned House Maintenance fee",
  "House Rent",
  "Green Density",
  "Percentage of Bachelor Degree Holders",
  "Age",
  "Unemployment Rate",
  "Median Income",
  "Distance to Seashore",
  "Distance to Nearest School",
  "Distance to Hospital"
)

unknown_labels <- c(
  "Distance to CBD",
  "Population Density",
  "Average Age",
  "Median Income",
  "Distance to Seashore"
)

# -----------------------------
# 3) SCORING FUNCTION
# -----------------------------
compute_suitability <- function(df, weight, pref) {
  df <- df |> mutate(across(all_of(criteria), as.numeric))
  
  total_w <- sum(weight)
  if (total_w == 0) {
    w_norm <- rep(1 / length(weight), length(weight))
    names(w_norm) <- names(weight)
  } else {
    w_norm <- weight / total_w
  }
  
  for (col in criteria) {
    vals <- df[[col]]
    vmin <- suppressWarnings(min(vals, na.rm = TRUE))
    vmax <- suppressWarnings(max(vals, na.rm = TRUE))
    norm_col <- paste0(col, "_norm")
    
    if (!is.finite(vmin) || !is.finite(vmax) || vmax == vmin) {
      df[[norm_col]] <- 0
      next
    }
    
    if (col %in% unknown_criteria) {
      if (identical(pref[[col]], "higher")) {
        df[[norm_col]] <- (vals - vmin) / (vmax - vmin)
      } else {
        df[[norm_col]] <- (vmax - vals) / (vmax - vmin)
      }
    } else if (col %in% cost_criteria) {
      df[[norm_col]] <- (vmax - vals) / (vmax - vmin)
    } else {
      df[[norm_col]] <- (vals - vmin) / (vmax - vmin)
    }
  }
  
  df$suit_score <- 0
  for (col in criteria) {
    df$suit_score <- df$suit_score + df[[paste0(col, "_norm")]] * w_norm[[col]]
  }
  
  df
}

# -----------------------------
# 4) UI
# -----------------------------
ui <- fluidPage(
  titlePanel("DA Suitability Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Set importance (0–10)"),
      lapply(seq_along(criteria), function(i) {
        sliderInput(
          inputId = paste0("w_", criteria[i]),
          label = labels[i],
          min = 0, max = 10, value = 1, step = 1
        )
      }),
      hr(),
      h4("Preference for these criteria"),
      lapply(seq_along(unknown_criteria), function(i) {
        radioButtons(
          inputId = paste0("p_", unknown_criteria[i]),
          label = unknown_labels[i],
          choices = c("Lower is better" = "lower", "Higher is better" = "higher"),
          selected = "lower",
          inline = TRUE
        )
      }),
      hr(),
      checkboxInput("show_top10", "Show Top 10 markers", value = TRUE)
    ),
    mainPanel(
      leafletOutput("map", height = 700),
      hr(),
      h4("Top 10 most suitable dissemination areas"),
      DTOutput("top10_table")
    )
  )
)

# -----------------------------
# 5) SERVER
# -----------------------------
server <- function(input, output, session) {
  
  weights_reactive <- reactive({
    w <- sapply(criteria, function(c) input[[paste0("w_", c)]])
    names(w) <- criteria
    w
  })
  
  prefs_reactive <- reactive({
    p <- lapply(unknown_criteria, function(c) input[[paste0("p_", c)]])
    names(p) <- unknown_criteria
    p
  })
  
  scored <- reactive({
    compute_suitability(gdf, weights_reactive(), prefs_reactive())
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addTiles() |>
      setView(lng = center_lng, lat = center_lat, zoom = 10)
  })
  
  observe({
    df <- scored()
    df <- st_as_sf(df)
    
    pal <- colorNumeric(
      palette = c("#4281a4", "#9cafb7", "#ead2ac", "#e6b89c", "#fe938c"),
      domain = df$suit_score
    )
    
    # keep hover label small (prevents giant string crash)
    df$label <- sprintf(
      "<b>%s</b>: %s<br><b>Suitability</b>: %.3f",
      ID, df[[ID]], df$suit_score
    )
    
    proxy <- leafletProxy("map")
    
    proxy |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addPolygons(
        data = df,
        fillColor = ~pal(suit_score),
        fillOpacity = 0.8,
        color = "black",
        weight = 0.3,
        label = lapply(df$label, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#FFFFFF", bringToFront = TRUE)
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df$suit_score,
        title = "Suitability Score",
        opacity = 0.9
      )
    
    if (isTRUE(input$show_top10)) {
      top10 <- df |>
        arrange(desc(suit_score)) |>
        slice_head(n = 10) |>
        mutate(Rank = row_number())
      
      coords <- st_coordinates(top10$pt)
      
      proxy |>
        addLabelOnlyMarkers(
          lng = coords[, "X"],
          lat = coords[, "Y"],
          label = as.character(top10$Rank),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "black",
              "font-weight" = "700",
              "font-size" = "14px",
              "background-color" = "#ffe8c1",
              "border" = "2px solid #555",
              "border-radius" = "8px",
              "padding" = "4px 6px"
            )
          )
        )
    }
  })
  
  output$top10_table <- renderDT({
    df <- scored() |>
      arrange(desc(suit_score)) |>
      slice_head(n = 10) |>
      mutate(Rank = row_number()) |>
      select(Rank, all_of(ID), suit_score, all_of(criteria))
    
    datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatRound("suit_score", digits = 3)
  })
}

shinyApp(ui, server)





