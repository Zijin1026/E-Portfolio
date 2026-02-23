Data overview as follows:
  
  
```{r leaflet, include = FALSE}

library(sf)
library(dplyr)
library(leaflet)
library(htmltools)

# 1) Read + prep shapefile
shp <- st_read("Shiny/GVan_DA_Final.shp", quiet = TRUE) |>
  st_transform(4326) |>
  select(-any_of(c("DGUID","PRUID","Shape_Leng","Shape_Area"))) |>
  rename(
    "Land Area" = LANDAREA,
    
    "Distance to Stop" = StopDis,
    "Distance to CBD" = CBDDis,
    
    "House Maintenance Fee" = H_maintain,
    "House Rent" = H_rent,
    
    "Population Density" = popdensity,
    "Green Density" = green_dens,
    "High Education Count" = H_edu,
    
    "Age" = Age_,
    "Unemployment Rate" = Unemploy_R,
    "Median Income" = MedianInc,
    
    "Distance to Coast" = DIST_Coast,
    "Distance to School" = dis_scho,
    "Distance to Hospital/Clinic" = dis_medi,
    
    "Surface Temperature" = SurfaceT,
    "PM2.5" = PM2_5_Mean,
    "Crime Rate" = CrimeRate
  )

# 2) Popup builder (all columns, prettier)
popup_allcols <- function(x){
  att <- st_drop_geometry(x)
  
  apply(att, 1, function(row){
    tags$div(
      style="font-size:13px; line-height:1.25;",
      tags$b("Area Information"),
      tags$hr(style="margin:6px 0;"),
      tags$table(
        style="border-collapse:collapse;",
        lapply(names(row), function(nm){
          val <- row[[nm]]
          # format numbers nicely
          if (suppressWarnings(!is.na(as.numeric(val)))) {
            val <- format(round(as.numeric(val), 3), big.mark = ",", scientific = FALSE)
          } else {
            val <- as.character(val)
          }
          tags$tr(
            tags$td(style="padding:2px 10px 2px 0; font-weight:600; vertical-align:top;", nm),
            tags$td(style="padding:2px 0; vertical-align:top;", val)
          )
        })
      )
    ) |> as.character()
  })
}

shp$popup <- popup_allcols(shp)

# 3) Map (auto geometry type)
g <- unique(as.character(st_geometry_type(shp)))

m <- leaflet(shp) |>
  addTiles() |>
  addScaleBar(position = "bottomleft")

if (any(g %in% c("POLYGON","MULTIPOLYGON"))) {
  m <- m |>
    addPolygons(
      weight = 0.6,              # thinner line
      color  = "#1B3A6B",         # darker boundary
      opacity = 0.9,
      fillOpacity = 0.30,
      smoothFactor = 0.5,
      popup = ~popup,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#FFD54F",
        bringToFront = TRUE
      )
    )
} else if (any(g %in% c("LINESTRING","MULTILINESTRING"))) {
  m <- m |>
    addPolylines(
      weight = 1.2,
      color = "#1B3A6B",
      opacity = 0.9,
      popup = ~popup,
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#FFD54F",
        bringToFront = TRUE
      )
    )
} else {
  m <- m |>
    addCircleMarkers(
      radius = 4,
      stroke = TRUE,
      weight = 0.8,
      color = "#1B3A6B",
      opacity = 0.9,
      fillOpacity = 0.6,
      popup = ~popup
    )
}
```

```{r map, echo=FALSE}
m
```