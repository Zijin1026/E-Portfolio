library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(DT)
library(terra)

#load data
df <- st_read("datasummary5.shp")
df <- st_transform(df, 4326)
df <- st_make_valid(df)


slope <- rast("Slope.tif")

radpot <- rast("radpot.tif")

twi <- rast("TWI.tif")



# set up
pts <- st_centroid(df)




ui <- fluidPage(titlePanel("Displaying Options"),
                sidebarLayout(sidebarPanel(radioButtons("base", 
                                                        "Basemap raster", 
                                                        choices = c("Slope" = "Slope", "Radpot" = "Radpot", "TWI" = "TWI"), 
                                                        selected = "Slope"),
                                           radioButtons("err", 
                                                        "Error layer",
                                                        choices = c("CanopyDensityError" = "CDN",
                                                                    "HeightError" = "HE",
                                                                    "VolumeError" = "VL"),
                                                        selected = "CanopyDensityError"),),
                              mainPanel(leafletOutput("map", height = 650))))



server <- function(input, output, session){
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
      addRasterImage(slope,  group = "Slope",  project = FALSE) %>%
      addRasterImage(radpot, group = "Radpot",  project = FALSE) %>%
      addRasterImage(twi,    group = "TWI",  project = FALSE)
  })
  
  # show only selected raster
  observeEvent(input$base, {
    leafletProxy("map") %>%
      hideGroup(c("Slope","Radpot","TWI")) %>%
      showGroup(input$base)
  }, ignoreInit = FALSE)
  
  # redraw proportional dots based on selected error variable
  observeEvent(input$err, {
    
    # choose the column
    col <- switch(input$err,
                  "CDN" = "dff_CDn",
                  "HE"  = "diff_he",
                  "VL"  = "diff_vl")
    
    vals <- pts[[col]]
    
    # scale circle radius (sqrt makes area proportional)
    max_radius <- 18
    sfac <- max_radius / sqrt(max(abs(vals), na.rm = TRUE))
    
    proxy <- leafletProxy("map") %>%
      clearGroup("dots") %>%
      clearControls()
    
    proxy %>%
      addCircleMarkers(
        data = pts,
        group = "dots",
        radius = ~sqrt(abs(get(col))) * sfac,
        color = "black",
        weight = 0.4,
        fillColor = ~ifelse(get(col) < 0, "blue", "red"),
        fillOpacity = 0.65,
        stroke = TRUE,
        popup = ~paste0(col, ": ", round(get(col), 2))
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue","red"),
        labels = c("Underestimation (negative)", "Overestimation (positive)"),
        title  = "Sign"
      ) %>%
      addControl(
        html = "<div style='background: rgba(255,255,255,0.85); padding:6px 8px; border-radius:6px;'>
                  <b>Size</b> ∝ |error|
                </div>",
        position = "bottomright"
      )
    
  }, ignoreInit = FALSE)
  
}

shinyApp(ui, server)
  
  
  
  
  
  
  
  
  
  
