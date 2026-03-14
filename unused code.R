{r leaflet_4, include = FALSE}
library(scales);library(terra)
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("GDAL_DATA")
radius <- function(x){
  scaled <- rescale(x, to = c(4,25))
} 
vec <- st_make_valid(vec)
centroid <- st_centroid(vec)

slope <- rast("images/geo_layers/Slope.tif")
radpot <- rast("images/geo_layers/radpot.tif")
twi <- rast("images/geo_layers/TWI.tif")



dotmap <- leaflet(centroid)%>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(group = "CanopyDensityError",
                   radius = ~radius(dff_CDn),
                   fillColor =  ~pal2(dff_CDn),
                   stroke = TRUE,
                   color = "black", 
                   weight = 0.25,
                   fillOpacity = 1,
                   popup = ~paste0("CanopyDensityError: ", dff_CDn))%>%
  addCircleMarkers(group = "HeightError",
                   radius = ~radius(diff_he),
                   fillColor =  ~pal3(diff_he),
                   stroke = TRUE,
                   fillOpacity = 1,
                   color = "black", 
                   weight = 0.25,
                   popup = ~paste0("CanopyDensityError: ", diff_he)) %>%
  addCircleMarkers(group = "VolumeError",
                   radius = ~radius(diff_vl),
                   fillColor =  ~pal4(diff_vl),
                   stroke = TRUE,
                   fillOpacity = 1,
                   color = "black", 
                   weight = 0.25,
                   popup = ~paste0("VolumeError: ", diff_vl)) %>%
  addRasterImage(slope, 
                 group = "slope") %>%
  addRasterImage(radpot, 
                 group = "radpot") %>%
  addRasterImage(twi, 
                 group = "twi") %>%
  addLayersControl(overlayGroups = c("CanopyDensityError","HeightError","VolumeError"),
                   baseGroups = c("slope","radpot","twi"),
                   options = layersControlOptions(collapsed = TRUE))

)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Code Snippets
  
  Sample code snippet. Notice that you can provide a toggle to switch between coding languages - this is referred to as a 'tabset' in quarto. It is good practice to try and convert your R code to python, and vice-versa to demonstrate coding proficiency. For example, let's showcase a function for calculating NDVI in R and Python.

R

calc_ndvi <- function(nir, red){
  ndvi <- (nir-red)/(nir+red)
  return(ndvi)
}

(Assuming nir and red are terra rast objects)

Python

def calc_ndvi(nir, red): 
  ndvi = (nir - red)/(nir + red)
  return(ndvi)

(Assuming nir and red are numpy arrays)

External links

You can also provide a frame linking to external websites. For example, here is a Google Earth Engine application I developed - which I embedded in this webpage using the code below:

{code}

<iframe width="900" height="700"
src="https://ee-melserramon.projects.earthengine.app/view/thermal-analysis-tool">
</iframe>

The full-screen GEE application is available here in case you're interested.
  
  (To use the GEE tool, navigate to any city you'd like, hit apply filters, and click anywhere on the map to retrieve a time-series of Landsat surface temperature observations for that point. Areas where the maximum temp exceeded 35 degrees Celsius in your date-range are highlighted in red.)

<iframe width="900" height="700" src="https://ee-melserramon.projects.earthengine.app/view/thermal-analysis-tool"></iframe>


  
  
  
  
  
---
title: "Zijin(Sarah) Xiao"
image: images/me.jpg
about:
  template: trestles
  image-width: 20em
  links: 
    - icon: linkedin
      text: LinkedIn
      href: www.linkedin.com/in/sarah-xiao-4a40b72ba
    - icon: github
      text: GitHub
      href: https://github.com/Zijin1026
    - icon: envelope-at
      text: Email
      href: xiaozijin2024@gmail.com
format:
  html:
    theme: superhero
    css: styles.css
---


<h2>Hi there people,</h2>

I am a new Master graduate from UBC Master of Geomatics for Environmental Management (MGEM) program and ready to participate and contribute to my future workplace!!!!
With past 5 years of study in Geography and GIS_related field, I am confident to show I have gotten strong analytical and critical thinking skills, specializing in data visualization and cartographic design. Proficient in a wide range of geospatial software, office tools and picture editing platforms, including ESRI, ArcGIS Pro, QGIS, Microsoft Office Suite, Adobe Illustrator, etc. Proficient in coding with Python and R for geospatial data analysis, automation, and statistical modeling. Demonstrated strengths in multitasking, time management, and team collaboration, with a proven ability to work efficiently in project-oriented environments.


Over the past five years of my academic career, I have participated in numerous projects related to remote sensing, programming, and GIS. You can explore the "Map Gallery"/Project Conetent" section for further details.


## Projects
  
  
  
  
  
  