library(leaflet)
library(dplyr)
library(stringr)
library(htmlwidgets)
library(rmarkdown)
library(ggplot2)
library(sf)
library(RColorBrewer)

legendCoordinates1 <- list(
  bottomleft = c(0.1, 0.1),   # Bottom-left corner
  bottomright = c(0.9, 0.1),  # Bottom-right corner
  topleft = c(0.1, 0.9),      # Top-left corner
  topright = c(0.9, 0.9)      # Top-right corner
)

legendCoordinates <- list(
  bottomleft = list(position = c(0.1, 0.1), justification = c(0, 0)),
  bottomright = list(position = c(0.9, 0.1), justification = c(1, 0)),
  topleft = list(position = c(0.1, 0.9), justification = c(0, 1)),
  topright = list(position = c(0.9, 0.9), justification = c(1, 1))
)


#######################PREPARE AND RENDER MAP########################################

# Join the explorer's species seen data with shapefile to create a renderable object
prepareExploreMapData <- function(filteredspeciesdata, shapefile) {
#  write.csv(filteredspeciesdata, "test.csv")
  shapefile %>%
    left_join(filteredspeciesdata %>% select(District, Score) , by = c("COUNTY" = "District")) %>%
    mutate(Score = ifelse(is.na(Score), 0, Score)) 
}

# Join the explorer's species seen data with shapefile to create a renderable object
prepareTargetMapData <- function(filteredspeciesdata, shapefile) {
  shapefile %>%
    left_join(filteredspeciesdata %>% select(District, Score), by = c("COUNTY" = "District")) %>%
    mutate(Score = ifelse(is.na(Score), 0, Score)) 
}

createMap4Save <- function(map_data, legendTitle, lineWeight, lineColor, fillOpacity, legendPosition, colorPalette, naColor) {

  
  # Extract color palette as a vector
  colorPalette <- brewer.pal(9, colorPalette)  # Extract colors from "Purples" or other palettes

  # Ensure Score column exists and replace NA values for proper mapping
  map_data <- map_data %>%
    mutate(Score = ifelse(0==Score, NA, Score))  # Ensure NA values are handled correctly
  
  if (!is.null(map_data) && nrow(map_data) > 0) {
    ggplot(map_data) +
      geom_sf(aes(fill = Score), color = lineColor, size = as.numeric(lineWeight), alpha = as.numeric(fillOpacity)) +
      scale_fill_gradientn(colors = colorPalette, na.value = naColor, labels = function(x) formatC(x, format = "f", digits = 2)) +
      labs(fill = legendTitle)+
      theme_minimal() +
      coord_sf(datum = NA) +
      theme(
        panel.background = element_rect(fill = "lightgrey", color = NA),  # Light grey background
        plot.background = element_rect(fill = "lightgrey", color = NA),   # Ensure full plot bg is grey
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.ticks = element_blank(), #element_line(color = "black"),  # Show axis ticks
        axis.text = element_blank(), #element_text(size = 10),  # Ensure axis text is visible
        legend.position = "bottom",  # Moves legend outside the plot
        legend.direction = "horizontal",  # Makes legend horizontal
        legend.box = "horizontal",  # Ensures it stays outside the plot
        legend.background = element_rect(fill = "white", color = "black")  # Add a border to legend
      )
  } else {
    message("Warning: map_data is empty or invalid.")
  }
}


createMap <- function(map_data, legendTitle, lineWeight, lineColor, fillOpacity, legendPosition, colorPalette, naColor) {
  
  log_message(paste("Generating color scale for map using...",colorPalette,"\n"), 2)
  
  if (!is.null(map_data) && nrow(map_data) > 0) {
    bounds <- st_bbox(map_data)  # Get bounding box as xmin, ymin, xmax, ymax
    lng_min <- as.numeric(bounds["xmin"])
    lat_min <- as.numeric(bounds["ymin"])
    lng_max <- as.numeric(bounds["xmax"])
    lat_max <- as.numeric(bounds["ymax"])
  } else {
    log_message("Warning: map_data is empty or invalid. Using default bounds.", 1)
    lng_min <- -180  # Default longitude min (global view)
    lat_min <- -90   # Default latitude min (global view)
    lng_max <- 180   # Default longitude max (global view)
    lat_max <- 90    # Default latitude max (global view)
  }
  
  #  bins <- getNoOfColorBins (map_data$Score)
  
  # Create the color scale with the calculated bins
  #  color_scale <- colorBin(colorPalette, domain = map_data$Score, bins = bins, na.color = naColor)
  
  # Verify the bins inside the color scale
  #  log_message(attr(color_scale, "colorArgs")$bins, 0)
  
  color_scale <- colorNumeric(colorPalette, map_data$Score, na.color = naColor)
  leaflet(map_data, width = "656px", height = "528px") %>%
    addPolygons(fillColor = ~color_scale(Score),               # Fill color based on score
                weight = as.numeric(lineWeight),                           # Thin boundary line
                color = lineColor,                             # Boundary line color
                fillOpacity = as.numeric(fillOpacity),         # Transparency of fill
                popup = ~paste(COUNTY, ": ", round(Score, 2))  # Popup information
    ) %>% 
    addLegend(legendPosition, pal = color_scale, values = ~Score, title = legendTitle) %>%  # Adjust vertical position by 30px) %>%
    fitBounds(lng1 = lng_min, lat1 = lat_min, lng2 = lng_max, lat2 = lat_max) 
}

testMaps <- function(state, testdata, shapefile_data)
{
#  state = "Arunachal Pradesh"
#  testdata <- read.csv(".\\test\\test.csv", sep=",")
#  shapefile_data <- st_read("ind_dist.shp")
  if (state != "India")
  {
    testdata <- testdata %>% filter (State == state)
    shapefile_data <- shapefile_data %>% filter (STATE == state)
  }
  
  legendPosition <- read.csv(".\\test\\StateLegend.csv") %>% 
    filter (State == state) %>% 
    select(LegendPosition) %>% 
    pull()
  
  map_data <- prepareExploreMapData(testdata, shapefile_data)
  map <- createMap4Save(map_data, 
                        "ExplorerScore",
                        2.0,
                        "darkgray",
                        0.7,
                        legendPosition,
#                        ifelse ((state == "Odisha") | (state == "Assam"), "bottomright", ifelse (state == "Arunachal Pradesh", "topleft", "bottomleft")),
                        "Purples",
                        "white")
  
  png_file <- paste0("map", state, ".png")
  ggsave(png_file, map)
  #  mapshot(map, file = paste0("map",state,".png"), vwidth = 656, vheight = 528, zoom = 1)
}

looptest <- function()
{
  shapefile_data <- st_read("ind_dist.shp")
  testdata <- read.csv(".\\test\\test.csv", sep=";", dec=",", stringsAsFactors = FALSE)
  for (state in unique(shapefile_data$STATE)) {
    print (state)
    if (testdata %>% filter (State == state) %>% nrow() >0)
    {
      testMaps(state, testdata, shapefile_data)
    }
  }
  testMaps("India",testdata, shapefile_data)
}
