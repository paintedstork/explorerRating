# TODO ExplorerRating
# EndUser: Bug: Update is called twice
# BCI: Move to BirdCountIndia shiny account before publicity
# BCI: Put code in github
# BCI: Move to new Insta screen

# To be done later
# BCI: Single district states will look odd. Remove them?
# Developer: Connect all debugging to config parameters
# EndUser: Better messaging during wait times
# Developer: Enable saving crash-logs in google document for later retrival
# Developer: Explore ways to protect configurations and logs from external attacks/misuse
# EndUser: Implement a percentile calculation per region to show relative standing of a user amongst all users
# EndUser: Some graphs on how your district coverage increases over years (how many new districts)
# EndUser: ExplorerScore fine-tuning to handle relative rarity of the species added with exploration

library(leaflet)
library(webshot2)
library(mapview)

library(shiny)
library(reactlog)
library(pryr)
library(profvis)
library(rmapshaper)
library(httr)
library(htmltools)
library(DT)
library(sf)
library(googlesheets4)
library(googledrive)
library(magick)
library(pagedown)
library(curl)
library(dplyr)
#library(chromote)
gs4_deauth()

source("configutil.R", local = TRUE)
config_util <- ConfigUtility$new()

log_message <- function(message, level) {
  if(config_util$isInitialised())
  {
    if (level <= as.integer(unlist(config_util$store[["sourceDebugLevel"]])))
    {
      timestamp <- Sys.time()
      cat(sprintf("[%s] %s\n", timestamp, message))
    }
  }
  else
  {
    timestamp <- Sys.time()
    cat(sprintf("[%s] %s\n", timestamp, message))
  }
}

refreshSoftwareLogging <- function ()
{
  if (config_util$isInitialised())
  {
    # Enable Shiny reactlog for debugging reactive dependencies base on configurations

    options(shiny.trace = as.integer(unlist(config_util$store[["shinyDebug"]])))
    options(shiny.reactlog = as.integer(unlist(config_util$store[["shinyReactDebug"]])))
    
    if(TRUE == as.integer(unlist(config_util$store[["reactDebug"]])))
    {
      reactlog::reactlog_enable()  # Start reactive logging
    }
    else
    {
      reactlog::reactlog_disable()  # Start reactive logging
    }
  }
  else
  {
    # Enable Shiny reactlog for debugging reactive dependencies at start
    options(shiny.trace = TRUE)
    options(shiny.reactlog = TRUE)
    reactlog::reactlog_enable()  # Start reactive logging
  }
}


source("helper.R", local = TRUE)
source("map.R", local = TRUE)
source("cardCreator.R", local = TRUE)

# Google Sheet link and shapefile path
shapefilePath <- "ind_dist.shp"
gSheetLink <- "https://docs.google.com/spreadsheets/d/1C_s4f6zm80wDLJAMXlHAzcvSUcSuIwdiwvbF5PbCgHE"
configSheet <- "Configurations"

req(gSheetLink)
# Load configurations from the Google Sheet during startup
config_util$refresh_from_sheet(gSheetLink, configSheet)

# Set maximum upload size for files
options(shiny.maxRequestSize = 300 * 1024^2)  # Set the limit to 30 MB




req(gSheetLink)
gs_taxonomy <- read_sheet(gSheetLink, "Taxonomy") 
log_message(paste("Google Sheets data fetched successfully with", nrow(gs_taxonomy), "rows.\n"),0)

gs_data <- read_sheet(gSheetLink, "Stats")
log_message(paste("Google Sheets data fetched successfully with", nrow(gs_data), "rows.\n"),0)

gs_statelist <- read_sheet(gSheetLink, "StateList")
log_message(paste("Google Sheets data fetched successfully with", nrow(gs_statelist), "rows.\n"), 0)

gs_statelegends <- read_sheet(gSheetLink, "StateLegends")
log_message(paste("Google Sheets data fetched successfully with", nrow(gs_statelegends), "rows.\n"), 0)

# Show a progress indicator when the shapefile is loading
req(file.exists(shapefilePath))
shapefile_data <- st_read(shapefilePath) 
log_message(paste("Shapefile loaded successfully with", nrow(shapefile_data), "rows.\n"), 0)

source("token.R")

# Define server logic
server <- function(input, output, session) {
  log_message("Starting Server", 0)
  
#  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
#    
#    # Specify the new headless mode
#    chrome <- ChromoteSession$new(
#      args = c("--headless=new", "--disable-gpu", "--no-sandbox")
#    )    
#  }
  
  # Reactive values to store processed data
  data <- reactiveValues(state_list = "India", 
                         eBird_data = NULL, 
                         species_data = NULL, 
                         filtered_species_data = NULL, 
                         filtered_shapefile_data = NULL, 
                         explorer_name = "", 
                         explorer_score = 0, 
                         district_count = 0, 
                         district_total_count = 0, 
                         explore_district_summary = NULL, 
                         target_district_summary = NULL, 
                         explore_map_data = NULL,
                         target_map_data = NULL,
                         posterTable = NULL,
                         explore_map = NULL,
                         explore_map4Save = NULL,
                         target_map = NULL,
                         refreshUI = TRUE)
  
  
  # Reactive value to store configuration data
  config_data <- reactiveVal(data.frame(name = character(), value = character(), stringsAsFactors = FALSE))
  
  config_data(config_util$get_config_table())
  log_message("Config Data Populated", 0)
  
  developerMode <- as.integer(unlist(config_util$store[["developerMode"]]))
  if (developerMode == 1)
  { # Code that builds the developer panel
    log_message("Entering Developer Mode", 0)
    source("developerOptions.R", local = TRUE)
  }
  
  # Trigger processing when the eBird ZIP file is uploaded
  observeEvent(input$ebird_zip, {
    req(input$ebird_zip, gs_statelist, gs_data, gs_taxonomy)
    
    data$updateFlag <- FALSE
    refreshSoftwareLogging()
    withProgress(message = "Extracting eBird data...", value = 0, {
      data$eBird_data <- processEbirdFile (input$ebird_zip$datapath, gs_taxonomy)
      
      # Populate dropdown with state options
      # Remove lists with empty county as this is not useful for scores.
      data$eBird_data = data$eBird_data %>% 
                            filter(County != "")  %>%
                            inner_join (gs_statelist, by = c("State.Province" = "State Code")) 
      data$state_list <- unique(c("India", sort(data$eBird_data$State)))
      
      isolate({
      
        updateSelectInput(session, "state_filter", choices = data$state_list, selected = "India")
      
        data$species_data <- data$eBird_data %>% 
                                group_by(County, State) %>%
                                  summarize(species_count = n_distinct(Scientific.Name), .groups = "drop") %>% 
                                    as.data.frame()
        log_message(paste("eBird data processed with", nrow(data$species_data), "rows.\n"),0)
        
        req(data$species_data, gs_data)
        # Merge species data with geographical data and calculate district scores
        data$species_data <- inner_join(data$species_data, gs_data, by = c("County" = "County", "State" = "State")) %>% 
                                mutate(Score = species_count / `Total Species`) %>% 
                                  select (County, State, Score) %>%
                                    group_by (State) %>%
                                      mutate(StateScore = round(10000 * sum(Score) / shapefile_data %>% filter (STATE_N == "Karnataka") %>% nrow(), 5)) %>%
                                        as.data.frame() %>% 
                                          select (1:4)
        
        colnames(data$species_data) <- c("District", "State", "Score", "StateScore")
  
        data$explorer_name <- getExplorerName (data$eBird_data$Submission.ID[1], eBirdAPIToken)
        log_message(paste("Explorer: ",data$explorer_name), 2)
        incProgress(1)
        
        data$refreshUI <- TRUE
        update_state_view(input$state_filter)  
        
        # Read the poster table and keep it ready
        data$posterTable <- read_sheet(unlist(config_util$store[["birdPosterSheet"]]), "Posters") 
        log_message(paste("Google Sheets posters fetched successfully with", nrow(data$posterTable), "rows.\n"),0)
      })
    })
  })

  # React to state selection
  observeEvent(input$state_filter, {
    if(data$refreshUI)
    {
      req(data$species_data)
      refreshSoftwareLogging()
      update_state_view(input$state_filter)
    }
  })
  

  # Calculate and render outputs
  calculate_and_render <- function(filtered_data, shapefile, state) {
    data$explorer_score       <- round (10000 * sum (filtered_data$Score) / nrow (shapefile), 0)
    data$district_count       <- nrow(filtered_data)
    data$district_total_count <- nrow(shapefile)
    data$explore_district_summary   <- generateExploreDistrictSummary(filtered_data, unlist(config_util$store[["NoOfDistrictsShown"]]))
    
    targetDistricts                 <- generateTargetDistricts (filtered_data, shapefile)
    
    maxDistrictstoShow <- ifelse (state == "India", as.integer(unlist(config_util$store[["maxDistrictsfromStateinTarget"]])), nrow(shapefile))
    data$target_district_summary    <- generateTargetDistrictSummary(targetDistricts, 
                                                                      as.integer(unlist(config_util$store[["NoOfDistrictsShown"]])), 
                                                                      as.numeric(unlist(config_util$store[["thresholdScoreforTarget"]])), 
                                                                      maxDistrictstoShow)
    
    
    data$explore_map_data           <- prepareExploreMapData(filtered_data, shapefile)
    data$target_map_data            <- prepareTargetMapData(data$target_district_summary, shapefile)
    
    data$explore_map                <- createMap(data$explore_map_data, 
                                                  unlist(config_util$store[["exploreMapLegendTitle"]]),
                                                  unlist(config_util$store[["mapBoundaryWeight"]]),
                                                  unlist(config_util$store[["mapLineColor"]]),
                                                  unlist(config_util$store[["mapfillOpacity"]]),
                                                  ifelse (state == "Odisha"  | (state == "Assam"), "bottomright", ifelse (state == "Arunachal Pradesh", "topleft", unlist(config_util$store[["mapLegendPosition"]]))),
                                                  unlist(config_util$store[["mapBaseColor"]]),
                                                  unlist(config_util$store[["mapNAColor"]]))
    

    legendPosition <- gs_statelegends %>% filter (State == state) %>% select (LegendPosition) %>% pull()
    
    data$explore_map4Save            <- createMap4Save(data$explore_map_data, 
                                                 unlist(config_util$store[["exploreMapLegendTitle"]]),
                                                 unlist(config_util$store[["mapBoundaryWeight"]]),
                                                 unlist(config_util$store[["mapLineColor"]]),
                                                 unlist(config_util$store[["mapfillOpacity"]]),
                                                 legendPosition,
                                                 unlist(config_util$store[["mapBaseColor"]]),
                                                 unlist(config_util$store[["mapNAColor"]]))
    
    data$target_map                 <- createMap(data$target_map_data, 
                                                  unlist(config_util$store[["targetMapLegendTitle"]]),
                                                  unlist(config_util$store[["mapBoundaryWeight"]]),
                                                  unlist(config_util$store[["mapLineColor"]]),
                                                  unlist(config_util$store[["mapfillOpacity"]]),
                                                  unlist(config_util$store[["mapLegendPosition"]]),
                                                  unlist(config_util$store[["mapBaseColor"]]),
                                                  unlist(config_util$store[["mapNAColor"]]))

    output$score_panel_explorer_name_heading   <- renderUI(h3(paste("Name:", data$explorer_name)))
    output$target_panel_explorer_name_heading  <- renderUI(h3(paste("Name:", data$explorer_name)))
    
    output$score_panel_explorer_score_heading  <- renderUI(h3(paste("Explorer Score:", data$explorer_score)))
    output$target_panel_explorer_score_heading <- renderUI(h3(paste("Explorer Score:", data$explorer_score)))
    
    output$score_panel_district_count_heading  <- renderUI(h3(paste("District Count:", data$district_count, "(", data$district_total_count ,")")))
    output$target_panel_district_count_heading <- renderUI(h3(paste("District Count:", data$district_count, "(", data$district_total_count ,")")))
    
    output$score_panel_district_summary_heading  <- renderUI({h4("Top Districts")})
    output$target_panel_district_summary_heading <- renderUI({h4("Suggested Districts")})
    
    output$score_panel_district_summary     <- renderTable({data$explore_district_summary})
    output$target_panel_district_summary    <- renderTable({data$target_district_summary})
    
    output$score_panel_map                  <- renderLeaflet({data$explore_map})
    output$target_panel_map                 <- renderLeaflet({data$target_map})
  }
  
  
  update_state_view <- function(state) {
    log_message(state, 0)
    write.csv2(data$species_data, "test.csv")
    if (state == "India") {
      data$filtered_data            <- data$species_data
      data$filtered_shapefile_data  <- shapefile_data
    } else {
      data$filtered_data            <- data$species_data %>% filter(State == state)
      # Filter shapefile data based on the state code
      data$filtered_shapefile_data <- shapefile_data %>% filter (STATE_N == state)

      
      data$filtered_shapefile_data  <- shapefile_data %>% filter (STATE_N == state)
      log_message(nrow(data$filtered_data), 1)
      log_message(nrow(data$filtered_shapefile_data), 1)
    }
    if(nrow(data$filtered_data) > 0)
    {
      calculate_and_render(data$filtered_data, data$filtered_shapefile_data, state)
    }
  }
  
  # Helper function to generate explorer card
  generate_card <- function(file, filetype) {
    withProgress(message = "Generating Explorer Card...", value = 0, {
      
      incProgress(0.1, detail = "Preparing data...")
      log_message("Start of generate_card", 3)
      req(data$explorer_name, data$explorer_score, data$district_count,  
          data$district_total_count, data$explore_map4Save, 
          data$explore_district_summary, data$posterTable)
      
      bird_info <- getBirdPosterIndex(data$posterTable, data$explorer_name, 
                                      data$explorer_score, input$state_filter)
      
      incProgress(0.3, detail = "Downloading bird image...")
      log_message(paste("FileId", bird_info$FileId), 3)
      bird_data <- tryCatch({
        log_message("Downloading Image", 0)
        list(
          bird_image = download_and_crop_image(bird_info$FileId, 300, 212),
          bird_text = bird_info$Quote,
          photographer = bird_info$Photographer
        )
      }, error = function(e) {
        log_message(paste("Image not obtained, taking default image:", e$message, "\n"), 0)
        list(
          bird_image = "redmunia.png",
          bird_text = "You are like a Red Munia, wandering locally in search of new pastures to find new birds.",
          photographer = "Saswat Mishra"
        )
      })
      
      incProgress(0.6, detail = "Rendering card layout...")
      log_message("Generating Explorer Card", 0)
      generate_explorer_card(
        output_file = file,
        explorer_name = toupper(data$explorer_name),
        explorer_score = data$explorer_score,
        map_image = data$explore_map4Save,
        bird_image = bird_data$bird_image,
        bird_text = bird_data$bird_text,
        region = toupper(input$state_filter),
        num_districts = data$district_count,
        total_districts = data$district_total_count,
        date = format(Sys.Date(), "%d/%m/%Y"),
        photographer = bird_data$photographer,
        template_file = "template.png",
        cssfile = "bcicardstyle.css",
        filetype = filetype
      )
      
      incProgress(1, detail = "Done!")
    })
  }

  # Common filename generator
  generate_filename <- function(filetype) {
    req(data$explorer_name)
    paste0("EC_", data$explorer_name, "_", input$state_filter, "_", 
           Sys.Date(), "_", Sys.time(), ".", filetype)
  }
  
  # Download handlers
  output$download_html <- downloadHandler(
    filename = function() generate_filename("html"),
    content = function(file) generate_card(file, "html")
  )
  
  output$download_card <- downloadHandler(
    filename = function() generate_filename("jpg"),  # Change to "jpg" if needed
    content = function(file) generate_card(file, "jpg")
  )
}


