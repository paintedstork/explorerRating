library(dplyr)
library(stringr)


############################PROCESS EBIRD DATA#####################################
processEbirdFile <- function(filepath, gs_taxonomy) {
  zipfile <- filepath
  eBird_data <- read.csv(unz(zipfile, "MyEBirdData.csv"))
  eBird_data %>%
    filter(str_starts(`State.Province`, "IN-")) %>%
      inner_join (gs_taxonomy %>% filter (is.na(Exotic)), c('Scientific.Name' = 'Scientific Name') )
}

############################GET EXPLORER NAME#####################################
getExplorerName <- function (checklist_id, ebird_api_token)
{
  # Call the eBird API to get the observer's display name
  ebird_api_url <- paste0("https://api.ebird.org/v2/product/checklist/view/", checklist_id)
  
  response <- tryCatch({
              httr::GET(ebird_api_url, httr::add_headers("X-eBirdApiToken" = ebird_api_token))
  }, error = function(e) {
    log_message(paste("Error calling eBird API:", e$message, "\n"), 0)
    return(NULL)
  })
  
  # Parse the API response and extract the userDisplayName
  if (!is.null(response)) {
    api_data <- httr::content(response, "parsed")
    user_display_name <- api_data$userDisplayName
    log_message(paste("User display name fetched:", user_display_name, "\n"), 1)
  } else {
    log_message("Error: Unable to fetch user display name.\n", 0)
  }
  return (user_display_name)
}

############################DISTRICT SUMMARY########################################
generateExploreDistrictSummary <- function(filteredspeciesdata, noOfDistrictsShown) {
  filteredspeciesdata %>%
    select(District, Score) %>%  
      arrange(desc(Score)) %>%
        head(as.integer(noOfDistrictsShown)) %>% 
          select (District, Score)
}

############################DISTRICTS AS TARGETS ########################################

generateTargetDistricts <- function(filteredspeciesdata, shapefile) {
  filteredspeciesdata <- shapefile %>%
    # Get all districts and states
    as.data.frame() %>%
    rename(State = STATE_N, District = COUNTY) %>%
    select(State, District) %>%
    
    # Identify missing districts and states
    mutate(
      StateScoreTemp = ifelse(State %in% filteredspeciesdata$State, NA, 0),
      Score = 0
    ) %>%
    
    # Include districts missing from filtered data
    anti_join(filteredspeciesdata, by = c("District"="District")) %>%
    
    # Add StateScores for states with existing districts
    left_join(
      filteredspeciesdata %>%
        select(State, StateScore) %>%
        distinct(),
      by = c("State"="State")
    )  %>%
    
    # Handle missing StateScores
    mutate(StateScore = ifelse(is.na(StateScoreTemp), StateScore, StateScoreTemp)) %>%
    select (District, State, Score, StateScore) %>%
    
    # Add missing districts to original data
    bind_rows(filteredspeciesdata) %>%
    
    # Update scores
    mutate(Score = 1 - Score)
    #print(head(filteredspeciesdata,20))
    #print(tail(filteredspeciesdata,20))
    return(filteredspeciesdata)
}

  
############################DISTRICT SUMMARY########################################

generateTargetDistrictSummary <- function(targetDistricts, noOfDistrictsShown, thresholdScore, maxDistrictsinState) {
  # Select districts with score less than thresholdscore for targets
  # Group by state and only select the top few
  # Arrange them in descending order of state score and then ascending order of district score
  # Essentially unvisited districts in most visited states should be suggested first
  targetDistricts %>%
    filter (Score > 1 - thresholdScore) %>%
      group_by(State) %>%
        slice_head (n = maxDistrictsinState) %>%
          ungroup () %>%
            arrange(desc(StateScore), desc(Score)) %>%
              head(noOfDistrictsShown) %>% 
                select (District, State, Score)
}

