# Load necessary libraries
library(googlesheets4)
library(shiny)

gs4_deauth()

# Create a Configuration Utility
ConfigUtility <- R6::R6Class(
  "ConfigUtility",
  public = list(
    # Store for the name-value pairs
    store = NULL,
    init = FALSE,
    
    # Constructor to initialize the store
    initialize = function(initial_names = character(), initial_values = character()) {
      if (length(initial_names) != length(initial_values)) {
        stop("Initial names and values must have the same length.")
      }
      self$store <- setNames(as.list(initial_values), initial_names)
    },
    
    # Function to set a value in the store
    set_value = function(name, new_value) {
      if (name %in% names(self$store)) {
        self$store[[name]] <- new_value
      } else {
        stop(paste("Name", name, "not found in the store."))
      }
    },
    
    # Function to read from Google Sheets and update the store
    read_from_sheet = function(sheet_url, sheet_name) {
      tryCatch({
        data <- read_sheet(sheet_url, sheet = sheet_name)
        if (!all(c("name", "value") %in% colnames(data))) {
          stop("The Google Sheet must have 'name' and 'value' columns.")
        }
        for (i in seq_len(nrow(data))) {
          self$store[[data$name[i]]] <- data$value[i]
        }
        self$init = TRUE
      }, error = function(e) {
        stop("Error reading Google Sheet: ", e$message)
      })
    },
    
    isInitialised = function () { return (self$init)},
    
    # Function to return the entire store as a data frame for a Shiny renderer
    get_config_table = function() {
      data.frame(
        name = names(self$store),
        value = unlist(self$store, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    },
    
    # Function to refresh the store from the Google Sheet
    refresh_from_sheet = function(sheet_url, sheet_name) {
      self$read_from_sheet(sheet_url, sheet_name)
    },
    
    # Function to write the store back to a Google Sheet
    write_to_sheet = function(sheet_url, sheet_name) {
      tryCatch({
        config_data <- self$get_config_table()
        sheet_write(config_data, sheet_url, sheet = sheet_name)
      }, error = function(e) {
        stop("Error writing to Google Sheet: ", e$message)
      })
    }
  )
)

