#developerOptions.R code gets included in the shinyserver code in place

#This is the UI part of developer options code
insertTab(
  inputId = "main_tabs",
  tabPanel(
    "Developer Options",
    titlePanel("Configuration Utility Example"),  # Title for the tab
    fluidRow(
      column(6, actionButton("refresh_btn", "Refresh")),
      column(6, actionButton("write_all_btn", "Write All"))
    ),
    br(),
    DTOutput("config_table")  # Replace the tableOutput with DTOutput for your editable table
  ),
  target = "About"  # Insert the new tab after Tab 2
)    


# This is the Server part of developer options code
# Render the editable config table
output$config_table <- renderDT({
  datatable(
    config_data(),
    editable = list(target = "cell", columns = c(2)), # Make the value column editable
    rownames = FALSE,
    options = list(dom = "t", paging = FALSE) # Simplified table
  )
}, server = FALSE)

# Observe Refresh button
observeEvent(input$refresh_btn, {
  config_util$refresh_from_sheet(gSheetLink, configSheet) # Refresh data from Google Sheet
  config_data(config_util$get_config_table()) # Update the reactive value
  
  # Explicitly re-render the table
  output$config_table <- renderDT({
    datatable(
      config_data(),
      editable = list(target = "cell", columns = c(2)), # Make the value column editable
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE) # Simplified table
    )
  }, server = FALSE)
  
  showNotification("Configurations refreshed from the sheet!", type = "message")
})

# Observe Write All button
observeEvent(input$write_all_btn, {
  config_util$write_to_sheet(gSheetLink, configSheet) # Write data to Google Sheet
  showNotification("All configurations written to the sheet!", type = "message")
})

# Observe cell edits and update the store
observeEvent(input$config_table_cell_edit, {
  info <- input$config_table_cell_edit
  row <- info$row
  col <- info$col
  new_value <- info$value


  # Update the store if the value column (column 2) is edited
  if (col == 1) {
    name <- config_data()$name[row]
    config_util$set_value(name, new_value)
    config_data(config_util$get_config_table()) # Update the reactive value
    log_message(paste("Config:",name,":",new_value,"updated"), 0)
  }
})
