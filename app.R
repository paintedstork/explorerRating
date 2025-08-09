# This is the start of the shinyapp, including view and controller files that have the ui and server
source("view.R")
source("controller.R")
shinyApp(ui = ui, server = server)
