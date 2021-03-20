 
# Sourcing the UI and  Serverfiles
source("ui.R")
source("server.R")

# Importing Libraries 
library(shiny)
library(shinydashboard)
library(rhandsontable)

# Run the application 
shinyApp(ui = ui, server = server)
