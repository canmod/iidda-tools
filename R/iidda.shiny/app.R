library(shiny)

source('ui.R', local = TRUE)
source('server.R')

# Define server logic required to draw a histogram

# Run the application
shinyApp(ui = ui, server = server)