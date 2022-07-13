library(shiny)
library(DT)
library(iidda.api)
library(shinydashboard)
library(shinycssloaders)
library(purrr)
require(plyr)
library(jsonlite)

# Define UI for application that draws a histogram
header <- dashboardHeader(title = "IIDDA")

# Sidebar with a slider input for number of bins

body <- dashboardBody(
  fluidRow(
    column(
      width = 9,
      box(
        title = "Dataset",
        width = NULL,
        DT::dataTableOutput("data_table") %>% withSpinner(color = "#FDBF57"),
        style = "height: fit-content; overflow-y:scroll; overflow-x:scroll;"
      )
    ),
    column(
      width = 3,
      box(
        h3("Dataset Selection"),
        width = NULL,
        selectInput(
          inputId = "data_type",
          label = "Dataset Type",
          choices = iidda.api::ops$metadata(response_type = "metadata", jq_query = '[.[] | select(. != "No metadata.") | .resourceType .resourceType] | unique'),
          selected = "Communicable Disease Incidence"
        ),
        uiOutput("dataset_name"),
        p(
          class = "text-muted",
          paste(
            'Changes will only apply when you click "Apply Changes"'
          )
        ),
        actionButton("select_data", "Apply Changes")
      ),
      box(
        h3("Downloads"),
        width = NULL,
        checkboxGroupInput(
          "files_to_include",
          "Optional Files to Include",
          choices = list(
            "CSV" = "csv",
            "Metadata" = "metadata",
            "Source Files" = "pipeline_dependencies"
          ),
        ),
        p(
          class = "text-muted",
          paste(
            'Selecting "Source Files" will significantly increase download time due to large file sizes.'
          )
        ),
        downloadButton(
          outputId = "download_data",
          label = "Download",
        )
      )
    )
  ),
  tags$head(tags$style(
    HTML(
      '
        .has-feedback .form-control {
          padding-right: 0;
        }
        '
    )
  )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- eventReactive(input$select_data, {
    iidda.api::ops$raw_csv(input$dataset_name)
  })
  
  output$dataset_name = renderUI(
    selectizeInput(
      inputId = "dataset_name",
      label = "Dataset Name",
      multiple = TRUE,
      choices = iidda.api::ops$metadata(
        response_type = "metadata",
        jq_query = sprintf(
          'map_values(select(. != "No metadata." and .resourceType .resourceType == "%s") ) | keys',
          input$data_type
        )
      )
    )
  )
  output$data_table = renderDT({
    data_dictionary <-
      iidda.api::ops$metadata(
        dataset_ids = input$dataset_name,
        response_type = "data_dictionary",
        jq_query = '[.[] | select(. != "No metadata.") | .[] | {(.name) : [(.title), (.description)]} ] | unique | add'
      )
    
    datatable(
      data(),
      filter = "top",
      rownames = F,
      callback = JS(
        sprintf(
          "
                    header = table.columns().header();
                    var data_dictionary = %s
                    for (var i = 0; i < header.length; i++) {
                      var raw_header = $(header[i]).text()
                      $(header[i]).attr('title', data_dictionary[raw_header][1]);
                      $(header[i]).css('[title]:hover', 'background-color: red');
                      $(header[i]).text(data_dictionary[raw_header][0])
                    }
                              ",
          toJSON(data_dictionary)
        )
      )
      
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function()
    {
      paste(paste(input$dataset_name, collapse = '-'), ".zip", sep  =  '')
    },
    content = function(file)
    {
      withProgress(message = 'Preparing files for download...', {
        writeBin(
          iidda.api::ops$download(
            dataset_ids = input$dataset_name,
            resource = input$files_to_include
          ),
          file
        )
      })
    },
    contentType = "application/zip"
  )
}

# Run the application
shinyApp(ui = ui, server = server)
