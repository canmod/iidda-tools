library(shiny)
library(DT)
library(iidda.api)
library(shinydashboard)
library(shinycssloaders)
library(purrr)
require(plyr)
library(jsonlite)

base_url <- "http://127.0.0.1:8000"

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Application title
  dashboardHeader(title = "IIDDA"),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dataset Selection",
      icon = icon("server"),
      tabName = "dataset_selection",
      selectInput(
        inputId = "data_type",
        label = "Dataset Type",
        choices = iidda.api::ops$dataset_metadata(jq_query = '[.[] | select(. != "No metadata.") | .resourceType .resourceType] | unique')[[1]],
        selected = "Communicable Disease Incidence"
      ),
      uiOutput("dataset_name"),
      submitButton(text = "Apply Changes")
    ),
    menuItem(
      "Downloads",
      icon = icon("download"),
      tabName = "downloads",
      checkboxGroupInput(
        "files_to_include",
        "Optional Files to Include",
        choices = list(
                        "Data Dictionary" = "data_dictionary", 
                        "CSV Dialect" = "csv_dialect",
                        "Metadata" = "metadata",
                        "Source Files" = "pipeline_dependencies"
                       ),
      ),
      downloadButton(
        outputId = "download_data",
        label = "Download",
        style = "color: black; margin-left: 15px; margin-bottom: 12px;"
      )
    )
  )),
  
  dashboardBody(fluidRow(
    box(
      title = "Dataset",
      width = 12,
      DT::dataTableOutput("data_table") %>% withSpinner(color = "#FDBF57"),
      style = "height: fit-content; overflow-y:scroll; overflow-x:scroll;"
    )
  ),
  tags$head(tags$style(
    HTML(
      '
                                  /* navbar */
                                  .skin-blue .main-header .navbar {
                                    background-color: #7A003C;
                                  }
                                  .skin-blue .main-header .logo {
                                    background-color: #56002a;
                                  }
                                  .skin-blue .main-header .logo:hover {
                                    background-color: #7A003C;
                                  }
                                  .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                    background-color: #56002a;
                                  }
                                  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
                                    background-color: #5E6A71;
                                  }
                                  .content-wrapper, .right-side {
                                    background-color: #efefef;
                                  }
                                  .btn-primary {
                                    margin-left: 15px; margin-bottom: 12px;
                                  }
                                  .has-feedback .form-control {
                                    padding-right: 0;
                                  }
                                  '
    )
  )))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <-
    reactive({
      req(input$dataset_name)
      rbind.fill(lapply(input$dataset_name, function(x) {iidda.api::ops$dataset(x, response_type = "raw_csv")}))
      })
  
  
  output$dataset_name = renderUI(
    selectizeInput(
      inputId = "dataset_name",
      label = "Dataset Name",
      multiple = TRUE,
      choices = iidda.api::ops$dataset_metadata(
        jq_query = sprintf(
          'map_values(select(. != "No metadata." and .resourceType .resourceType == "%s") ) | keys',
          input$data_type
        )
      )[[1]],
      selected = "cdi_ca_1957_wk_prov_dbs",
      options = list(maxOptions = 5)
      
    )
  )
  output$data_table = renderDT({
    data_dictionary <-
      iidda.api::ops$dataset(input$dataset_name, response_type = "data_dictionary")
    
    datatable(
      data(),
      filter = "top",
      rownames = F,
      callback = JS(
        sprintf(
          "
                    var tips = %s
                    var titles = %s
                    header = table.columns().header();
                    for (var i = 0; i < tips.length; i++) {
                      $(header[i]).attr('title', tips[i][0]);
                      $(header[i]).css('[title]:hover', 'background-color: red');
                      $(header[i]).text(titles[i][0])
                    }
                              ",
          toJSON(map(data_dictionary, "description")),
          toJSON(map(data_dictionary, "title"))
        )
      )
      
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function()
    {
      paste(input$dataset_name, '.csv', sep  =  '')
    },
    content = function(file)
    {
      showModal(modalDialog("Downloading files...", footer  =  NULL))
      write.csv(data(), file)
      on.exit(removeModal())
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
