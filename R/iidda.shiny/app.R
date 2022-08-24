<<<<<<< HEAD
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

        .sorting {
        white-space: nowrap;
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
        dataset_ids = isolate(input$dataset_name),
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
=======
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

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Dataset Selection",
    tabName = "dataset_selection",
    icon = icon("th")
  ),
  menuItem(
    "Data Filtering",
    icon = icon("filter"),
    tabName = "data_filtering",
    badgeLabel = "new",
    badgeColor = "green"
  )
))

body <- dashboardBody(tabItems(tabItem(
  tabName = "dataset_selection",
  h2("Dataset Selection"),
  fluidRow(column(
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
        paste('Changes will only apply when you click "Apply Changes"')
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
      downloadButton(outputId = "download_data",
                     label = "Download", )
    )
  ))
),
tabItem(
  tabName = "data_filtering",
  h2("Data Filtering"),
  fluidRow(column(
    width = 12,
    box(
      title = "Filtered Data",
      width = NULL,
      DT::dataTableOutput("data_filter_table") %>% withSpinner(color = "#FDBF57"),
      style = "height: fit-content; overflow-y:scroll; overflow-x:scroll;"
    ),
    box(
      h3("Dataset Filtering"),
      width = NULL,
      p(
        class = "text-muted",
        paste('Changes will only apply when you click "Apply Changes"')
      ),
      actionButton("filter_data", "Apply Changes"),
      selectInput(
        inputId = "data_filter_type",
        label = "Dataset Type",
        choices = iidda.api::ops$metadata(response_type = "metadata", jq_query = '[.[] | select(. != "No metadata.") | .resourceType .resourceType] | unique'),
        selected = "Communicable Disease Incidence"
      ),
      uiOutput("column_filters"),
    ),
  ))
  ))
,
tags$head(tags$style(
  HTML(
    '
        .has-feedback .form-control {
          padding-right: 0;
        }

        .sorting {
        white-space: nowrap;
        }
        '
  )
)))

ui <- dashboardPage(header,
                    sidebar,
                    body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- eventReactive(input$select_data, {
    response <- iidda.api::ops$raw_csv(input$dataset_name)
    if(is.data.frame(response)) {
      response
    } else {
      data.frame(response)
    }
  })
  
  datasets <- reactive(iidda.api::ops$metadata(
    response_type = "metadata",
    jq_query = sprintf(
      'map_values(select(. != "No metadata." and .resourceType .resourceType == "%s") ) | keys',
      input$data_filter_type
    )
  ))
  
  data_dictionary <- reactive(
    iidda.api::ops$metadata(response_type = "data_dictionary", dataset_ids = datasets()) %>%   unlist(recursive = FALSE) %>%
      unname %>%
      unique %>%
      na.omit
  )
  
  filtered_data <- eventReactive(input$filter_data, {
    filter_params <- lapply(data_dictionary(), function(x) {
      named_list <- list()
      if (x$format == "num_missing") {
        if (input[[sprintf("%s_checkbox", x$name)]]) {
          named_list[[x$name]] = c(paste(input[[sprintf("%s_range", x$name)]], collapse = '-'), input[[sprintf("%s_unavailable_values", x$name)]])
        } else if (input[[sprintf("%s_checkbox", x$name)]] == FALSE &&
                   is.null(input[[sprintf("%s_unavailable_values", x$name)]]) == FALSE) {
          named_list[[x$name]] = c('none', input[[sprintf("%s_unavailable_values", x$name)]])
        } else {
          NULL
        }
      } else if (x$type == "date") {
        if (anyNA(input[[x$name]])) {
          NULL
        } else {
          named_list[[x$name]] = paste(input[[x$name]], collapse = '/')
        }
      } else{
        named_list[[x$name]] = input[[x$name]]
      }
      named_list
    }) %>% unlist(recursive = FALSE)
    response <- do.call(iidda.api::ops$filter, c(list(resource_type = input$data_filter_type), filter_params))
    if(is.data.frame(response)) {
      response
    } else {
      data.frame(response)
    }
  })
  
  output$dataset_name = renderUI(
    selectizeInput(
      inputId = "dataset_name",
      label = "Dataset Name",
      multiple = TRUE,
      choices = datasets()
    )
  )
  output$column_filters = renderUI({
    columns <-
      iidda.api::ops$metadata(response_type = "columns", dataset_ids = datasets())
    lapply(data_dictionary(), function(x) {
      if (x$type == "string" && x$format == "default") {
        tags$div(title=x$description,
        selectizeInput(
          inputId = x$name,
          label = x$title,
          multiple = TRUE,
          choices = columns %>%
            lapply(function(z) {
              z[[x$name]]
            }) %>%
            unlist(recursive = FALSE) %>%
            unname() %>%
            unique()
        )
        )
      } else if (x$type == "date") {
        tags$div(title=x$description,
        dateRangeInput(
          inputId = x$name,
          label = x$title,
          start = NA,
          end = NA,
          min = NULL,
          
          max = NULL,
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "en",
          separator = " to ",
          autoclose = TRUE,
        )
        )
      } else if (x$format == "num_missing") {
        range_of_values = columns %>%
          lapply(function(z) {
            z[[x$name]][['range']]
          }) %>%
          unlist(recursive = FALSE) %>%
          unname %>%
          as.integer() %>%
          na.omit %>%
          range
        
        tags$div(title=x$description,
        tabBox(
          title = x$title,
          tabPanel(
            "Range",
            checkboxInput(
              sprintf("%s_checkbox", x$name),
              "Range enabled",
              value = FALSE
            ),
            conditionalPanel(
              condition = sprintf("input.%s == true", sprintf("%s_checkbox", x$name)),
              sliderInput(
                inputId = sprintf("%s_range", x$name),
                label = x$title,
                min = range_of_values[1],
                max = range_of_values[2],
                value = c(range_of_values[1], range_of_values[2]),
                step = 1,
                round = TRUE
              )
            )
          )
          ,
          tabPanel(
            "Unavailable Values",
            selectizeInput(
              inputId = sprintf("%s_unavailable_values", x$name),
              label = x$title,
              multiple = TRUE,
              choices = columns %>%
                lapply(function(z) {
                  z[[x$name]][['unavailable_values']]
                }) %>%
                unlist(recursive = FALSE) %>%
                unname %>%
                unique %>%
                replace(. == "NULL", "NULL")
            )
          ),
          width = 12
        ))
      }
    })
  })
  output$data_table = renderDT({
    data_dictionary <-
      iidda.api::ops$metadata(
        dataset_ids = isolate(input$dataset_name),
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
  
  output$data_filter_table = renderDT({
    data_dictionary_names <- lapply(data_dictionary(), function(x) {
      x$name
    })
    
    data_dictionary <- lapply(data_dictionary(), function(x) {
      list(x$title, x$description)
    })
    
    names(data_dictionary) <- data_dictionary_names
    
    datatable(
      filtered_data(),
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
>>>>>>> refs/remotes/origin/main
