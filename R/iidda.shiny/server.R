library(shiny)
library(DT)
library(iidda.api)
library(shinydashboard)
library(shinycssloaders)
library(purrr)
require(plyr)
library(jsonlite)
source("ui.R")

downloadMenuServer <- function(id, datasets) {
  moduleServer(
    id,
    function(input, output, session) {
      output$download_data <- downloadHandler(
        filename = function()
        {
          sprintf("%s.zip", 'datasets_combined')
        },
        content = function(file)
        {
          withProgress(message = 'Preparing files for download...', {
            writeBin(
              iidda.api::ops$download(
                dataset_ids = datasets,
                resource = input$files_to_include
              ),
              file
            )
          })
        },
        contentType = "application/zip"
      )
    }
  )
}

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
  
  data_filters <- eventReactive(input$filter_data, {
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
    return(filter_params)
  })
  
  filtered_data <- eventReactive(input$filter_data, {
    response <- do.call(iidda.api::ops$filter, c(list(resource_type = input$data_filter_type), data_filters()))
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
                 tags$label(x$title),
                 tabBox(
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
                         label = NULL,
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
                       label = NULL,
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
  
  downloadMenuServer(id="dataset_selection", datasets = input$dataset_name)
  
  output$filter_data_download_menu = renderUI({
    req(input$filter_data, filtered_data())
    box(
      width = NULL,
      h4("Download Filtered Data"),
      downloadButton('download_filtered_data',"Download"),
      h4("Download Individual Datasets"),
      checkboxGroupInput(
        "filtered_data_files_to_include",
        "Files to Include",
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
      downloadButton(outputId =  "download_filtered_data_individual",
                     label = "Download", )
    )
  })
  
  output$download_filtered_data <- downloadHandler(
    filename = function(){"filtered_data.csv"}, 
    content = function(fname){
      write.csv(filtered_data(), fname)
    }
  )
  
  output$download_filtered_data_individual <- downloadHandler(
    filename = function()
    {
      sprintf("%s.zip", 'filtered_datasets_combined')
    },
    content = function(file)
    {
      withProgress(message = 'Preparing files for download...', {
        writeBin(
          iidda.api::ops$download(
            dataset_ids = do.call(iidda.api::ops$filter, c(list(resource_type = input$data_filter_type, response_type="dataset list"), data_filters())),
            resource = input$filtered_data_files_to_include
          ),
          file
        )
      })
    },
    contentType = "application/zip"
  )
  

}