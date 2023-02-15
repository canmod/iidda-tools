library(shiny)
library(DT)
library(iidda.api)
library(shinydashboard)
library(shinycssloaders)
library(purrr)
require(plyr)
library(jsonlite)
source("ui.R")

downloadMenuServer <- function(id, datasets, action_button_id, data) {
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

server <- function(input, output, session) {

  # Dataset Selection Section
  data <- eventReactive(input$select_data, {
    response <- iidda.api::ops$raw_csv(dataset_ids = input$dataset_name)
    if (is.data.frame(response)) {
      response
    } else {
      data.frame(response)
    }
  })

  datasets <- reactive(names(iidda.api::ops$metadata(
    response_type = "metadata",
    metadata_search = input$data_type,
    key = ".types .resourceType"
  )))

  data_dictionary <- reactive(
    iidda.api::ops$metadata(
      response_type = "data_dictionary",
      metadata_search = input$data_type,
      key = ".types .resourceType"
    ) %>%   unlist(recursive = FALSE) %>%
      unname %>%
      unique %>%
      na.omit
  )

  output$dataset_name = renderUI(
    selectizeInput(
      inputId = "dataset_name",
      label = "Dataset Name",
      multiple = TRUE,
      choices = datasets()
    )
  )

  output$data_table = renderDT({
    data_dictionary <-
      iidda.api::ops$metadata(
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

  downloadMenuServer(id="dataset_selection", datasets = input$dataset_name)

  output$dataset_selection_download_menu <- renderUI({
    if (isTruthy(input$dataset_name) && isTruthy(data())) {
      box(
        width = NULL,
        h4("Download Combined Data"),
        downloadButton('download_combined_datasets',"Download"),
        h4("Download Individual Datasets"),
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
    } else {
      p(
        class = "text-muted",
        paste(
          'Please select some datasets before attempting to download.'
        )
      )
    }
  })

  output$download_combined_datasets <- downloadHandler(
    filename = function(){"combined_datasets.csv"},
    content = function(fname){
      write.csv(data(), fname, na="", row.names=FALSE)
    }
  )

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
            dataset_ids = input$dataset_name,
            resource = input$files_to_include
          ),
          file
        )
      })
    },
    contentType = "application/zip"
  )

  #Dataset Filtering Section

  filter_data_dictionary <- reactive(
    iidda.api::ops$metadata(
      response_type = "data_dictionary",
      metadata_search = input$filter_data_type,
      key = ".types .resourceType"
    ) %>%   unlist(recursive = FALSE) %>%
      unname %>%
      unique %>%
      na.omit
  )

  data_filters <- eventReactive(input$filter_data, {
    filter_params <- lapply(filter_data_dictionary(), function(x) {
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
      if (length(named_list) != 0L) {
        if (isTRUE(x$format == "default") & isTRUE(x$type == "string")) {
          named_list[[x$name]][named_list[[x$name]] == " -- EMPTY -- "] = ""
        }
      }
      named_list
    }) %>% unlist(recursive = FALSE)
    return(filter_params)
  })

  filtered_data <- eventReactive(input$filter_data, {
    response <- do.call(iidda.api::ops$filter, c(list(resource_type = input$filter_data_type), data_filters()))
    if(is.data.frame(response)) {
      response
    } else {
      data.frame(response)
    }
  })

  filtered_data_source_code <- eventReactive(input$filter_data, {
    df <- data_filters()
    df_names <- names(df)
    df_names <- lapply(df_names,
                       function(x) {
                         if(length(df[x][[1]]) > 1) {
                           sprintf('%s = %s', x, df[x])
                         } else {
                           sprintf('%s = "%s"', x, df[x])
                         }
                       })
    df <- paste(df_names, collapse=', ')
    sprintf('iidda.api::ops$filter(resource_type = "%s", %s)', input$filter_data_type, df)
  })

  api_request_url <- eventReactive(input$filter_data, {
    df <- data_filters()
    df_names <- names(df)
    df_names <- lapply(df_names,
                       function(x) {
                         if(length(df[x][[1]]) > 1) {
                           paste(lapply(df[[x]], function(y) { sprintf('%s=%s', x, y)}), collapse="&")
                         } else {
                           sprintf('%s=%s', x, df[x])
                         }
                       })
    df <- paste(df_names, collapse='&')
    sprintf('%sfilter?resource_type=%s&%s', substring(iidda.api::docs_url, 1, nchar(iidda.api::docs_url)-4), input$filter_data_type, df)
  })

  make_default_string_choices = function(columns, name) {
    choices = columns %>%
      lapply(function(z) {
        z[[name]]
      }) %>%
      unlist(recursive = FALSE) %>%
      unname() %>%
      unique()
    ## the " -- EMPTY -- " token will get mapped to a blank
    ## string, "",  in data_filters reactive event.
    ## this mapping will take place before the before
    ## the API code is generated.
    ## the blank string is the NULL token in iidda,
    ## for better or for worse.
    choices = c(" -- EMPTY -- ", choices)
    choices
  }

  output$column_filters = renderUI({
    columns <-
      iidda.api::ops$metadata(
        response_type = "columns",
        metadata_search = input$filter_data_type,
        key = ".types .resourceType"
      )
    lapply(filter_data_dictionary(), function(x) {
      if (x$type == "string" && x$format == "default") {
        tags$div(title=x$description,
                 selectInput(
                   inputId = x$name,
                   label = x$title,
                   multiple = TRUE,
                   choices = make_default_string_choices(columns, x$name),
                   selectize = TRUE
                 )
        )
      } else if (x$type == "date") {
        date_range <- columns %>%
          lapply(function(z) {
            z[x$name]
          }) %>%
          unlist(recursive = FALSE) %>%
          unname() %>%
          range(na.rm = FALSE)

        tags$div(title=x$description,
                 dateRangeInput(
                   inputId = x$name,
                   label = x$title,
                   start = date_range[1],
                   end = date_range[2],
                   min = date_range[1],

                   max = date_range[2],
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
            range = z[[x$name]][['range']]
            range[lengths(range) != 0]
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

  output$filter_data_table = renderDT({
    data_dictionary_names <- lapply(filter_data_dictionary(), function(x) {
      x$name
    })

    data_dictionary <- lapply(filter_data_dictionary(), function(x) {
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


  output$filter_data_download_menu = renderUI({
    if (isTruthy(input$filter_data) && isTruthy(filtered_data())) {
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
                       label = "Download")
      )
    } else {
      p(
        class = "text-muted",
        paste(
          'Please apply a filter before attempting to download.'
        )
      )
    }
  })

  output$download_filtered_data <- downloadHandler(
    filename = function(){"filtered_data.csv"},
    content = function(fname){
      write.csv(filtered_data(), fname,na="", row.names=FALSE)
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
            dataset_ids = do.call(iidda.api::ops$filter, c(list(resource_type = input$filter_data_type, response_type="dataset list"), data_filters())),
            resource = input$filtered_data_files_to_include
          ),
          file
        )
      })
    },
    contentType = "application/zip"
  )

  output$iidda_api_code <- renderUI({
    if (isTruthy(input$filter_data) && isTruthy(filtered_data())) {
      tags$pre(tags$code(filtered_data_source_code()))
    } else {
      p(
        class = "text-muted",
        paste(
          'Please apply a filter before attempting to access R code.'
        )
      )
    }
  })

  output$request_url <- renderUI({
    if (isTruthy(input$filter_data) && isTruthy(filtered_data())) {
      tags$pre(tags$code(api_request_url()))
    } else {
      p(
        class = "text-muted",
        paste(
          'Please apply a filter before attempting to access the request URL.'
        )
      )
    }
  })

}
