library(shiny)
library(DT)
library(iidda.api)
library(shinydashboard)
library(shinycssloaders)
library(purrr)
require(plyr)
library(jsonlite)

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
    tabName = "data_filtering"
  )
))

body <- dashboardBody(tabItems(tabItem(
  tabName = "dataset_selection",
  h2("Dataset Selection"),
  fluidRow(column(
    width = 12,
    tabBox(
      width = 12,
      tabPanel(
        "Data Viewer",
        DT::dataTableOutput("data_table") %>% withSpinner(color = "#FDBF57"),
        style = "height: fit-content; overflow-y:scroll; overflow-x:scroll;"
      ),
      tabPanel(
        "Download",
        uiOutput("dataset_selection_download_menu")
      )),
  ),
  column(
    width = 12,
    box(
      h3("Dataset Selection"),
      width = 12,
      selectInput(
        inputId = "data_type",
        label = "Dataset Type",
        choices = iidda.api::ops$metadata(response_type = "metadata", jq_query = '[.[] | select(. != "No metadata.") | .types .resourceType] | unique'),
        selected = "Communicable Disease Incidence"
      ),
      uiOutput("dataset_name"),
      p(
        class = "text-muted",
        paste('Changes will only apply when you click "Apply Changes"')
      ),
      actionButton("select_data", "Apply Changes")
    )
    )
  )),
tabItem(
  tabName = "data_filtering",
  h2("Data Filtering"),
  fluidRow(column(
    width = 12,
    tabBox(
      width = 12,
      tabPanel(
        "Filtered Data",
        DT::dataTableOutput("data_filter_table") %>% withSpinner(color = "#FDBF57"),
        style = "height: fit-content; overflow-y:scroll; overflow-x:scroll;"
      ),
      tabPanel(
        "Download",
        uiOutput("filter_data_download_menu")
      )),
    box(
      h3("Dataset Filtering"),
      width = 12,
      p(
        class = "text-muted",
        paste('Changes will only apply when you click "Apply Changes"')
      ),
      actionButton("filter_data", "Apply Changes"),
      selectInput(
        inputId = "data_filter_type",
        label = "Dataset Type",
        choices = iidda.api::ops$metadata(response_type = "metadata", jq_query = '[.[] | select(. != "No metadata.") | .types .resourceType] | unique'),
        selected = "Communicable Disease Incidence"
      ),
      uiOutput("column_filters"),
    ),
  )
))
)
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