
# staging
staging = list(
  api_url = "https://math.mcmaster.ca",
  base_path = "iidda/api"
)

# local development
local = list(
  api_url = "http://127.0.0.1:8000",
  base_path = ""
)

# production environment
production = NULL

#' @importFrom stats setNames
#' @importFrom readr cols
#' @importFrom httr content
#' @importFrom rapiclient get_api get_operations set_default_args_list
#' @importFrom iidda list_xpath rm_trailing_slash

handle_iidda_response <- function(x) {
    content_type <- x$headers$`content-type`
    if (content_type == 'application/json') {
      return(httr::content(x))
    }
    else if (content_type == 'text/plain; charset=utf-8') {
      data = httr::content(x
        , type = "text/csv"
        , encoding = "UTF-8"
        , col_types = readr::cols(.default = "c") # read all columns in as strings
        , na = character() # nothing is missing, only blank
      ) |> arrange_rows() |> parse_columns() # only if options set
      return(data)
    }
    else if (content_type == 'application/x-zip-compressed') {
      message("retrieving zip archive with your system's default browser ...")
      browseURL(x$url)
      invisible(x)
    }
    else {
      return(httr::content(x))
    }
  }

make_ops_list = function(api_url, base_path) {

  summary_to_function_name = function(x) {
    gsub(pattern = " ", replacement = "_", tolower(x))
  }

  iidda_api = try(
    rapiclient::get_api(
      url = file.path(
        iidda::rm_trailing_slash(file.path(api_url, base_path)),
        'openapi.json'
      )
    ),
    silent = TRUE
  )

  if (class(iidda_api)[1] == 'try-error') return(iidda_api)

  iidda_api$basePath = file.path('',  base_path)

  raw_requests = rapiclient::get_operations(
    iidda_api,
    handle_response = handle_iidda_response
  )

  parameter_list <- function(x) {
    parameters <- environment(raw_requests[[x]])[["op_def"]][["parameters"]]
    default_values <- list()
    for (parameter in parameters) {
      if (parameter[["required"]] == FALSE) {
        default_values[[parameter[["name"]]]] =
          parameter[["schema"]][["default"]]
      } else {
        next
      }
    }
    return(default_values)
  }

  for (name in names(raw_requests)) {
    raw_requests[[name]] <- rapiclient::set_default_args_list(
      raw_requests[[name]],
      parameter_list(name)
    )
  }

  get_request_names = summary_to_function_name(
    iidda::list_xpath(iidda_api$paths, 'get', 'summary')
  )
  post_request_names = summary_to_function_name(
    iidda::list_xpath(iidda_api$paths, 'post', 'summary')
  )
  request_names = ifelse(
    get_request_names == "list()",
    post_request_names,
    get_request_names
  )
  setNames(raw_requests, request_names)
}

#' \pkg{iidda.api}
#'
#' R binding to the IIDDA API.
#'
#' IIDDA is the International Infectious Disease Data Archive.
#' This archive has an API (Application Programming Interface)
#' for accessing data and potentially for building applications.
#' This R package provides a simple wrapper to this API so that
#' the datasets are returned as data frames.
#'
#' ## Listing the Datasets
#'
#' ```{r}
#' iidda.api::ops_staging$metadata() |> names()
#' ```
#'
#' ## Getting Datasets
#'
#' Just choose one of the identifiers above, and pass it to the
#' `raw_csv` function in the API.
#' ```{r, eval = FALSE}
#' cdi_1975 = iidda.api::ops_staging$raw_csv(
#'    dataset_ids = c(
#'      "cdi_ca_1975_wk_prov_statcan",
#'      "cdi_ca_1976_wk_prov_statcan"
#'    )
#' )
#' ```
#'
#' You can pass more than one dataset.
#' ```{r, eval = FALSE}
#' cdi_1975 = iidda.api::ops_staging$raw_csv(
#'    dataset_ids = "cdi_ca_1975_wk_prov_statcan"
#' )
#' ```
#'
#'
#' ## All Metadata by Dataset
#'
#' ```{r, eval = FALSE}
#' iidda.api::ops_staging$metadata()
#' ```
#'
#' On my browser at least, it is easier to look at in JSON form rather than
#' R list form. The JSON metadata can be directly accessed using
#' the URL form of the API:
#' [https://math.mcmaster.ca/iidda/api/metadata](https://math.mcmaster.ca/iidda/api/metadata).
#'
#' ## CANMOD Digitization Project
#'
#' The communicable disease incidence (CDI) data collected as part of
#' the CANMOD digitization project can be accessed using
#' `resource_type = "CANMOD CDI"` with the `filter` function.
#'
#' ```{r, eval = FALSE}
#' canmod_cdi = iidda.api::ops_staging$filter(
#'      resource_type = "CANMOD CDI"
#'    , iso_3166 = "CA" ## country code for canada
#' )
#' ```
#'
"_PACKAGE"

#' IIDDA API Operations
#' @name ops
NULL

#' @importFrom iidda list_xpath
#' @describeIn ops List containing available operations from the IIDDA API
#' as \code{R} functions
#' @export
ops = suppressWarnings({try(do.call(make_ops_list, production), silent = TRUE)})

#' @describeIn ops Operations list for a local development environment,
#' if it exists
#' @export
ops_local = suppressWarnings({try(do.call(make_ops_list, local), silent = TRUE)})

#' @describeIn ops Operations list for a staging environment, if it exists
#' @export
ops_staging = suppressWarnings({try(do.call(make_ops_list, staging), silent = TRUE)})

docs_url = try(file.path(production$api_url, "docs"), silent = TRUE)

docs_url_local = try(file.path(local$api_url, "docs"), silent = TRUE)

docs_url_staging = try(file.path(staging$api_url, "docs"), silent = TRUE)
