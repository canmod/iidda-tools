
# staging
staging = list(
  api_url = "https://math.mcmaster.ca",
  base_path = "iidda/api",
  type = "staging"
)

# local development
local = list(
  api_url = "http://127.0.0.1:8000",
  base_path = "",
  type = "local"
)

# production environment
production = NULL

#' @importFrom stats setNames
#' @importFrom readr cols
#' @importFrom httr content
#' @importFrom rapiclient get_api get_operations
#' @importFrom iidda list_xpath rm_trailing_slash
#' @importFrom utils browseURL

handle_iidda_response <- function(x) {
    if (x$status_code == 400L) {
      response_content = httr::content(x)
      if ("detail" %in% names(response_content)) { ## condition needed because 400s can originate outside of the iidda api
        api_err_msg = unlist(response_content$detail
          , use.names = FALSE
          , recursive = TRUE
        )
        err_tmplt = paste(
            "The IIDDA API returned the following error:\n    %s\nThis"
          , "documentation might be of interest:\n    %s"
        )
        err_msg = sprintf(err_tmplt, api_err_msg, iidda.api::docs_url_staging)
        stop(err_msg)
      }
    } else if (x$status_code == 404L) {
      err_tmplt = paste(
          "404 Not Found. The requested URL was not found on the server."
        , "If you entered the URL manually please check your spelling and"
        , "try again. This documentation might be of interest:\n    %s"
      )
      err_msg = sprintf(err_tmplt, iidda.api::docs_url_staging)
      stop(err_msg)
    }
    if (x$status_code != 200L) {
      err_tmplt = paste(
          "Something went wrong with the IIDDA API.\nStatus code:"
        , "%s\nRequest URL: %s\nThis documentation might be of"
        , "interest:\n    %s"
      )
      err_msg = sprintf(err_tmplt
        , x$status_code, x$url, iidda.api::docs_url_staging
      )
      stop(err_msg)
    }
    content_type <- x$headers$`content-type`
    if (content_type == 'application/json') {
      data = httr::content(x)

      ## data dictionary has names in the first element of each inner list
      if (is.null(names(data))) {
        try_names = try(
          vapply(data, getElement, character(1L), "name"),
          silent = TRUE
        )
        if (!inherits(try_names, "try-error")) names(data) = try_names
      }
      return(data)
    }
    else if (content_type == 'text/plain; charset=utf-8') {
      data = httr::content(x
        , type = "text/csv"
        , encoding = "UTF-8"
        , col_types = readr::cols(.default = "c") # read all columns in as strings
        , na = character() # nothing is missing, only blank
      )
      data = parse_api_result(arrange_rows(data)) # only if options set
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

make_api_obj = function(api_url, base_path, type) {
  api_obj = try(
    rapiclient::get_api(
      url = file.path(
        iidda::rm_trailing_slash(file.path(api_url, base_path)),
        'openapi.json'
      )
    ),
    silent = TRUE
  )
  return(api_obj)
}

make_ops_list = function(api_url, base_path, type) {
  summary_to_function_name = function(x) {
    gsub(pattern = " ", replacement = "_", tolower(x))
  }

  iidda_api = make_api_obj(api_url, base_path, type)
  if (class(iidda_api)[1] == 'try-error') {
    backup = cached_api_list[[type]]
    if (inherits(backup, "rapi_api")) {
      iidda_api = backup
    } else {
      return(iidda_api)
    }
  }

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
        default_values[[parameter[["name"]]]] = parameter[["schema"]][["default"]]
      } else {
        next
      }
    }
    return(default_values)
  }

  for (name in names(raw_requests)) {
    raw_requests[[name]] <- set_default_args_list(
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
  requests = setNames(raw_requests, request_names)

  ## HACK: use our .api_args function (iidda.api/R/rapiclient.R),
  ## instead of the function of the same name in the rapiclient package.
  ## the benefit to users is being able to filter on a set of values
  ## as opposed to single values. for example, i can filter on the
  ## `disease` column taking values in the set `c("measles", "chickenpox")`.
  ## without this .api_args function we would need to ask for measles
  ## and chickenpox in separate queries, which isn't the end of the
  ## world but why not allow it.
  for (method in names(requests)) {
    assign(".api_args", .api_args, environment(requests[[method]]))
  }

  return(requests)
}

#' IIDDA API Operations
#'
#' Objects containing the functions associated with API functions
#' documented [here](https://math.mcmaster.ca/iidda/api/docs).
#' These objects are for advanced usage, providing more functionality
#' than the simpler tools for accessing \code{\link{featured_data}}.
#'
#' @examples
#' ## Print out the available functions.
#' names(ops_staging)
#'
#' ## Access functions with a dollar sign. For example, this command
#' ## will give weekly incidence data in January of 1956.
#' options(iidda_api_msgs = FALSE, iidda_api_all_char = TRUE)
#' jan_56 = ops_staging$filter(
#'      resource_type = "Communicable Disease Incidence"
#'    , dataset_id = "cdi_ca_1956_wk_prov_dbs"
#'    , period_end_date = "1956-01-01..1956-02-01"
#'    , time_scale = "wk"
#' )
#' cols = c(
#'     "period_end_date"
#'   , "location"
#'   , "historical_disease"
#'   , "cases_this_period"
#' )
#' print(jan_56[, cols])
#'
#' ## Operations objects that are not available are error objects. As of the
#' ## time of writing `ops` is not live, but will be.
#' print(class(ops))
#'
#' ## The `ops_local` is only live for developers who have deployed a
#' ## local version of the API.
#' print(class(ops_local))
#'
#' @name ops
NULL


#' @importFrom iidda list_xpath
#' @describeIn ops List containing available operations from the IIDDA API
#' as \code{R} functions
#' @export
ops = NULL

#' @describeIn ops Operations list for a local development environment,
#' if it exists
#' @export
ops_local = NULL

#' @describeIn ops Operations list for a staging environment, if it exists
#' @export
ops_staging = NULL

#' Links to IIDDA API Interactive Documentation
#' @name interactive
NULL

#' @describeIn interactive Link to interactive documentation.
#' @export
docs_url = try(file.path(production$api_url, production$base_path, "docs"), silent = TRUE)

#' @describeIn interactive Link to interactive documentation for a staging
#' environment.
#' @export
docs_url_staging = try(file.path(staging$api_url, staging$base_path, "docs"), silent = TRUE)

#' @describeIn interactive Localhost link to interactive documentation for
#' a development environment, if it exists.
#' @export
docs_url_local = try(file.path(local$api_url, local$base_path, "docs"), silent = TRUE)

