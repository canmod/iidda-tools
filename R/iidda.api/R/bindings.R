
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
#' @importFrom rapiclient get_api get_operations set_default_args_list
#' @importFrom iidda list_xpath rm_trailing_slash
#' @importFrom utils browseURL

handle_iidda_response <- function(x) {
    if (x$status_code == 400L) {
      api_err_msg = (httr::content(x)$detail
        |> unlist(use.names = FALSE, recursive = TRUE)
      )
      err_tmplt = "The IIDDA API returned the following error:\n    %s\nThis documentation might be of interest:\n    %s"
      err_msg = sprintf(err_tmplt, api_err_msg, iidda.api::docs_url_staging)
      stop(err_msg)
    } else if (x$status_code == 404L) {
      msg = sprintf("404 Not Found. The requested URL was not found on the server. If you entered the URL manually please check your spelling and try again. This documentation might be of interest:\n    %s", iidda.api::docs_url_staging)
      stop(msg)
    } else if (x$status_code != 200L) {
      err_tmplt = "Something went wrong with the IIDDA API.\nStatus code: %s\nThis documentation might be of interest:\n    %s"
      err_msg = sprintf(err_tmplt, x$status_code, iidda.api::docs_url_staging)
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
      ) |> arrange_rows() |> parse_api_result() # only if options set
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
  try(
    rapiclient::get_api(
      url = file.path(
        iidda::rm_trailing_slash(file.path(api_url, base_path)),
        'openapi.json'
      )
    ),
    silent = TRUE
  )
}

our_rapiclient_fork = function() {
  ## look for a zero-byte file in the rapiclient package. the
  ## existence of this file would indicate that the user has
  ## the our fork of rapiclient that supports passing vectors.
  file.exists(system.file("canmod", package = "rapiclient"))
}
msg_rapiclient = function() {
  c(
      "Your installation of rapiclient does not support passing "
    , "vectors to the arguments of iidda.api functions. "
    , "If you wish to pass vectors, please follow installation "
    , "instructions for rapiclient here: "
    , "https://canmod.r-universe.dev/rapiclient"
  )
}
check_rapiclient = function() {
  if (!our_rapiclient_fork()) warning(msg_rapiclient())
}

make_ops_list = function(api_url, base_path, type) {
  check_rapiclient()

  summary_to_function_name = function(x) {
    gsub(pattern = " ", replacement = "_", tolower(x))
  }

  iidda_api = make_api_obj(api_url, base_path, type)
  if (class(iidda_api)[1] == 'try-error') {
    backup = iidda.api:::cached_api_list[[type]]
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
#' IIDDA API.
#'
#' IIDDA is the International Infectious Disease Data Archive.
#' This archive has an [API](https://math.mcmaster.ca/iidda/api/docs)
#' (Application Programming Interface)
#' for accessing data and potentially for building applications.
#' This R package provides a simple wrapper to this API so that
#' the datasets are returned as data frames.
#'
#' Useful links for people who just want to get data.
#' * `?featured`
#' * `vignette("QuickStart")`
#' * `vignette("Provenance")`
#'
#' More advanced users might be interested in the lower-level wrapper
#' of the API operations here: `?ops`.
#'
"_PACKAGE"

#' IIDDA API Operations
#'
#' Objects containing the functions associated with API functions
#' documented [here](https://math.mcmaster.ca/iidda/api/docs).
#'
#' @examples
#' ## Print out the available functions.
#' names(ops_staging)
#'
#' ## Access functions with a dollar sign. For example, this command
#' ## will give weekly incidence data in PEI in January of 1940.
#' ops_staging$filter(
#'      resource_type = "Compilation"
#'    , dataset_id = "canmod-cdi-normalized"
#'    , iso_3166_2 = "CA-PE"
#'    , period_end_date = "1940-01-01..1940-02-01"
#'    , time_scale = "wk"
#' )
#'
#' ## Operations objects that are not available are error objects. As of the
#' ## time of writing only ops_staging is live.
#' print(class(ops))
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

