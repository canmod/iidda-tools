
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
production = local

# data dictionary location
global_data_dictionary_url = file.path(
  "https://raw.githubusercontent.com", # api
  "canmod", # github user/org
  "iidda", # github repo
  "main", # github branch
  "global-metadata", # folder
  "data-dictionary.json" # file
)

default_global_data_dictionary = try(
  read_json(global_data_dictionary_url),
  silent = TRUE
)

global_data_dictionary = function() {
  current_global_data_dictionary_url = try(
    read_json(global_data_dictionary_url),
    silent = TRUE
  )
  if (class(current_global_data_dictionary_url) != 'try-error') {
    return(current_global_data_dictionary_url)
  } else if (class(default_global_data_dictionary) != 'try-error') {
    return(default_global_data_dictionary)
  } else {
    stop('cannot find iidda data dictionary')
  }
}

make_ops_list = function(api_url, base_path) {
  handle_iidda_response <- function(x) {
    content_type <- x$headers$`content-type`
    if (content_type == 'application/json') {
      return(httr::content(x))
    }
    else if (content_type == 'text/plain; charset=utf-8') {
      x_data_frame = httr::content(
        x,
        type="text/csv",
        encoding = "UTF-8",
        col_types = readr::cols(.default = "c")
      )
      dict = iidda.api:::global_data_dictionary()
      allowed_names = iidda::list_xpath(dict, 'name') %>% unlist
      if (!all(names(x_data_frame) %in% allowed_names)) {
        warning(
          "\nthe global iidda data dictionary is out of sync",
          "\nwith one or more iidda datasets. returning all",
          "\ncolumns as strings."
        )
        return(x_data_frame)
      }
      tidy_data = (dict
        %>% iidda::key_val('name', 'type')
        %>% get_elements(colnames(x_data_frame))
        %>% unlist
        %>% iidda::lookup(iidda::col_classes_dict)
        %>% iidda::set_types(data = x_data_frame)
      )
      return(tidy_data)
    }
    else {
      return(httr::content(x))
    }
  }

  summary_to_function_name = function(x) {
    gsub(pattern = " ", replacement = "_", tolower(x))
  }

  iidda_api = try(
    get_api(
      url = file.path(
        rm_trailing_slash(file.path(api_url, base_path)),
        'openapi.json'
      )
    ),
    silent = TRUE
  )

  if (class(iidda_api)[1] == 'try-error') return(iidda_api)

  iidda_api$basePath = file.path('',  base_path)

  raw_requests = get_operations(
    iidda_api,
    handle_response = handle_iidda_response
  )

  parameter_list <- function(x) {
    parameters <- environment(raw_requests[[x]])[["op_def"]][["parameters"]]
    default_values <- list()
    for (parameter in parameters) {
      if(parameter[["required"]] == FALSE) {
        default_values[[parameter[["name"]]]] <-
          parameter[["schema"]][["default"]]
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
    list_xpath(iidda_api$paths, 'get', 'summary')
  )
  post_request_names = summary_to_function_name(
    list_xpath(iidda_api$paths, 'post', 'summary')
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
"_PACKAGE"

#' IIDDA API Operations
#' @name ops
NULL

#' @describeIn ops List containing available operations from the IIDDA API
#' as \code{R} functions
#' @export
ops = try(do.call(make_ops_list, production), silent = TRUE)

#' @describeIn ops Operations list for a local development environment,
#' if it exists
#' @export
ops_local = try(do.call(make_ops_list, local), silent = TRUE)

#' @describeIn ops Operations list for a staging environment, if it exists
#' @export
ops_staging = try(do.call(make_ops_list, staging), silent = TRUE)
