default_api_base_url = "http://127.0.0.1:8000"

make_ops_list = function(api_base_url) {
  handle_iidda_response <- function(x) {
    content_type <- x$headers$`content-type`
    if (content_type == 'application/json') {
      return(httr::content(x))
    }
    else if (content_type == 'text/plain; charset=utf-8') {
      return(httr::content(x, type="text/csv", encoding = "UTF-8"))
    }
    else {
      return(httr::content(x))
    }
  }

  summary_to_function_name = function(x) {
    gsub(pattern = " ", replacement = "_", tolower(x))
  }

  iidda_api = get_api(
    url = file.path(api_base_url, 'openapi.json')
  )

  raw_requests = get_operations(
    iidda_api,
    handle_response = handle_iidda_response
  )

  parameter_list <- function(x) {
    parameters <- environment(raw_requests[[x]])[["op_def"]][["parameters"]]
    default_values <- list()
    for (parameter in parameters) {
      if(parameter[["required"]] == FALSE) {
        default_values[[parameter[["name"]]]] <- parameter[["schema"]][["default"]]
      } else {
        next
      }
    }
    return(default_values)
  }

  for (name in names(raw_requests)) {
    raw_requests[[name]] <- set_default_args_list(raw_requests[[name]], parameter_list(name))
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

#' IIDDA Requests
#' @export
ops = make_ops_list(default_api_base_url)
