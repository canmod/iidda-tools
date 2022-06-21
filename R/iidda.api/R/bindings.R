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

  requests_names = summary_to_function_name(
    unlist(list_xpath(iidda_api$paths, 'get', 'summary'))
  )
  setNames(raw_requests, requests_names)
}

#' IIDDA Requests
#' @export
ops = make_ops_list(default_api_base_url)
