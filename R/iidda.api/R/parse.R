set_dict_names = function(x) {
  setNames(x, vapply(x, getElement, character(1L), "name"))
}
get_dict = function() {
  suppressWarnings({iidda.api::ops_staging$data_dictionary() |> set_dict_names()})
}

## converted to messager objects in zzz.R
messages = list(
  conversion = list(
    "Now converting some fields from character to numeric or date. ",
    "\nYou can turn this off with\noptions(iidda_api_all_char = TRUE). "
  ),
  sort = list(
    "Now sorting by date. ",
    "\nYou can turn this off with\noptions(iidda_api_date_sort = FALSE). "
  )
)
MakeMessage = function(msg) {
  self = new.env()
  self$on = TRUE
  self$msg_msg = list(
    "\nDo not display any iidda.api messages with",
    "\noptions(iidda_api_msgs = FALSE).",
    "\nMessages displayed at most once per R session.\n"
  )
  self$msg = c(msg, self$msg_msg)
  self$display = function() {
    if (isTRUE(self$on) & isTRUE(getOption("iidda_api_msgs"))) {
      do.call(message, self$msg)
      self$on = FALSE
    }
  }
  self
}

#' @importFrom iidda parse_columns
parse_api_result = function(data) {
  if (getOption("iidda_api_all_char")) return(data)
  messages$conversion$display()
  dict = get_dict()
  iidda::parse_columns(data, dict)
}

arrange_rows = function(data) {
  if (!getOption("iidda_api_date_sort")) return(data)
  messages$sort$display()
  dict = get_dict()
  if (!all(names(data) %in% names(dict))) return(data)
  if (any(names(data) == "period_end_date")) {
    data = data[order(data$period_end_date), , drop = FALSE]
  } else if (any(names(data) == "date")) {
    data = data[order(data$date), , drop = FALSE]
  }
  data
}
