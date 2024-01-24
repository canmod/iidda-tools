set_dict_names = function(x) setNames(x, vapply(x, getElement, character(1L), "name"))
get_dict = function() suppressWarnings({iidda.api::ops_staging$data_dictionary() |> set_dict_names()})

parse_columns = function(data) {
  if (getOption("iidda_api_all_char")) return(data)
  if (getOption("iidda_api_msgs")) {
    message(
      "Now converting some fields from character to numeric or date. ",
      "You can turn this off with\noptions(iidda_api_all_char = TRUE). ",
      "Turn these messages off with\noptions(iidda_api_msgs = FALSE).",
      "\n"
    )
  }
  dict = get_dict()
  if (!all(names(data) %in% names(dict))) return(data)
  for (cc in names(data)) {
    if (dict[[cc]]$type == "string" & dict[[cc]]$format == "num_missing") {
      data[[cc]] = readr::parse_number(data[[cc]])
    }
    if (dict[[cc]]$type == "date" & dict[[cc]]$format == "ISO8601") {
      data[[cc]] = readr::parse_date(data[[cc]])
    }
  }
  data
}

arrange_rows = function(data) {
  if (!getOption("iidda_api_date_sort")) return(data)
  if (getOption("iidda_api_msgs")) {
    message(
      "Now sorting by date. ",
      "You can turn this off with\noptions(iidda_api_date_sort = FALSE). ",
      "Turn these messages off with\noptions(iidda_api_msgs = FALSE). ",
      "\n"
    )
  }
  dict = get_dict()
  if (!all(names(data) %in% names(dict))) return (data)
  if (any(names(data) == "period_end_date")) {
    data = data[order(period_end_date), , drop = FALSE]
    #data = dplyr::arrange(data, period_end_date)
  } else if (any(names(data) == "date")) {
    data = data[order(date), , drop = FALSE]
    #data = dplyr::arrange(data, date)
  }
  data
}
