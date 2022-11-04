#' Read IIDDA Dataset into a Dataframe
#'
#' @param dataset_id vector of IDs of datasets in the IIDDA
#'
#' @importFrom iidda.api ops
#' @importFrom jsonlite toJSON fromJSON
#' @export
read_iidda_dataset = function(dataset_id) {

  (iidda.api::ops$raw_csv(dataset_id)
   %>% set_iidda_col_types
  )
#       return(tidy_data)
#
#   api_template = "%{api_url}s/datasets/%{dataset_id}s?response_type=raw_csv"
#   dataset_request = sprintf_named(
#     api_template,
#     api_url = getOption('iidda_api_base_url'),
#     dataset_id = dataset_id
#   )
#   read.csv(dataset_request)
}

iidda_filter = function(...) {
  err = 'all arguments need to be either character vectors or
  named lists of character vectors'
  l = list(...)
  which_char = vapply(l, is.character, logical(1L))
  which_list = vapply(l, is.list, logical(1L))
  if (!all(which_char | which_list)) stop(err)
  filter_args = c(l[which_char], do.call(c, l[which_list]))
  if (any(is_empty(names(filter_args)))) stop(err)

  allowed_filter_args = names(formals(iidda.api::ops$filter))
  good_args = names(filter_args) %in% allowed_filter_args
  if (!all(good_args)) {
    bad_args = paste0(unique(names(filter_args)[!good_args]), collapse = ";")
    stop(
      '\nthe following arguments were used:\n',
      bad_args,
      '\n\nbut only these arguments are allowed:\n',
      paste0(allowed_filter_args, collapse = ';')
    )
  }

  l = (filter_args
    %>% tapply(names(filter_args), unlist, use.names = FALSE)
    %>% sapply(unique, simplify = FALSE)
  )

  with(
    iidda_data_dictionary(),
    setNames(format, name)
  )[names(l)]

}

compile_iidda_dataset = function(
    resource_type = c(
      "Communicable Disease Incidence",
      "Population"
    ),
    ...
  ) {
  l = c(list(resource_type = resource_type), iidda_filter(list(...)))
  do.call(iidda.api::ops$filter, l)
}

#' Read IIDDA Column Index
#'
#' Read in a list containing all unique string values and numeric/date ranges
#' in each column in every dataset in IIDDA.
#'
#' This function is useful for searching IIDDA.
#'
#' @param resource_type Get column summaries for a specific resource type.
#' The default value is \code{NULL}, which gives all datasets. Other
#' acceptable values include \code{"Communicable Disease Incidence"} and
#' \code{"Population"}.
#' @importFrom iidda.api ops
#' @export
read_column_index = function(
    resource_type = NULL
) {
  by_datasets = iidda.api::ops$metadata(
    string_matching = "Equals",
    key = ".resourceType.resourceType",
    value = resource_type,
    response_type = "columns"
  )
  dict = iidda::iidda_data_dictionary()
  get_col_summary = function(col_nm) lapply(by_datasets, getElement, col_nm)
  index = (dict$name
    %>% lapply(get_col_summary)
    %>% setNames(dict$name)
  )
  index_collapsed = vector(mode = "list", nrow(dict))
  for(i in seq_len(nrow(dict))) {
    if (dict[i, "format"] == "num_missing") {
      valid_datasets = index[[i]][!unlist(lapply(lapply(index[[i]], names), is.null))]
      index_collapsed[[i]] = list(
        range = range(as.numeric(unlist(lapply(valid_datasets, getElement, "range")))),
        unavailable_values = unique(unlist(lapply(valid_datasets, getElement, "unavailable_values")))
      )
    } else if (dict[i, "format"] == "default") {
      index_collapsed[[i]] = unique(unlist(index[[i]]))
    } else if (dict[i, "format"] == "ISO8601") {
      index_collapsed[[i]] = range(as.Date(unlist(index[[i]])))
    } else {
      warning("unrecognized format: ", dict[i, "format"])
      index_collapsed[[i]] = index[[i]]
    }
  }
  setNames(index_collapsed, dict$name)
}

collapse_range = function(l, format) {

}

#' @export
iidda_dataset_names = function(resource_type = NULL) {
  names(ops$metadata(
    string_matching = "Equals",
    key = ".resourceType.resourceType",
    value = resource_type,
    response_type = ""
  ))
}
