#' Featured Datasets
#'
#' Access featured datasets and associated metadata. These datasets required
#' substantial preparation to enhance their utility for research and analysis.
#'
#' @name featured
NULL

#' @param dataset_id ID for a particular featured dataset. Run
#' `featured_ids()` for the list of available options.
#' @param ... Character vectors for filtering the data on specific columns,
#' with one vector for each column. See examples below for the syntax for
#' different types of columns. The following list gives instructions for
#' columns that might be available for your dataset. If they are not available
#' you will get a message telling you what columns are in the dataset
#' that you are asking for. `r roxygen_featured_params()`
#' @describeIn featured Return a data frame of a featured
#' dataset, possibly filtered.
#'
#' @examples
#' options(iidda_api_msgs = FALSE)
#' featured_ids()
#' atlantic_polio_1950s = featured_data("canmod-cdi-normalized"
#'   , iso_3166_2 = c("CA-NL", "CA-NS", "CA-PE", "CA-NB")
#'   , basal_disease = "poliomyelitis"
#'   , period_end_date = "1950-01-01..1959-12-31"
#' )
#' head(atlantic_polio_1950s)
#'
#' @export
featured_data = function(dataset_id, ...) {
  dataset_id = as.character(dataset_id)
  if (length(dataset_id) != 1L) {
    stop("Can only get one featured dataset at a time.")
  }
  cid = featured_ids()
  if (!dataset_id %in% cid) {
    stop(
        "Please specify the ID for a featured dataset. Your options are:"
      , paste(cid, collapse = "\n  ")
    )
  }
  args_filter = list(...)
  if (length(args_filter) == 0L) {
    output = iidda.api::ops_staging$raw_csv(dataset_ids = dataset_id)
    return(output)
  }
  cnames_filter_valid = simple_filter_params() |> names()
  cnames_filter = names(args_filter) # names of columns used in the filter

  cnames_data = ops_staging$metadata(
      dataset_ids = dataset_id
    , response_type = "data_dictionary"
  )[[1]] |> vapply(\(x) x$name, character(1L))
  cnames_data_valid = intersect(cnames_data, cnames_filter_valid)

  cnames_bad = setdiff(cnames_filter, cnames_data_valid)
  if (length(cnames_bad) > 0L) {
    warning(
        "\nYou set filters for the following columns:\n  "
      , paste(cnames_bad, collapse = "  \n  ")
      , "\nBut these filters have been ignored because "
      , "only the following columns in this dataset can be filtered:\n  "
      , paste(cnames_data_valid, collapse = "  \n  ")
    )
  }

  cnames_good = intersect(cnames_filter, cnames_data_valid)
  if (length(cnames_good) == 0L) {
    output = iidda.api::ops_staging$raw_csv(dataset_ids = dataset_id)
    return(output)
  }

  args = list(
      resource_type = "Compilation"
    , response_type = "csv"
    , dataset_ids = dataset_id
  )
  args_filter = args_filter[cnames_good]
  args = c(args, args_filter)
  output = do.call(ops_staging$filter, args)[ , cnames_data, drop = FALSE]
  return(output)
}

all_filter_params = function() {
  filter_params = try(attributes(ops_staging$filter)$definition$parameters)
  if (inherits(filter_params, "try-error")) {
    filter_params = cached_api_list$staging$paths$`/filter`$get$parameters
  }
  names(filter_params) = vapply(filter_params, \(x) x$name, character(1L))
  return(filter_params)
}
simple_filter_params = function() {
  filter_params = all_filter_params()
  exclude = c("resource_type", "response_type", "dataset_ids")
  pnames = setdiff(names(filter_params), exclude)
  filter_params = filter_params[pnames]
  return(filter_params)
}
roxygen_featured_params = function() {
  filter_params = simple_filter_params()
  pdesc = vapply(filter_params, \(x) x$description, character(1L))
  vec = sprintf("\n* %s : %s", names(filter_params), pdesc)
  paste(vec, collapse = "")
}

#' @describeIn featured Return a list of lists, each of which represents
#' the metadata for a featured dataset.
#' @export
featured_metadata = function() {
  ops_staging$metadata(
      metadata_search = "Compilation"
    , key = ".types .resourceType"
    , string_comparison = "Equals"
    , response_type = "metadata"
  )
}

#' @describeIn featured Return the IDs of all the featured
#' datasets in the repository.
#' @export
featured_ids = function() names(featured_metadata())


