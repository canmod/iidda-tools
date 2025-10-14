#' CANDID Datasets
#'
#' Access datasets used in the paper describing the
#' Canadian Disease Incidence Dataset (CANDID).
#'
#' @name candid
NULL

#' @param dataset_id ID for a particular CANDID dataset. Run
#' `candid_data_ids()` for the list of available options.
#' @describeIn candid Return a data frame of one of the datasets used in the
#' CANDID paper.
#' @export
candid_data = function(dataset_id) {
  dataset_id = as.character(dataset_id)
  valid_dataset_ids = candid_data_ids()
  if (!dataset_id %in% valid_dataset_ids) {
    stop(
        "\nYou asked for a dataset that is not used by the CANDID paper. "
      , "Please select from one of the following dataset IDs:\n"
      , paste(valid_dataset_ids, collapse = "  \n  ")
    )
  }
  if (getOption("iidda_api_pull_msg")) message("Pulling dataset ", dataset_id)
  data = iidda.api::ops_staging$raw_csv(dataset_ids = dataset_id)
  return(data)
}

#' @param lookup_id ID for a particular CANDID lookup table. Run
#' `candid_lookup_ids()` for the list of available options.
#' @describeIn candid Return a data frame of one of the datasets used in the
#' CANDID paper.
#' @export
candid_lookup = function(lookup_id) {
  lookup_id = as.character(lookup_id)
  valid_lookup_ids = candid_lookup_ids()
  if (!lookup_id %in% valid_lookup_ids) {
    stop(
        "\nYou asked for a dataset that is not used by the CANDID paper. "
      , "Please select from one of the following dataset IDs:\n"
      , paste(valid_lookup_ids, collapse = "  \n  ")
    )
  }
  if (getOption("iidda_api_pull_msg")) message("Pulling lookup ", lookup_id)
  lookup = iidda.api::ops_staging$lookup_tables(lookup_type = lookup_id)
  return(lookup)
}

#' @describeIn candid Return a list of lists, each of which represents
#' the metadata for a CANDID dataset.
#' @export
candid_metadata = function() ops_staging$metadata(dataset_ids = candid_data_ids())

#' @describeIn candid Return the IDs of all the datasets
#' used in the CANDID paper.
#' @export
candid_data_ids = function() {
  c(
      "canmod-cdi-unharmonized"
    , "canmod-cdi-harmonized"
    , "canmod-cdi-normalized"
    , "canmod-pop-normalized"
    , "phac-cdi-portal"
    , "phac-reporting-schedule"
    , "canmod-disease-cross-check"
    , "canmod-location-cross-check"
    , "canmod-time-scale-cross-check"
  )
}

#' @describeIn candid Return the IDs of all the lookup tables
#' used in the CANDID paper.
#' @export
candid_lookup_ids = function() {
  c("phac-to-canmod-disease-lookup", "canmod-disease-lookup")
}
