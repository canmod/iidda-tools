#' Data Provenance
#'
#' Get URLs to dependencies of datasets for investigating
#' data provenance. See `vignette("Provenance")` for an
#' illustration. See
#' [iidda-staging](https://github.com/canmod/iidda-staging#identifiers)
#' for information on IIDDA identifiers. If you do not have access
#' to this link, please contact the
#' [maintainer](https://github.com/canmod/iidda-tools/tree/main/R/iidda.api#maintainer).
#'
#' @param dataset_ids Identifiers for datasets that will
#' restrict the search for dependencies to metadata for
#' these datasets.
#' @param metadata List of IIDDA DataCite metadata over
#' a collection of datasets.
#'
#' @name urls
NULL


#' @param scan_ids Character vector of identifiers for scans.
#' @describeIn urls Convert scan identifiers into URLs that link
#' to associated scans (typically PDF files).
#' @export
url_scans = function(scan_ids
  , dataset_ids = character()
  , metadata = ops_staging$metadata(dataset_ids = dataset_ids)
) {
  resource_urls(scan_ids, "scans", dataset_ids, metadata)
}
#' @param digitization_ids Character vector of identifiers for digitizations.
#' @describeIn urls Convert digitization identifiers into URLs that link
#' to associated digitizations (typically Excel files).
#' @export
url_digitizations = function(digitization_ids
  , dataset_ids = character()
  , metadata = ops_staging$metadata(dataset_ids = dataset_ids)
) {
  resource_urls(digitization_ids, "digitizations", dataset_ids, metadata)
}
#' @param digitization_id Single digitization identifier.
#' @describeIn urls Returns URLs to all prep scripts that could have
#' their output modified if the identified digitization were modified.
#' @export
url_affected_scripts = function(digitization_id
  , dataset_ids = character()
  , metadata = ops_staging$metadata(dataset_ids = dataset_ids)
) {
  ## list over datasets giving the urls to the related identifiers.
  ## related identifiers is DataCite terminology, which in our case here
  ## means resources like Excel files and R scripts.
  rel_ids = lapply(metadata, getElement, "relatedIdentifiers")
  rel_urls = lapply(rel_ids, vapply, getElement, character(1L), 1L)

  ## url for the focal digitization
  dig_urls = url_digitizations(digitization_id)

  ## dataset IDs of datasets that will be affected
  affected_dataset_ids = vapply(rel_urls, function(x) any(x %in% dig_urls), logical(1L))
  affected_dataset_ids = names(which(affected_dataset_ids))

  ## the first resource is always the main prep script of the dataset,
  ## and so we extract these urls to those script.
  vapply(
      rel_urls[affected_dataset_ids]
    , getElement
    , character(1L)
    , 1L
    , USE.NAMES = FALSE
  )
}


resource_urls = function(resource_ids
  , type = c("scans", "digitizations", "prep-scripts", "access-scripts")
  , dataset_ids = character()
  , metadata = ops_staging$metadata(dataset_ids = dataset_ids)
  , valid_ext = c("pdf", "csv", "xlsx", "json", "xls", "R", "py")
) {
  type = match.arg(type)
  relation_type = switch(type
    , scans = "References"
    , digitizations = "IsDerivedFrom"
    , `prep-scripts` = "IsCompiledBy"
    , `access-scripts` = "References"
  )
  ext_pat = paste(valid_ext, collapse = "|")
  urls = dataset_dependency_urls(dataset_ids, relation_type, metadata)
  pat = sprintf("%s/%s.(%s)$", type, resource_ids, ext_pat)
  filtered_urls = lapply(pat, grep, urls, value = TRUE)
  filtered_urls = unique(unlist(filtered_urls, use.names = FALSE))
  return(filtered_urls)
}

dataset_dependency_urls = function(dataset_ids = character()
    , dependency_types = c("IsCompiledBy", "IsDerivedFrom", "References")
    , metadata = ops_staging$metadata(dataset_ids = dataset_ids)
) {
  identifier_list = related_identifiers(dataset_ids, metadata)
  filtered_list = Filter(
      function(x) x$relationType %in% dependency_types
    , identifier_list
  )
  rel_ids = lapply(filtered_list, getElement, "relatedIdentifier")
  unlist(rel_ids, use.names = FALSE)
}

related_identifiers = function(dataset_ids = character()
  , metadata = ops_staging$metadata(dataset_ids = dataset_ids)
) {
  rel_ids = lapply(metadata, getElement, "relatedIdentifiers")
  unique(unlist(rel_ids, recursive = FALSE, use.names = FALSE))
}
