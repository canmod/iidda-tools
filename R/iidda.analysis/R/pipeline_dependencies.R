#' Browse Pipeline Dependencies
#'
#' Open a browser at the locations of the dependencies associated with a set
#' of datasets.
#'
#' @param dataset_ids Character vector of dataset identifiers.
#' @param dependency_types Vector of types of dependencies to browse. Possible
#' values include \code{"IsCompiledBy"}, \code{"IsDerivedFrom"}, and
#' \code{"References"}.
#' @param metadata Optional list giving dataset metadata. The default uses
#' the IIDDA API, which requires the internet.
#'
#' @importFrom utils browseURL
#' @export
browse_pipeline_dependencies = function(dataset_ids
    , dependency_types = c("IsCompiledBy", "IsDerivedFrom", "References")
    , metadata = iidda.api::ops_staging$metadata(dataset_ids = dataset_ids)
  ) {
  need_iidda_api("browse_pipeline_dependencies")
  dataset_ids = unique(dataset_ids)
  if (length(dataset_ids) > 1L) {
    lapply(dataset_ids, browse_pipeline_dependencies, dependency_types, metadata)
  } else {
    d = do.call(rbind, lapply(
      metadata[[dataset_ids]]$relatedIdentifiers,
      data.frame
    ))
    d = d[d$relatedIdentifierType == "URL",,drop = FALSE]
    d = d[d$relationType %in% dependency_types,,drop = FALSE]
    .trash = lapply(d$relatedIdentifier, browseURL)
    invisible(.trash)
  }
}
