#
# ll = lookup_to_synonym_list(
#   read.csv(system.file('locations_ISO.csv', package = "iidda")),
#   "iso_3166", "location", "iso_3166_2"
# )

#' Normalize Diseases
#'
#' Normalize the names of diseases to simplify the
#' harmonization of disease names across historical
#' sources.
#'
#' @param diseases Character vector of disease names
#'
#' @export
normalize_diseases = function(diseases) {
  (diseases
   %>% as.character
   %>% trimws
   %>% tolower
   %>% gsub(pattern = "[*]", replacement = "")
  )
}
