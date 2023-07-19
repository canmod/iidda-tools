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

#' @export
two_field_format = function(dataset) {
  (dataset
   %>% mutate(nesting_disease = "")
   %>% mutate(nesting_disease = ifelse(disease_subclass != "", disease, nesting_disease))
   %>% mutate(disease = ifelse(disease_subclass != "", disease_subclass, disease))
   %>% mutate(nesting_disease = ifelse(disease_subclass == "" & disease_family != "", disease_family, nesting_disease))
   %>% relocate(nesting_disease, .after = period_end_date)
   %>% select(-disease_subclass)
   %>% select(-disease_family)
  )
}
