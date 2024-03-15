
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

#' Two Field Format
#'
#' Attempt to automatically convert a dataset from `disease{|_subclass|_family}`
#' format of disease ID to the `{|nesting_}disease` format.
#'
#' @param dataset A tidy data set with `disease{|_subclass|_family}` columns.
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


find_overlap = function(lower, upper) {
  UseMethod("find_overlap")
}

no_overlaps = function(lower, upper) {
  i = order(lower)
  n = length(lower)

  for (j in 2:n) {
    consistent_bin_bounds = lower[i][j] <= upper[i][j]
    consistent_bin_neighbours = lower[i][j] > upper[i][j - 1L]
    if (!consistent_bin_neighbours | !consistent_bin_bounds) return(FALSE)
  }
  TRUE
}


