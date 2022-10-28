#' Return the Shortest ICD-10 Codes that Match a Regex Pattern
#'
#' Requires an internet connection
#'
#' @param disease_pattern Regex pattern describing a disease
#' @export
icd_finder = function(disease_pattern) {
  icd_10 = (
    "https://github.com/kamillamagna/ICD-10-CSV/raw/master/categories.csv"
    %>% read.csv(header = FALSE)
    %>% setNames(c("code", "description"))
    %>% filter(grepl(pattern = disease_pattern, description, ignore.case = TRUE))
    %>% filter(nchar(code) == min(nchar(code)))
  )
  return(icd_10)
}
