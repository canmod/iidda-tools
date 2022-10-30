#' ICD Finder
#'
#' Return the Shortest ICD-10 Codes that Match a Regex Pattern.
#' Requires an internet connection.
#'
#' @param disease_pattern Regex pattern describing a disease.
#' @param maximum_number_results Integer giving the maximum number
#' of ICD codes to return, with preference given to shorter codes.
#' @examples
#' icd_finder("chick")  ## Struc by chicken!!
#' @importFrom utils head
#' @export
icd_finder = function(disease_pattern, maximum_number_results = 10L) {
  icd_10 = (
    "https://github.com/kamillamagna/ICD-10-CSV/raw/master/categories.csv"
    %>% read.csv(header = FALSE)
    %>% setNames(c("code", "description"))
    %>% filter(grepl(
      pattern = disease_pattern,
      description,
      ignore.case = TRUE
    ))
    %>% arrange(nchar(code))
    %>% head(maximum_number_results)
    #%>% filter(nchar(code) == min(nchar(code)))
  )
  return(icd_10)
}
