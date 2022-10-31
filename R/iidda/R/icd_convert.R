#' ICD Finder
#'
#' Return the Shortest ICD-10 Codes that Match a Regex Pattern.
#' Requires an internet connection.
#'
#' @param disease_pattern Regex pattern describing a disease.
#' @param maximum_number_results Integer giving the maximum number
#' of ICD codes to return, with preference given to shorter codes.
#' @param ... Arguments to pass on to \code{\link{grepl}}.
#' It is recommended to set \code{ignore.case = TRUE} and often
#' \code{perl = TRUE}.
#' @examples
#' icd_finder("chick")  ## Struc by chicken!!
#' @importFrom utils head
#' @export
icd_finder = function(
    disease_pattern,
    maximum_number_results = 10L,
    ...
  ) {
  icd_10 = (
    "https://github.com/kamillamagna/ICD-10-CSV/raw/master/categories.csv"
    %>% read.csv(header = FALSE)
    %>% setNames(c("code", "description"))
    %>% filter(grepl(
      pattern = disease_pattern,
      description,
      ...
    ))
    %>% arrange(nchar(code))
    %>% head(maximum_number_results)
    #%>% filter(nchar(code) == min(nchar(code)))
  )
  return(icd_10)
}
