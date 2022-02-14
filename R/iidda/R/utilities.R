#' @export
is_empty = function(x) {
  is.na(x) | is.nan(x) | is.null(x) | (nchar(as.character(x)) == 0L) | (tolower(as.character(x)) == 'na')
}

#' @export
empty_is_blank = function(x) {
   ifelse(is_empty(x), '', as.character(x))
}

#' Open a Path
#'
#' @export
open_locally = function(blob_github_url, command = 'open', args = character()) {
  (blob_github_url
   %>% strip_blob_github
   %>% c(args)
   %>% system2(command = command, stdout = FALSE)
  )
}
