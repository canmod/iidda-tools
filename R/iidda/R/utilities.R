#' @export
is_empty = function(x) {
  is.na(x) | is.nan(x) | is.null(x) | (nchar(as.character(x)) == 0L) | (tolower(as.character(x)) == 'na')
}

#' Open a Path
#'
#' @export
open_blob_mac = function(blob_github_url, command = 'open', args = character()) {
  (blob_github_url
   %>% strip_blob_github
   %>% c(args)
   %>% system2(command = command, stdout = FALSE)
  )
}
