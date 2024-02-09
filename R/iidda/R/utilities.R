#' Is Empty
#'
#' Return \code{\link{TRUE}} if a string is empty. Emptiness means that
#' any one of the following is true: \code{NA}, \code{NaN},
#' \code{nchar(as.character(x)) == 0L},
#' \code{tolower(as.character(x)) == "na"}
#'
#' @param x object to test
#'
#' @export
is_empty = function(x) {
  is.na(x) | is.nan(x) | is.null(x) | (nchar(as.character(x)) == 0L) | (tolower(as.character(x)) == 'na')
}

#' Empty is Blank
#'
#' Force empty strings to be blank. See \code{\link{is_empty}}.
#'
#' @inheritParams is_empty
#' @export
empty_is_blank = function(x) {
   ifelse(is_empty(x), '', as.character(x))
}

#' Open a Path on Mac OS or Windows
#'
#' @inheritParams strip_blob_github
#' @param command Command-line function to use to open the file (not
#' applicable on Windows systems.
#' @param args Additional options to pass to \code{command} (ignored on
#' Windows systems).
#'
#' @export
open_locally = function(urls, command = 'open', args = character()) {
  paths = strip_blob_github(urls)
  sys_name = Sys.info()["sysname"]
  if (sys_name == "Windows") {
    for (path in paths) shell.exec(path)
  } else if (sys_name == "Darwin") {
    system2(command = c(command, paths, args), stdout = FALSE)
  } else {
    stop("Currently only works on Windows and MacOS.")
  }
}
