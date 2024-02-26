#' Missing Handlers
#'
#' Construct an object with functions for handling missing values.
#'
#' @param unclear Character vector giving values corresponding to numbers
#' that were unclear to data enterers.
#' @param not_reported Character vector giving values corresponding to
#' numbers that were not reported in the original source.
#' @param zeros Character vector giving values corresponding to
#' `0` but that were entered as another character to resemble the original
#' source.
#'
#' @returns An environment with functions for handling missing values.
#' @export
MissingHandlers = function(
      unclear = c("Unclear", "unclear", "uncleaar", "uncelar", "r")
    , not_reported = c("", "Not available", "*", "Not reportable")
    , zeros = c("\u2014", "\u23BB", "\u002D", "\u203E", "\u005F", "\u2013")
   #, zeros = c("—"     , "⎻"     , "-"     , "‾"     , "_"     , "–"     )
) {
  unclear_pattern = function(template = "\\s*\\((%s)\\)\\s*") {
    sprintf(template, paste0(unclear, collapse = "|"))
  }
  get_unclear_guesses = function(x) {
    r = unclear_pattern()
    sub(r, "", x) |> sub(pattern = "^([0-9]+)", replacement = "\\1")
  }
  rm_backticks = function(x) {
    gsub("`", "", x, fixed = TRUE)
  }
  get_zeros = function(x) {
    r = sprintf("^(\\s*%s\\s*)$", paste0(zeros, collapse = "|"))
    sub(r, "0", x)
  }
  get_if_starts_with_number = function(x) {
    sub("^([0-9]+)([^0-9]+)$", "\\1", x)
  }
  get_hidden_numbers = function(x) {
    (x
     |> get_zeros()
     |> get_unclear_guesses()
     |> get_if_starts_with_number()
    )
  }
  is_reported = function(x) {
    !(x %in% not_reported)
  }
  environment()
}
