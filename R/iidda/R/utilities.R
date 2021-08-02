#' Remove Trailing Slash
#'
#' @param x Character vector with paths
#' @return Character vector without trailing slash
#' @export
rm_trailing_slash = function(x) sub('/$', '', x)


#' Extract Value Between Parentheses
#'
#' @param x Character vector
#' @param left Left parenthetical string
#' @param right Right parenthetical string
#' @return Character vector with NA's for elements in \code{x} that
#' do not have parentheses and the substring between the first matching
#' parentheses.
#' @export
extract_between_paren = function(x, left = "\\(", right = "\\)") {
  pattern = sprintf_named(
    "(?<=%{left}s).*(?=%{right}s)",
    left = left, right = right)
  (x
    %>% regexec(pattern = pattern, perl = TRUE)
    %>% regmatches(x = x)
    %>% vapply(function(x) {
      stopifnot(is.character(x))
      if(length(x) == 0L) return(as.character(NA))
      else return(x[1])
    }, character(1L))
  )
}


#' Lightweight Templating
#'
#' https://stackoverflow.com/a/55423080/2047693
#'
#' @param template
#' @param ... Named arguments with strings that fill template variables
#' of the same name between \code{%\{} and \code{\}s}
#' @examples
#' sprintf_named("You might like to download datasets from %{repo}s.", repo = "IIDDA")
#' @export
sprintf_named <- function(template, ...) {
  args <- list(...)
  argn <- names(args)
  if(is.null(argn)) return(sprintf(template, ...))

  for(i in seq_along(args)) {
    if(argn[i] == "") next;
    template <- gsub(sprintf("%%{%s}", argn[i]),
                     sprintf("%%%d$", i),
                     template, fixed = TRUE)
  }

  do.call(sprintf, append(args, template, 0))
}
