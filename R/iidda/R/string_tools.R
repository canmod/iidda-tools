#' Lightweight Templating
#'
#' Version of the \code{sprintf} base R function that adds basic templating -- \url{https://stackoverflow.com/a/55423080/2047693}.
#'
#' @details
#'
#' Because this is based on the sprintf function, use \code{\%\%} when you
#' would like a single \code{\%} to appear in the template. However, when
#' supplying a single \code{\%} to a named argument will result in a single
#' \code{\%} in the output.
#'
#' You can use syntactically invalid names for arguments by enclosing them
#' in backticks in the argument list, but not in the template.
#'
#' @param template template
#' @param ... Named arguments with strings that fill template variables
#' of the same name between \%\{ and \}s
#' @param .check Should the consistency between the arguments and the template be checked?
#' @examples
#' sprintf_named("You might like to download datasets from %{repo}s.", repo = "IIDDA")
#' @export
sprintf_named <- function(template, ..., .check = TRUE) {
  args <- list(...)
  argn <- names(args)
  if(.check) {
    names_in_template = extract_all_between_paren(
      template, left = '%\\{', right = '\\}s',
      contents_pattern = '[^%]*')
  } else {
    # if no checks, assume no names in template
    names_in_template = character(0)
  }
  if(is.null(argn)) {
    if(length(names_in_template) != 0L) {
      stop("There are items in the template, but none were specified as arguments.")
    } else {
      return(sprintf(template, ...))
    }
  }
  if(.check) {
    arg_check_pattern = paste0("%\\{", argn, '\\}s')
    if(!all(sapply(arg_check_pattern, grepl, template))) {
      stop("Not all items specified as arguments are in the template.")
    } else if(!all(names_in_template %in% argn)){
      stop("Not all items in the template were specified as arguments.")
    }
  }

  for(i in seq_along(args)) {
    if(argn[i] == "") next;
    template <- gsub(sprintf("%%{%s}", argn[i]),
                     sprintf("%%%d$", i),
                     template, fixed = TRUE)
  }

  do.call(sprintf, append(args, template, 0))
}

#' Extract Substring Between Parentheses
#'
#' Note that unless you specify an appropriate \code{contents_pattern}
#' \code{extract_between_paren} will not work as you probably expect
#' if there are multiple sets of parentheses. You can use exclusion patterns
#' to make this work better (e.g. \code{content_pattern = '[^)]*'}).
#'
#' @param x Character vector
#' @param left Left parenthetical string
#' @param right Right parenthetical string
#' @param contents_pattern Regex pattern for the contents between parentheses
#' @return Character vector with NA's for elements in \code{x} that
#' do not have parentheses and the substring between the first matching
#' parentheses.
#' @examples
#' x = c("-", "", NA, "1", "3", "1 (Alta.)", "(Sask) 20")
#' extract_between_paren(x)
#' @export
extract_between_paren = function(x, left = "\\(", right = "\\)",
                                 contents_pattern = '.*') {

  # -------------------
  # TODO: unfinished
  content_patterns = function(type) {
    switch (type,
            multi = sprintf_named("[^%{left}s]*", left = left)
    )
  }
  # -------------------

  pattern = sprintf_named(
    #"(?<=%{left}s)[^][]*(?=%{right}s)"
    "(?<=%{left}s)%{contents_pattern}s(?=%{right}s)",
    left = left, right = right, contents_pattern = contents_pattern,
    .check = FALSE) # checking creates circular call graph
  (x
    %>% regexec(pattern = pattern, perl = getOption('iidda_perl'))
    %>% regmatches(x = x)
    %>% vapply(function(x) {
      stopifnot(is.character(x))
      if(length(x) == 0L) return(as.character(NA))
      else return(x[1])
    }, character(1L))
  )
}

#' Remove Parenthesized Substring
#'
#' @param x Character vector
#' @param left Left parenthetical string
#' @param right Right parenthetical string
#' @return Version of \code{x} with first parenthesized substrings removed
#' @examples
#' x = c("-", "", NA, "1", "3", "1 (Alta.)", "(Sask) 20")
#' remove_between_paren(x)
#' @export
remove_between_paren = function(x, left = "\\(", right = "\\)",
                                contents_pattern = '.*') {
  pattern = sprintf_named(
    "%{left}s%{contents_pattern}s%{right}s",
    left = left, right = right,
    contents_pattern = contents_pattern,
    .check = FALSE) # checking creates circular call-graph
  sub(pattern, '', x, perl = getOption('iidda_perl'))
}

#' @rdname extract_between_paren
#' @param max_iters maximum number of items to return
#' @export
extract_all_between_paren = function(x, left = "\\(", right = "\\)",
                                     contents_pattern = '.*',
                                     max_iters = 100) {
  stopifnot(length(x) == 1L)
  output = c()
  for(i in 1:max_iters) {
    output = append(output, extract_between_paren(x, left, right, contents_pattern))
    y = remove_between_paren(x, left, right, contents_pattern)
    if(x == y) {break}
    else {x = y}
  }
  return(output[!is.na(output)])
}

#' Vectorized String Substitution
#'
#' @param pattern,replacement,x first three arguments to \code{sub},
#' but the first is allowed to be a vector
#' @param ... additional arguments to pass on to \code{sub}
#' @export
vsub = function(pattern, replacement, x, ...) {
  f = Vectorize(sub, vectorize.args = 'pattern')
  stopifnot(length(pattern) == length(x))
  non_empty = !is_empty(pattern)
  if(length(replacement) != 1L) {
    stopifnot(length(replacement) == length(pattern))
    replacement = replacement[non_empty]
  }
  x[non_empty] = f(pattern[non_empty], replacement, x[non_empty], ...)
  x
}

#' Remove Trailing Slash
#'
#' @param x Character vector with paths
#' @return Character vector without trailing slash
#' @export
rm_trailing_slash = function(x) sub('/$', '', x)

#' @export
rm_leading_slash = function(x) sub('^/', '', x)

#' @export
or_pattern = function(x) {
  paste0(x, collapse = "|")
}

#' Simplify String with List of Numbers Grouped by Dashes
#'
#' @param x atomic vector
#' @return length-1 character string giving a sorted list of numbers
#' with contiguous numbers grouped by dashes.
#' @examples
#' group_with_dash(c("3840", "34", "2", "3", "1", "33", '5-50'))
#' group_with_dash(group_with_dash(c("3840", "34", "2", "3", "1", "33", '5-50')))
#' @importFrom dplyr between
#' @export
group_with_dash = function(x) {
}

# ------------------------------------------------
# copied from MacPan TMB branch

#' Paste Operators
#'
#' Syntactic sugar for common string pasting operations.
#'
#' \describe{
#'   \item{\code{\%+\%}}{Paste with a blank separator, like python string concatenation}
#'   \item{\code{\%_\%}}{Paste with underscore separator}
#'   \item{\code{\%.\%}}{Paste with dot separator -- useful for adding file extensions}
#'   \item{\code{\%-\%}}{Paste with dash separator -- useful for representing contiguous numbers}
#' }
#'
#' @param x character vector
#' @param y character vector
#' @return \code{x} concatenated with \code{y}
#' @examples
#' 'google' %.% 'com'
#' 'snake' %_% 'case'
#' @name paste_operators
NULL

#' @rdname paste_operators
#' @export
`%_%` = function(x, y) paste(x, y, sep = "_")

#' @rdname paste_operators
#' @export
`%+%` = function(x, y) paste(x, y, sep = "")

#' @rdname paste_operators
#' @export
`%.%` = function(x, y) paste(x, y, sep = '.')

#' @rdname paste_operators
#' @export
`%-%` = function(x, y) paste(x, y, sep = '-')


# ------------------------------------------------
# paste-collapse summaries

#' @export
summarise_strings = function(x, sep = ", ") {
  (x
   %>% unique
   %>% strsplit(',|;')
   %>% unlist(use.names = FALSE)
   %>% trimws
   %>% paste0(collapse = sep)
  )
}

#' @export
summarise_integers = function(x, range_operator = "-", collapse = TRUE) {
  x = (x
       %>% as.character
       %>% strsplit(',|;')
       %>% unlist(use.names = FALSE)
       %>% trimws
       %>% unique
  )
  range_regex = "(\\-?\\d+)" %+% range_operator %+% "(\\-?\\d+)"
  o = (x
       %>% strsplit(range_operator)
       %>% lapply(getElement, 1L)
       %>% unlist
       %>% trimws
       %>% as.integer
       %>% order
  )
  x = x[o]
  y = x[1]
  for(z in x[-1]) {
    ly = length(y)
    that_head = sub(range_regex, '\\1', y[ly]) %>% as.integer
    this_head = sub(range_regex, '\\1', z) %>% as.integer
    stopifnot(that_head <= this_head)
    that_tail = sub(range_regex, '\\2', y[ly]) %>% as.integer
    this_tail = sub(range_regex, '\\2', z) %>% as.integer
    if(between(that_tail, this_head - 1, this_tail)) {
      y[ly] = that_head %+% range_operator %+% this_tail
    } else if(this_head > that_tail + 1) {
      y = append(y, z)
    }
  }
  if(collapse) y = paste0(y, collapse = ", ")
  y
}

#' @export
summarise_dates = function(x_start, x_end, range_operator = " to ", collapse = TRUE) {
  x_start = (x_start
   %>% as.Date
   %>% as.integer
  )
  x_end = (x_end
    %>% as.Date
    %>% as.integer
  )
  x_integer = summarise_integers(
    (x_start %+% range_operator %+% x_end),
    range_operator = range_operator, collapse = FALSE)

  range_regex = "(\\-?\\d+)" %+% range_operator %+% "(\\-?\\d+)"

  y = (
    as.character(as.Date(as.integer(sub(range_regex, '\\1', x_integer)), origin = "1970-01-01")) %+%
    " to " %+%
    as.character(as.Date(as.integer(sub(range_regex, '\\2', x_integer)), origin = "1970-01-01"))
  )
  if(collapse) y = paste0(y, collapse = ', ')
  y
}
