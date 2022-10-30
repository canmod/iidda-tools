#' Get Firsts
#'
#' Get the first item in each sublist of sublists (ugh ... I know).
#'
#' @param l A list of lists of lists
#' @param key Name of focal sublist (TODO: needs better description/motivation)
#'
#' @export
#' @examples
#' l = list(
#'   a = list(
#'     A = list(
#'       i = 1,
#'       ii = 2
#'     ),
#'     B = list(
#'       i = 3,
#'       ii = 4
#'     )
#'  ),
#'  b = list(
#'     A = list(
#'       i = 5,
#'       ii = 6
#'     ),
#'     B = list(
#'       i = 7,
#'       ii = 8
#'     )
#'   )
#' )
#' get_firsts(l, "A")
#' get_firsts(l, "B")
get_firsts = function(l, key) {
  (l
   %>% lapply(getElement, key)
   %>% lapply(getElement, 1L)
  )
}

#' Get with Key by Regex
#'
#' @param l list of lists
#' @param key name of item in inner list
#' @param pattern regex pattern with which to match values of the key
#' @param ... additional arguments to pass to \code{\link{grepl}}
#' @return subset of elements of \code{l} that match the pattern
#' @export
get_with_key = function(l, key, pattern, ...) {
  indices = (l
     %>% get_firsts(key) # keys can only be length-1, so taking first silently
     %>% sapply(grepl, pattern = pattern, ...)
     %>% which
  )
  #stopifnot(length(indices) > 0L)
  l[indices]
}

#' Get Items
#'
#' Get list of items within each inner list of a list of lists
#'
#' @param l A list of lists.
#' @param keys Name of the items in the inner lists.
#'
#' @export
get_items = function(l, keys) {
  lapply(l ,`[`, keys)
}

#' Get Elements
#'
#' Synonym for the \code{`[`} operator for use in pipelines.
#'
#' @export
get_elements = `[`

#' Lookup Value
#'
#' @param named_keys named character vector with values giving keys
#' to lookup in \code{l}
#' @param l list with names to match against the
#' values of \code{keys}
#' @export
lookup = function(named_keys, l) {
  if (is.null(names(named_keys))) named_keys = setNames(named_keys, named_keys)
  stopifnot(is.recursive(l))
  stopifnot(!any(is_empty(names(l))))
  stopifnot(is.character(named_keys))
  duplicate_names = duplicated(names(named_keys))
  stopifnot(length(duplicate_names) == length(named_keys))
  stopifnot(!any(duplicate_names))
  stopifnot(!any(is.na(named_keys)))
  lapply(named_keys, function(key) l[[key]])
}

#' Key-Value
#'
#' Create a set of key-value pairs by extracting elements from
#' within a list of named-lists.
#'
#' @param l A list of named lists
#' @param key A name of an element in each list in \code{l}
#' @param value A name of an element in each list in \code{l}
#'
#' @export
#' @examples
#' f = system.file("example_data_dictionary.json", package = "iidda")
#' d = jsonlite::read_json(f)
#' key_val(d, "name", "type")
key_val = function(l, key, value) {
  (l
   %>% get_firsts(value)
   %>% setNames(unlist(get_firsts(l, key)))
  )
}

#' List Extract
#'
#' Extract list items by regular expression matching
#' on their names.
#'
#' @param x A list.
#' @param pattern A regular expression
#' @param ... Arguments to pass to \code{\link{grepl}}
#'
#' @export
list_extract = function(x, pattern, ...) {
  x[grepl(pattern, names(x), ...)]
}

#' Self-Naming List
#'
#' Copied from \code{lme4:::namedList}.
#'
#' @param ... a list of objects
#' @export
nlist <- function(...) {
  L <- list(...)
  snm <- vapply(substitute(list(...)), deparse, character(1))[-1]
  if (is.null(nm <- names(L))) {
    nm <- snm
  }
  if (any(nonames <- nm == "")) {
    nm[nonames] <- snm[nonames]
  }
  setNames(L, nm)
}

#' Extract or Blank
#'
#' Try to extract a list element, and return a blank
#' list if it doesn't exist or if a proper list is not passed.
#'
#' @param l List
#' @param e Name of the focal element
#' @export
extract_or_blank = function(l, e) {
  if (!is.recursive(l)) return(list())
  le = l[[e]]
  if (is.null(le)) return(list())
  le
}

#' Extract Character or Blank
#'
#' Extract a character vector from a list or return
#' a blank string if it doesn't exist or if a proper
#' list isn't passed.
#' @inheritParams extract_or_blank
#'
#' @export
extract_char_or_blank = function(l, e) {
  if (!is.recursive(l)) return("")
  le = l[[e]]
  if (is.null(le)) return("")
  as.character(le)
}

#' List XPath
#'
#' Extract elements of lists using x-path-like syntax.
#'
#' @param l A hierarchical list.
#' @param ... Character strings describing the path down the hierarchy.
#'
#' @export
#' @examples
#' l = list(
#'   a = list(
#'     A = list(
#'       i = 1,
#'       ii = 2
#'     ),
#'     B = list(
#'       i = 3,
#'       ii = 4
#'     )
#'  ),
#'  b = list(
#'     A = list(
#'       i = 5,
#'       ii = 6
#'     ),
#'     B = list(
#'       i = 7,
#'       ii = 8
#'     )
#'   )
#' )
#' list_xpath(l, "A", "i")
#' list_xpath(l, "B", "ii")
list_xpath = function(l, ...) {
  path_names = list(...)
  for (i in seq_along(path_names)) {
    nm = path_names[[i]]
    l = lapply(l, extract_or_blank, nm)
  }
  l
}

#' Unlist a List of Character Vectors
#'
#' Replacing list elements with \code{list('')}
#' for each element that is null, not a character
#' vector, or length zero.
#'
#' @param x list of character vectors
#' @export
unlist_char_list = function(x) {
  repl_fn = function(y, j) {
    j = which(j)
    y[j] = rep(list(''), length(j))
    y
  }
  x = repl_fn(x, sapply(x, is.null))
  x = repl_fn(x, !sapply(x, is.character))
  x = repl_fn(x, sapply(x, length) == 0L)
  unlist(x, recursive = FALSE, use.names = FALSE)
}
