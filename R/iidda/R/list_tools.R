#' @export
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

#' @export
get_items = function(l, keys) {
  lapply(l ,`[`, keys)
}


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

#' @export
key_val = function(l, key, value) {
  (l
   %>% get_firsts(value)
   %>% setNames(unlist(get_firsts(l, key)))
  )
}

#' @export
list_extract = function(x, pattern, ...) {
  x[grepl(pattern, names(x), ...)]
}

##' self-naming list (copied from lme4:::namedList)
##' @param ... a list of objects
##' @export
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

#' @export
extract_or_blank = function(l, e) {
  if (!is.recursive(l)) return(list())
  le = l[[e]]
  if (is.null(le)) return(list())
  le
}

#' @export
extract_char_or_blank = function(l, e) {
  if (!is.recursive(l)) return("")
  le = l[[e]]
  if (is.null(le)) return("")
  as.character(le)
}

#' @export
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
