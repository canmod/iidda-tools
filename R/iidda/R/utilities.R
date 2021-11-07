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


#' Unique Column Values
#'
#' @param l list of data frames with the same column names
#' @return list of unique values in each column
#' @export
get_unique_col_values = function(l) {
  if(is.recursive(l)) {
    # check if all sub-lists have the same names
    col_nms = (l
               %>% lapply(names)
               %>% unique
    )
    stopifnot(length(col_nms) == 1L)
  } else {
    l = list(l)
  }
  (col_nms[[1]]
    %>% lapply(function(nm) {
      (l
       %>% lapply('[[', nm)
       %>% unlist
       %>% unique
       %>% sort
      )
    })
    %>% setNames(names(l[[1]]))
  )
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
is_empty = function(x) {
  is.na(x) | is.nan(x) | is.null(x) | (nchar(as.character(x)) == 0L) | (tolower(as.character(x)) == 'na')
}

#' @export
drop_empty_cols = function(table) {
  drop_indices = (table
                  %>% lapply(is_empty)
                  %>% sapply(all)
                  %>% which
                  %>% `[`
  )
  if(length(drop_indices) > 0) table = table[-drop_indices]
  return(table)
}

#' @export
drop_empty_rows = function(table) {
  (table
   %>% rowwise
   %>% filter(!all(is_empty(c_across())))
   %>% ungroup
  )
}

#' @export
freq_to_by = function(freq) {
  switch(freq,
         weekly = "7 days",
         `4-weekly` = "28 days",
         monthly = "1 month",
         stop('the frequency, ', frequency,
              ', given in the metadata is not currently an option'))
}

#' @export
freq_to_days = function(freq) {
  switch(freq,
         weekly = 7,
         `4-weekly` = 28,
         monthly = stop('cannot specify monthly patterns in days'),
         stop('the frequency, ', frequency,
              ', given in the metadata is not currently an option'))
}

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
get_with_key = function(l, key, pattern, ...) {
  indices = (l
    %>% get_firsts(key) # keys can only be length-1, so taking first silently
    %>% sapply(grepl, pattern = pattern, ...)
    %>% which
  )
  stopifnot(length(indices) > 0L)
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
  stopifnot(is.recursive(l))
  stopifnot(!any(is_empty(names(l))))
  stopifnot(is.character(named_keys))
  duplicate_names = duplicated(names(named_keys))
  stopifnot(length(duplicate_names) == length(named_keys))
  stopifnot(!any(duplicate_names))
  stopifnot(!any(is.na(named_keys)))
  lapply(named_keys, function(key) l[[key]])
}

#' Set Data Frame Column Types
#'
#' Set the types of the columns of a data frame.
#'
#' @param data data frame
#' @param types dict-like list with keys giving column names and
#' values giving types
#' @return data frame with changed column types -- note that the
#' returned data frame is a plain base R \code{data.frame}
#' (i.e. not a \code{tibble} or \code{data.table}).
#' @export
set_types = function(data, types) {
  (data
    %>% colnames
    %>% sapply(function(nm) {
      as(data[[nm]], types[[nm]])
    }, simplify = FALSE, USE.NAMES = TRUE)
    %>% as.data.frame
  )
}

#' @export
key_val = function(l, key, value) {
  (l
    %>% get_firsts(value)
    %>% setNames(unlist(get_firsts(l, key)))
  )
}

#' @export
or_pattern = function(x) {
  paste0(x, collapse = "|")
}

#' Write Tidy Digitized Data and Metadata
#'
#' @importFrom jsonlite write_json read_json
#' @importFrom dplyr `%>%`
#' @return file names where data were written
#' @export
write_tidy_data = function(tidy_data, metadata) {
  product = metadata$Product$product

  tidy_dir = strip_blob_github(metadata$Product$path_tidy_data)
  if(!dir.exists(tidy_dir)) dir.create(tidy_dir, recursive = TRUE)

  tidy_file = file.path(tidy_dir, product %.% 'csv')
  meta_file = file.path(tidy_dir, product %.% 'json')
  dict_file = file.path(tidy_dir, product %_% 'data_dictionary' %.% 'json')
  dial_file = file.path(tidy_dir, product %_% 'csv_dialect' %.% 'json')
  files = nlist(tidy_file, meta_file, dict_file, dial_file)

  make_data_cite(metadata, meta_file)
  global_dictionary = ('iidda_global_data_dictionary'
    %>% getOption
    %>% blob_to_raw
    %>% read_json
  )
  local_dictionary = (metadata
    %>% getElement('Columns')
    %>% getElement(metadata$Product$product)
    %>% rownames
    %>% or_pattern
    %>% get_with_key(l = global_dictionary, key = 'name')
  )
  write_json(local_dictionary, dict_file, pretty = TRUE, auto_unbox = TRUE)
  .trash = list(
    dialect = list(
      csvddfVersion =  "1.2",
      delimiter = ",",
      lineTerminator = "\r\n",
      quoteChar = "\"",
      doubleQuote = "false",
      nullSequence = "",
      skipInitialSpace = "false",
      header =  'true',
      commentChar = "#",
      caseSensitiveHeader = "true"
    )
  ) %>% write_json(dial_file, pretty = TRUE, auto_unbox = TRUE)

  # this bit is untested -- but it should work
  .trash = ('iidda_global_data_dictionary'
    %>% getOption
    %>% blob_to_raw
    %>% read_json
    %>% key_val('name', 'type')
    %>% get_elements(colnames(tidy_data))
    %>% unlist
    %>% lookup(col_classes_dict)
    %>% set_types(data = tidy_data)
    %>% write.table(tidy_file,
                              # CSV Dialect Translation
      sep = ',',              # delimiter
      eol = '\r\n',           # lineTerminator
      qmethod = 'escape',     # quoteChar="\"", doubleQuote=false
      na = "",                # nullSequence=""
      col.names = TRUE,       # header=true
                              # skipInitialSpace=false
                              # commentChar='#'
                              # caseSensitiveHeader=true
      row.names = FALSE
    )
  )
  return(files)
}

#' Save Results of a Data Prep Script
#'
#' Save the resulting objects of a data prep script into an R data file.
#' The names of the resulting objects are given by the names of the
#' result list.
#'
#' @param result Named list of data resulting from data prep scripts
#' @param metadata Nested named list describing metadata for the result.
#' It must have a `$Product[["Path to tidy data"]]` component, which is
#' a GitHub URL describing the ultimate location of the R data file.
#' The GitHub component of the URL will be removed to produce
#' a path that will correspond to the location within a cloned git
#' repository -- note that the path is relative to the top-level of
#' the cloned repository.
#'
#' @export
save_result = function(result, metadata) {
  output_file = strip_blob_github(metadata$Product$`Path to tidy data`)
  save(list = names(result), file = output_file, envir = list2env(result))
}

#' @export
test_result = function(result) {
  md_nms = grep('_metadata$', names(result), value = TRUE)
  stopifnot(length(md_nms) == 1L)
  metadata = result[[md_nms]]
  table_nms = grep('_metadata$', names(result), value = TRUE, invert = TRUE)
  stopifnot(length(table_nms) > 0L)
  stopifnot(is.recursive(metadata))
  stopifnot(is.character(metadata$Product$`Path to tidy data`))
  output_file = strip_blob_github(metadata$Product$`Path to tidy data`)
  e = new.env()
  load(output_file, envir = e)
  previous_result = as.list(e)
  if(length(previous_result) != length(result)) {
    stop('number of resulting objects has changed')
  }

  if(!isTRUE(all.equal(result[md_nms], previous_result[md_nms]))) {
    stop('metadata have changed in some way')
  }
  result = result[table_nms]
  previous_result = previous_result[table_nms]
  mapply(compare_columns, result, previous_result)
}

#' @export
schema_check = function(table, metadata) {
  stop('work in progress')
}

#' @export
read_digitized_data = function(metadata) {
  (metadata$Product$path_digitized_data
   %>% strip_blob_github
   %>% xlsx_cells
  )
}

#' @export
package_result = function(cleaned_sheets, sheet_dates, metadata) {
  output = list(
    (cleaned_sheets
     %>% bind_rows(.id = "sheet")
     %>% left_join(sheet_dates, by ="sheet")
     %>% select(-sheet)
     %>% relocate(period_end_date, .after = Province)
     %>% relocate(period_start_date, .after = Province)
     %>% as.data.frame
    ),
    metadata
  )
  names(output) = paste0(metadata$Product$Product,
                         c('_reportweek', '_metadata'))
  output
}

#' @export
list_extract = function(x, pattern, ...) {
  x[grepl(pattern, names(x), ...)]
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

# ------------------------------------------------

#' @export
open_locally = function(blob_github_url, command = 'open', args = character()) {
  (blob_github_url
   %>% strip_blob_github
   %>% c(args)
   %>% system2(command = command, stdout = FALSE)
  )
}

#' @export
col_classes_dict = list(
  string = "character",
  integer = "integer",
  number = "numeric",
  factor = "character",
  date = "Date",
  datetime = "POSIXct",
  boolean = "logical"
)
