#' Lightweight Templating
#'
#' Version of the \code{sprintf} base R function that
#' adds basic templating -- https://stackoverflow.com/a/55423080/2047693
#'
#' Because this is based on the sprintf function, use \code{%%} when you
#' would like a single \code{%} to appear in the template. However, when
#' supplying a single \code{%} to a named argument will result in a single
#' \code{%} in the output.
#'
#' You can use syntactically invalid names for arguments by enclosing them
#' in backticks in the argument list, but not in the template.
#'
#' @param template
#' @param ... Named arguments with strings that fill template variables
#' of the same name between \code{%\{} and \code{\}s}
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
  # check if all sub-lists have the same names
  col_nms = (l
             %>% lapply(names)
             %>% unique
  )
  stopifnot(length(col_nms) == 1L)
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
read_tracking_tables = function(path) {
  paths = file.path('tracking', list.files('tracking', pattern = '.csv'))
  (paths
    %>% lapply(read.csv, check.names = FALSE)
    %>% setNames(tools::file_path_sans_ext(basename(paths)))
    %>% lapply(drop_empty_cols)
  )
}

#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate relocate select semi_join left_join right_join
#' @importFrom tibble column_to_rownames
#' @export
get_tracking_metadata = function(product, tracking_path) {
  d = read_tracking_tables(tracking_path)
  metadata = list(
    Product = (d$Transformations
               %>% filter(Product == product)
    ),
    Source = (d$Originals
              %>% filter(Product == product)
              %>% semi_join(x = d$Sources, by = "Source")
    ),
    Originals = (d$Originals
                 %>% filter(Product == product )
                 %>% mutate(Original = basename(`Path to data (original)`))
                 %>% relocate(Original, .before = Source)
    ),
    Tables = (d$Tables
      %>% filter(Product == product)
      %>% remove_rownames
      %>% column_to_rownames("Table")
    ),
    Columns = (d$Tables
      %>% filter(Product == product)
      %>% select(Table)
      %>% right_join(d$Schema, by = "Table")
      %>% left_join(d$Columns, by = "Column")
    )
  )
  metadata$Columns = (metadata$Columns
    %>% split(metadata$Columns$Table)
    %>% lapply(remove_rownames)
    %>% lapply(column_to_rownames, var = "Column")
  )
  metadata$Originals = split(metadata$Originals, metadata$Originals$Original)
  metadata
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
test_result = function(result, metadata) {
  output_file = strip_blob_github(metadata$Product$`Path to tidy data`)
  e = new.env()
  load(output_file, envir = e)
  previous_result = as.list(e)
  if(length(previous_result) != length(result)) {
    stop('number of resulting objects has changed')
  }
  result = result[names(metadata$Tables)]
  previous_result = previous_result[names(metadata$Tables)]
  mapply(compare_columns, result, previous_result)
}

#' @export
schema_check = function(table, metadata) {
  stop('work in progress')
}

#' @export
read_digitized_data = function(metadata) {
  (metadata$Product$`Path to data (digitized)`
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
