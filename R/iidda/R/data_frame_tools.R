#' @export
set_iidda_col_types = function(data) {
  if (names(data)[1] == 'Internal Server Error') {
    stop("API Error")
  }
  dict = iidda_data_dictionary()
  allowed_names = dict$name
  if (!all(names(data) %in% allowed_names)) {
    warning(
      "\nthe global iidda data dictionary is out of sync",
      "\nwith one or more iidda datasets. returning all",
      "\ncolumns as strings."
    )
    return(data)
  }

  (dict
    %>% key_val('name', 'type')
    %>% get_elements(colnames(data))
    %>% unlist
    %>% lookup(iidda::col_classes_dict)
    %>% set_types(data = data)
  )
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
#' @importFrom methods as
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

# for some reason there is no character to date as method
setAs('character', 'Date', function(from) as.Date(from))

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

#' Drop Empty Rows
#'
#' Drop empty rows in a table using \code{\link{is_empty}}.
#'
#' @param table data frame
#'
#' @export
drop_empty_rows = function(table) {
  empty_cells = sapply(table, is_empty)
  if (!is.logical(empty_cells) | !is.matrix(empty_cells) | is.null(empty_cells) | !all(dim(table) == dim(empty_cells))) {
    return(table)
  }
  empty_rows = apply(empty_cells, 1, all)
  #empty_cols = apply(empty_cells, 2, all)
  table[!empty_rows, ]
}

# @importFrom dplyr ungroup rowwise filter c_across

# drop_empty_rows = function(table) {
#   keep_fn = function(x) !all(is_empty(as.character(x)))
#   (table
#    %>% rowwise
#    %>% filter(keep_fn(c_across()))
#    %>% ungroup
#   )
# }

#' Write Data Frame
#'
#' Write a data frame to a CSV file using the CSV dialect
#' adopted by IIDDA.
#'
#' @param data to data frame to write
#' @param filename string giving the filename
#' @export
write_data_frame = function(data, filename) {
  data = sapply(data, as.character)
  write.table(data,
    file = filename,
    # CSV Dialect Translation
    sep = ',',              # delimiter
    eol = '\r\n',           # lineTerminator
    qmethod = 'escape',     # quoteChar="\"", doubleQuote=false
    na = '""',              # nullSequence=""
    col.names = TRUE,       # header=true
    # skipInitialSpace=false
    # commentChar='#'
    # caseSensitiveHeader=true
    row.names = FALSE
  )
}
