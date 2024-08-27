#' Set IIDDA Column Types
#'
#' Deprecated -- iidda.api package is not more robust.
#'
#' @param data Dataset from IIDDA Api
#'
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
    %>% lookup(col_classes_dict)
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
#' @param data A data frame to write
#' @param filename string giving the filename
#' @export
write_data_frame = function(data, filename) {

  characterize = function(data) {
    data = sapply(data, as.character)
    if (!is.matrix(data)) data = t(data)
    return(data)
  }
  ## convert the data frame to a character matrix without line feeds or
  ## unnecessary whitespace
  if (nrow(data) > 0L) {
    data = (data
      |> characterize()
      |> gsub(pattern = "\\s+", replacement = " ")
      |> trimws()
    )
  } else {
    for (cc in names(data)) {
      data[[cc]] = as.character(data[[cc]])
    }
  }
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


#' Read Data Frame
#'
#' Read in a data frame from a CSV file using the CSV dialect
#' adopted by IIDDA.
#'
#' @param filename String giving the filename.
#' @param col_classes See \code{colClasses} from \code{\link{read.table}}.
#' @export
read_data_frame = function(filename, col_classes = "character") {
  read.table(filename
        # CSV Dialect Translation
      , header = TRUE           # header=true
      , sep = ','               # delimiter
      , quote = "\""            # quoteChar="\""
      , comment.char='#'        # commentChar='#'
      , na.strings = '""'         # nullSequence=""
      , colClasses = col_classes
  )
}

#' Read Data Columns
#'
#' @param filename Path to a CSV file in IIDDA format.
#'
#' @export
read_data_columns = function(filename) {
  read.table(filename
        # CSV Dialect Translation
      , header = TRUE           # header=true
      , sep = ','               # delimiter
      , quote = "\""            # quoteChar="\""
      , comment.char='#'        # commentChar='#'
      , na.strings = '""'       # nullSequence=""
      , colClasses = "character"
      , nrows = 1
  ) |> names()
}

#' Fix CSV
#'
#' Fix the format of a CSV file that is not in IIDDA format.
#' @param filename Path to the CSV file
#'
#' @returns Logical value that is `TRUE` if the CSV needed fixing
#' and `FALSE` otherwise.
#' @importFrom readr read_csv
#' @export
fix_csv = function(filename) {
  message("Checking ", filename)
  tmp_file = tempfile(fileext = ".csv")
  initial_guess = readr::read_csv(filename, col_types = "c")
  write_data_frame(initial_guess, tmp_file)
  best_guess = read_data_frame(tmp_file)
  current_read = try(read_data_frame(filename), silent = TRUE)
  need_to_fix = !identical(best_guess, current_read)
  if (need_to_fix) {
    file.copy(tmp_file, filename, overwrite = TRUE)
    message("CSV is fixed! Please check to make sure that your expectations are met.")
  } else {
    message("CSV file did not need fixing")
  }
  return(need_to_fix)
}
