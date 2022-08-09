#' @export
set_iidda_col_types = function(data) {
  if (names(data)[1] == 'Internal Server Error') {
    stop("API Error")
  }
  dict = iidda_data_dictionary()
  allowed_names = iidda::list_xpath(dict, 'name') %>% unlist
  if (!all(names(data) %in% allowed_names)) {
    warning(
      "\nthe global iidda data dictionary is out of sync",
      "\nwith one or more iidda datasets. returning all",
      "\ncolumns as strings."
    )
    return(data)
  }

  (dict
    %>% iidda::key_val('name', 'type')
    %>% get_elements(colnames(data))
    %>% unlist
    %>% iidda::lookup(iidda::col_classes_dict)
    %>% iidda::set_types(data = data)
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
