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

#' @importFrom dplyr ungroup rowwise filter c_across
#' @export
drop_empty_rows = function(table) {
  (table
   %>% rowwise
   %>% filter(!all(is_empty(c_across())))
   %>% ungroup
  )
}
