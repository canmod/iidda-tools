#' Pager
#'
#' @param page What page should be returned?
#' @param n_per_page How many entries on each page?
#' @param rev Should page one be at the end?
#' @return Function of `x` to return the `page`th `page` of size `n_per_page`
#' of `x`.
#' @export
pager = function(page, n_per_page, rev = TRUE) function(x) {
  if (rev) {
    i = seq(
        from = length(x) - (page * n_per_page) + 1
      , to = length(x) - ((page - 1) * n_per_page)
      , by = 1
    )
  } else {
    i = seq(
        from = 1 + ((page - 1) * n_per_page)
      , to = page * n_per_page
      , by = 1
    )
  }
  i = i[i > 0]
  i = i[i <= length(x)]
  x[i]
}
#pager(2, 10, FALSE)(letters)
