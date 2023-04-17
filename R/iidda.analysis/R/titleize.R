#' Titleize
#'
#' Convert a character vector (i.e. a character column) into a title for
#' a plot.
#'
#' @param title_info Character vector to be summarized into a title
#' @param max_items TODO
#' @param max_chars TODO
#'
#' @export
titleize = function(title_info, max_items = 3L, max_chars = 15L) {
  title_info = sort(unique(title_info))
  n_items = length(title_info)
  n_to_keep = min(max_items, n_items)
  title = abbreviate(title_info[1:n_to_keep], minlength = max_chars)
  if (n_items != n_to_keep) title = c(title, "...")
  paste0(title, collapse = ", ")
}
