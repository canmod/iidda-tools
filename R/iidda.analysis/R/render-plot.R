#' Render a Plot
#'
#' Extracts a plot object attached to a data set, applies a title, subtitle,
#' and theme, and returns the plot.
#'
#' @param data A data frame with an attached plot object.
#' @param title A plot title, or a function that takes `data` and returns a
#' title string.
#' @param subtitle A plot subtitle, or a function that takes `data` and returns
#' a subtitle string.
#' @param theme A function that returns a `ggplot2::theme` object.
#'
#' @export
iidda_render_plot = function(data
    , title = TitleGuesser()
    , subtitle = TimeRangeDescriber()
    , theme = iidda_theme ## TODO: Themer()??
  ) {
  resolve_choosers("title", "subtitle")
  (data
    |> get_iidda_attr("plot")
    |> iidda_title(title(data), subtitle(data))
  ) + theme()
}



#' @export
harmonize_plots = function(plots) {
  grobs = lapply(plots, as_grob)
  widths = lapply(grobs, getElement, "widths")
  n_widths = vapply(widths, length, integer(1L)) |> unique()
  if (length(n_widths) != 1L) {
    stop(
        "not comparable plots because the number "
      , "of plot elements is different"
    )
  }
  max_widths = lapply(
        seq_len(n_widths)
      , \(i) {
            w = lapply(widths, getElement, i)
            g = TRUE
            for (j in seq_along(w)[-1]) g = g & identical(w[[j]], w[[j - 1]])
            if (g) w = w[[1L]] else w = Reduce(max, w)
            return(w)
        }
  )
  focal_widths = widths[[1L]]
  for (i in seq_along(focal_widths)) focal_widths[[i]] = max_widths[[i]]
  for (i in seq_along(grobs)) grobs[[i]]$widths = focal_widths
  return(grobs)
}
