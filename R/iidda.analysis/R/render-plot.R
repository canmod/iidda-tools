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
  resolve_plot_values("title", "subtitle")
  (data
    |> get_iidda_attr("plot")
    |> iidda_title(title(data), subtitle(data))
  ) + theme()
}
