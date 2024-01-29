#' Themes for ggplot2
#'
#' @importFrom ggplot2 theme_bw theme element_blank
#' @export
iidda_theme = function() theme_bw()

#' @describeIn iidda_theme Theme for plots where the x-axis represents time.
#' No x-axis titles will be plotted with this theme, because the meaning of a
#' time axis is obvious.
iidda_theme_time = function() {
  (
      iidda_theme()
    + theme(axis.title.x = element_blank())
  )
}

#' @describeIn iidda_theme Theme for heatmaps where the x-axis represents time.
#' No x-axis titles will be plotted with this theme, because the
#' meaning of a time axis is obvious. Grid lines are not plotted with this
#' theme because interpretation can be compromised when grid lines
#' are visible through the colours of the heatmap.
#' @export
iidda_theme_heat = function() {
  (
      iidda_theme_time()
    + theme(panel.grid = element_blank())
  )
}

#' @describeIn iidda_theme Theme for plots where the x-axis represents time,
#' but for which time information is not displayed because there are vertically
#' aligned plots below with the same time axis.
#' @export
iidda_theme_above = function() {
  (
      iidda_theme_time()
    + theme(
        axis.title.x = element_blank()
      , axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
    )
  )
}
