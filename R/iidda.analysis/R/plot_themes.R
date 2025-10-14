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

#' @export
iidda_theme_availability_heatmap = function(text_size = 9, left_margin = 100, legend_margin = 15) {
  (
      iidda_theme()
    + theme(
          strip.text.y.left = element_text(angle = 0, hjust = 1, size = text_size)
        , axis.ticks.y = element_blank()
        , axis.text.x.bottom = element_text(size = text_size)
        , axis.title.x.bottom = element_text(size = text_size)
        , axis.title.y.left = element_text(margin = margin(l = -left_margin))
        , panel.grid.major.y = element_blank()
        , panel.spacing = unit(0, "lines") # no space between panels is recommended by Ben
        , panel.border = element_rect(fill = NA)
        , strip.background = element_blank()
        , plot.title = element_blank()
        , legend.margin = margin(b = -legend_margin)
        , legend.spacing = unit(0, "pt")
        , legend.box.spacing = unit(0, "pt")
        , legend.text = element_text(size = text_size)
        , legend.title = element_text(size = text_size)
        , legend.justification.top = "left"
        , legend.key.size = unit(1, "line")
      )
  )
}

#' @export
iidda_theme_availability_bars = function(text_size = 9) {
  (
      iidda_theme()
    + theme(
         strip.text.y.left = element_text(angle = 0
            , hjust = 1
            , size = text_size
            , margin = margin()
         )
       , strip.background = element_blank()
       , panel.border = element_blank()
       , panel.spacing = unit(0.1, "lines")
       , axis.ticks.x = element_blank()
       , axis.title.x.top = element_text(size = text_size, hjust = 0)
       , axis.text.x.top = element_blank()
       , axis.ticks.y = element_blank()
       , axis.text.y = element_blank()
       , axis.title.y.left = element_blank()
       , axis.ticks.length.y = unit(0, "pt")
       , panel.grid = element_blank()
       , plot.margin = unit(c(0, 0, 0, 0), "lines")
    )
  )
}
