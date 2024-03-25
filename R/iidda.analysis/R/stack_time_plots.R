#' Tag Reference
#'
#' Identify an object in a group of objects to be used to
#' determine aspects of the group.
#'
#' @param x an object in a group
#' @return the input object with the attribute
#' \code{'iidda_time_plot_reference'} equal to \code{TRUE}
#'
#' @export
ref = function(x) {
  attr(x, 'iidda_time_plot_reference') = TRUE
  x
}


#' @export
is_ref = function(x) {
  isTRUE(attr(x, 'iidda_time_plot_reference'))
}

#' Shared Time Limits
#'
#' Get limits of a time-axis that is shared by several
#' objects.
#'
#' @param ... list of objects
#' @param time_id identifier for finding time axis information
#' in each object
#' @return Date vector of the limits of a shared time axis
#' @export
shared_time_limits = function(..., time_id) {
  UseMethod("shared_time_limits")
}

#' @export
shared_time_limits.ggplot = function(..., time_id) {
  plts = list(...)
  (plts
    %>% list_xpath("data", time_id)
    %>% lapply(range)
    %>% Reduce(f = c)
    %>% range
  )
}


#' Time Extent
#'
#' Length of time in days representated by an object
#'
#' @param x an object
#' @param time_id identifier for finding time axis information
#' in the object
time_extent = function(x, time_id) {
  UseMethod("time_extent")
}


#' @export
time_extent_date = function(x) {
  args = c(as.list(rev(range(x))), list(units = 'days'))
  dif = do.call(difftime, args)
  as.integer(dif)
}

#' @export
time_extent.data.frame = function(x, time_id) {
  time_extent_date(x[[time_id]])
}

#' @export
time_extent.ggplot = function(x, time_id) {
  time_extent(x$data, time_id)
}

#' Time Harmonize
#'
#' Harmonize the time-axis over a group of objects
#'
#' @param ... list of objects
#' @param time_id identifier for finding time axis information
#' in each object
#' @return modified list of objects
#' @export
time_harmonize = function(..., time_id) {
  UseMethod("time_harmonize")
}

#' @export
time_harmonize.ggplot = function(..., time_id) {
  plts = ref_plts = list(...)
  which_ref = (plts
    %>% lapply(is_ref)
    %>% unlist
    %>% which
  )
  if (length(which_ref) != 0L) ref_plts = plts[which_ref]
  args = c(ref_plts, list(time_id = time_id))
  ref_range = do.call(shared_time_limits, args)
  for(i in seq_along(plts)) {
    plts[[i]] = plts[[i]] + scale_x_date(limits = ref_range)
  }
  plts
}


#' @export
time_harmonize_list = function(l, time_id) {
  do.call(time_harmonize, c(l, list(time_id = time_id)))
}

# avoiding cowplot and gtable dependencies --------
# https://github.com/wilkelab/cowplot/blob/master/R/get_plot_component.R



#' @export
to_gtable = function(x) {
  UseMethod('to_gtable')
}

#' @export
to_gtable.ggplot = function(x) {
  ggplotGrob(x)
}

#' @export
to_gtable.gtable = function(x) x

# to_gtable.grob = function(x) {
#   layout <- data.frame(
#     t = 1, l = 1, b = 1, r = 1, z = 1,
#     clip = 'off', name = x$name
#   )
#   gTree(
#     grobs = list(x), layout = layout, widths = unit(1, "npc"),
#     heights = unit(1, "npc"), #respect = respect, name = name,
#     #rownames = rownames, colnames = colnames, vp = vp,
#     cl = "gtable"
#   )
#
# }

#' Grob Utilities
#'
#' @param x TODO
#' @export
grob_names = function(x) {
  to_gtable(x)$layout$name
}

#' Which Grobs
#'
#' @param grob_nms names of grobs
#' @export
which_grobs = function(x, grob_nms) {
  which(grob_names(x) %in% grob_nms)
}

#' @export
which_grobs_not = function(x, grob_nms) {
  which(!grob_names(x) %in% grob_nms)
}

#' @export
get_grobs = function(x, grob_nms) {
  to_gtable(x)$grob[which_grobs(x, grob_nms)]
}

#' param grob_nm name of grob to get
#' @export
get_grob = function(x, grob_nm) {
  stopifnot(length(grob_nm) == 1L)
  out_grob = get_grobs(x, grob_nm)
  if (length(out_grob) == 0L) stop("could not find grob")
  if (length(out_grob) > 1L) stop("too many grobs of that name have been found")
  out_grob[[1L]]
}

#' @export
rm_grobs = function(x, grob_nms) {
  grobs_to_keep = which_grobs_not(x, grob_nms)
  y = to_gtable(x)
  y$layout <- y$layout[grobs_to_keep, , drop = FALSE]
  y$grobs <- y$grobs[grobs_to_keep]
  y
}

#' @export
zero_grob_col = function(x, grob_nm) {
  i = which_grobs(x, grob_nm)
  y = to_gtable(x)
  l = y$layout
  lr = l[i, c('l', 'r')]
  col_i = unique(unlist(mapply(seq, lr$l, lr$r, SIMPLIFY = FALSE)))
  y$widths[with(l, col_i)] = unit(0, 'points')
  y
}

seq_vec = function(v) {
  stopifnot(is.integer(v))
  stopifnot(length(v) == 2L)
  v[1]:v[2]
}

#remove_legend = function(plt, )

#' Stack Time Plots
#'
#'
stack_time_plots = function(..., time_id, layout, widths, heights) {
  plts = do.call(time_harmonize, c(list(...), list(time_id = time_id)))

  grobs = list()
  for(i in seq_along(plts)) {
    grobs[[i]] = ggplotGrob(plts[[i]] + theme(legend.position="none"))
  }
  grid.draw(do.call(rbind, grobs))
  #grid.draw(rbind(g1, g2, size = "last"))

  #check for legend, plot legends that exist
  if(length(ggplotGrob(p1))>18 & length(ggplotGrob(p2))>18){
    grob_legend1 = ggplotGrob(p1)$grobs[[15]]
    grob_legend2 = ggplotGrob(p2)$grobs[[15]]
    grid.arrange(g1, grob_legend1, g2,  grob_legend2, layout_matrix=layout, widths = widths, heights = heights)}

  if(length(ggplotGrob(p2))>18 & length(ggplotGrob(p1))==18){
    grob_legend2 = ggplotGrob(p2)$grobs[[15]]
    grid.arrange(g1, g2,  grob_legend2, layout_matrix=layout, widths = widths, heights = heights)}

  if(length(ggplotGrob(p1))>18 & length(ggplotGrob(p2))==18){
    grob_legend1 = ggplotGrob(p1)$grobs[[15]]
    grid.arrange(g1, grob_legend1, g2, layout_matrix=layout, widths = widths, heights = heights)}
}
