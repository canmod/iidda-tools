#' @export
iidda_availability = function(data
  , initial_ggplot_object = ggplot()

  ## vars
  , count_variable = NULL
  , total_count_variable = NULL
  , period_start_variable = NULL
  , period_mid_time_variable = NULL
  , period_end_variable = NULL
  , period_days_variable = NULL
  , norm_variable = NULL
  , colour_variable = NULL
  , within_panel_variable = NULL
  , among_panel_variable = NULL
  
  ## attach
  , within_panel_order = OrderGuesser()
  , colour_order = OrderGuesser()
  , variable_converter = DataDictionaryConverter()
  , period_describer = PeriodDescriber(mid_types = "time")
  , count_aggregator = CountAggregator()
  
  ## attach
  , pages = 1  ## need to convert to `page`
  , page_size = NULL
  , scale_colour = getOption("ggplot2.discrete.fill")
  , title_colour = waiver()
  , title_totals = waiver()
  , x_title = waiver()
  , x_breaks = waiver()
  , x_minor_breaks = waiver()
  , x_date_labels = waiver()
  , text_size = 9
  , left_margin = 100
  , legend_margin = 15
  , subplot_widths = c(5, 1)
) {
  data_prep = pass_args(data, iidda_prep_availabiliy)
  plots = list()
  for (page in pages) {
    plots[[page]] = (data_prep
      |> pass_args(iidda_attach_availability)
      |> get_iidda_attr("plot")
    )
  }
  return(plots)
}


PrepAvailability = function() {
  flush_arg_guesses = TRUE
  function(data
      , count_variable = NULL
      , total_count_variable = NULL
      , period_start_variable = NULL
      , period_mid_time_variable = NULL
      , period_end_variable = NULL
      , period_days_variable = NULL
      , norm_variable = NULL
      , colour_variable = NULL
      , within_panel_variable = NULL
      , among_panel_variable = NULL
      , within_panel_order = OrderGuesser()
      , colour_order = OrderGuesser()
      , variable_converter = DataDictionaryConverter()
      , period_describer = PeriodDescriber(mid_types = "time")
      , count_aggregator = CountAggregator()
  ) {
    resolve_choosers("within_panel_order", "colour_order")
    data = resolve_var_args(data
      , ign_variables = "total_count_variable"
      , opt_variables = c(
            "period_start_variable", "period_end_variable"
          , "period_mid_time_variable", "period_days_variable"
        )
      , req_variables = c(
          "colour_variable", "within_panel_variable", "among_panel_variable"
        )
    )
    data = resolve_specific_arg(data
      , arg = "total_count_variable"
      , default = sprintf("total_%s", count_variable)
    )
    data = (data
      |> variable_converter()
      |> period_describer()
      |> count_aggregator(grouping_variable = among_panel_variable)
    )
    aggregated = attr(data, "iidda")$aggregated
    within_panel_order = within_panel_order(data, within_panel_variable)
    colour_order = colour_order(data, colour_variable)
    output_data = (data
      |> filter(get(among_panel_variable) %in% aggregated[[among_panel_variable]])
      |> mutate(!!sym(within_panel_variable) := factor(get(within_panel_variable), levels = within_panel_order))
      |> mutate(!!sym(colour_variable) := factor(get(colour_variable), levels = colour_order))
      |> mutate(!!sym(among_panel_variable) := factor(get(among_panel_variable), levels = levels(aggregated[[among_panel_variable]])))
    )
    return_iidda(output_data, data)
  }
}




#' @export
AttachAvailability = function() function(data
  , initial_ggplot_object = ggplot()
  , count_variable = NULL
  , total_count_variable = NULL
  , period_start_variable = NULL
  , period_mid_time_variable = NULL
  , period_end_variable = NULL
  , period_days_variable = NULL
  , norm_variable = NULL
  , colour_variable = NULL
  , within_panel_variable = NULL
  , among_panel_variable = NULL
  , page = 1
  , page_size = NULL
  , scale_colour = getOption("ggplot2.discrete.fill")
  , title_colour = waiver()
  , title_totals = waiver()
  , x_title = waiver()
  , x_breaks = waiver()
  , x_minor_breaks = waiver()
  , x_date_labels = waiver()
  , text_size = 9
  , left_margin = 100
  , legend_margin = 15
  , subplot_widths = c(5, 1)
) {
  data = resolve_var_args(data
    , ign_variables = "total_count_variable"
    , opt_variables = c(
          "period_start_variable", "period_end_variable"
        , "period_mid_time_variable", "period_days_variable"
      )
    , req_variables = c(
        "colour_variable", "within_panel_variable", "among_panel_variable"
      )
  )
  data = resolve_specific_arg(data
    , arg = "total_count_variable"
    , default = sprintf("total_%s", count_variable)
  )
  totals = attr(data, "iidda")$aggregated
  if (is.null(totals)) {
    msg(
        "The dataset does not contain an `aggregated` dataset"
      , "in the `iidda` attributes. Did you use"
      , "`iidda_prep_availability()` to create `data`?"
    ) |> stop()
  }
  plims = c(
      as.POSIXct(min(data[[period_start_variable]]))
    , as.POSIXct(max(data[[period_end_variable]]) + days(1))
  )
  if (is.null(page_size)) page_size = nrow(totals)
  page = iidda::pager(page, page_size, rev = FALSE)(seq_len(nrow(totals)))
  totals_page = totals[page, , drop = FALSE]
  
  totals_labeller = (totals_page[[total_count_variable]]
    |> magnitude(digits = 1)
    |> setNames(totals_page[[among_panel_variable]])
    |> ggplot2::as_labeller()
  )
  data_this_page = filter(data
    , .data[[among_panel_variable]] %in% totals_page[[among_panel_variable]]
  )
  
  bar_plot = (initial_ggplot_object
    + aes(.data[[total_count_variable]], .data[[among_panel_variable]])
    + geom_col(fill = "grey", data = totals_page)
    + scale_x_continuous(name = title_totals, position = "top")
    + scale_y_discrete(name = "")
    + ggplot2::facet_wrap(facets = among_panel_variable
        , ncol = 1L
        , scales = "free_y"
        , strip.position = "left"
        , labeller = totals_labeller
      )
    + iidda_theme_availability_bars(text_size = text_size)
  )
  
  extent_plot = (initial_ggplot_object
    + aes(
          x = .data[[period_mid_time_variable]]
        , y = .data[[within_panel_variable]]
        , width = .data[[period_days_variable]] * 86400
        , fill = .data[[colour_variable]]
      )
    + geom_tile(alpha = 1, size = 0, colour = NA, data = data_this_page)
    + facet_wrap(among_panel_variable, ncol = 1L, strip.position = "left")
    + scale_y_discrete("", labels = NULL)
    + scale_x_datetime(x_title
      , breaks = x_breaks
      , minor_breaks = x_minor_breaks
      , limits = plims
      , date_labels = x_date_labels
      , expand = c(0, 0)
    )
    + iidda_theme_availability_heatmap(
          text_size = text_size
        , left_margin = left_margin
        , legend_margin = legend_margin
      )
    + scale_fill_discrete(guide = guide_legend(
            title = title_colour
          , position = "top"
        )
        , type = scale_colour
      )
  )
  plot = extent_plot + bar_plot + plot_layout(widths = subplot_widths)
  iidda_defaults_if(data, plot = plot)
}

#' @export
iidda_prep_availabiliy = PrepAvailability()

#' @export
iidda_attach_availability = AttachAvailability()
