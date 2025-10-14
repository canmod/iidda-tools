library(iidda.analysis)

if (interactive()) {
  measles = ("measles_1800_to_1842.rds"
    |> system.file(package = "iidda.analysis")
    |> readRDS()
  )
  meningitis = ("meningitis_on.rds"
    |> system.file(package = "iidda.analysis")
    |> readRDS()
  )
  london_mort = ("london-mort-harmonized.rds"
    |> system.file(package = "iidda.analysis")
    |> readRDS()
  )
  
  iidda_bar(meningitis)
  
  cause_lookup = iidda.api::ops_staging$lookup_tables("lbom-cause-lookup")
  cause_hierachy = (cause_lookup
    |> select(synonym, nesting_cause)
    |> rename(cause = synonym)
    |> distinct()
  )
  
  
  converter = DataDictionaryConverter()
  NestingHarmonizer = function(category_level, id_variables) {
    function(data
        , value_variable = NULL
        , category_variable = NULL
        , nesting_variable = NULL
      ) {
      data = iidda.analysis:::resolve_var_args(data)
      top_level_vars = c(id_variables, category_variable, value_variable)
      grouping_vars = c(id_variables, nesting_variable)
      final_id_vars = c(id_variables, category_variable)
      top_level_data = (data
        |> filter(.data[[category_variable]] == category_level)
        |> select(all_of(top_level_vars))
      )
      finer_data = (data
        |> filter(.data[[nesting_variable]] == category_level)
        |> summarise(!!sym(value_variable) := sum(.data[[value_variable]]), .by = all_of(grouping_vars))
        |> rename(!!sym(category_variable) := all_of(nesting_variable))
      )
      output_data = bind_rows(top_level_data, finer_data)
      unique_numbers_of_repeats = (output_data
        |> summarise(n = n(), .by = all_of(final_id_vars))
        |> pull(n)
        |> unique()
      )
      if (!identical(unique_numbers_of_repeats, 1L)) {
        stop("Harmonization failed. Please insert more coins for a better explanation")
      }
      iidda.analysis:::iidda_return(output_data, data)
    }
  }
  
  (meningitis
    |> iidda_prep_bar(time_unit = "month_factor_abbr")
    |> iidda_attach_bar()
    |> iidda_render_plot(title = "Friend")
  )
}
