test_that("can make a deheaped plot", {
  deheaper = Deheaper()
  measles_1800_to_1842_deheaped = deheaper(measles_1800_to_1842)
  measles_1800_to_1842_deheaped |> nrow()
  measles_1800_to_1842 |> nrow()
  ## iidda_bar(measles_1800_to_1842_deheaped, series_variable = "deaths", time_variable = "period_end_date")  ## warning because this isn't sorted
  iidda_bar(measles_1800_to_1842_deheaped, series_variable = "deheaped_deaths", time_variable = "period_end_date")
})

# dict = jsonlite::read_json("../../../iidda/global-metadata/data-dictionary.json") |> iidda.api:::set_dict_names()
# library(macpan2); library(testthat); library(dplyr); library(tidyr); library(ggplot2); library(deSolve)
# (measles_1800_to_1842
#   |> iidda:::parse_columns(dict)
#   |> ggplot()
#   + aes(period_end_date, deaths)
#   + geom_line()
# )
