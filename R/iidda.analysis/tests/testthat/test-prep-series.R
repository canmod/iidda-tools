if (interactive()) source("tests/testthat/setup.R")

dd = iidda_defaults_rm(smallpox)
xx = iidda_prep_series(smallpox, handle_zero_values = function(data) iidda_defaults_rm(data))


xx = CustomDataPrep(function(data, series_variable = NULL) data)
xx(smallpox)
