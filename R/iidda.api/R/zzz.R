.onLoad <- function(lib, pkg) {
  options(
      iidda_api_all_char = FALSE
    , iidda_api_date_sort = TRUE
    , iidda_api_msgs = TRUE
  )

  ops = suppressWarnings({try(do.call(make_ops_list, production), silent = TRUE)})
  ops_local = suppressWarnings({try(do.call(make_ops_list, local), silent = TRUE)})
  ops_staging = suppressWarnings({try(do.call(make_ops_list, staging), silent = TRUE)})
  assignInNamespace("ops", ops, "iidda.api")
  assignInNamespace("ops_local", ops_local, "iidda.api")
  assignInNamespace("ops_staging", ops_staging, "iidda.api")
}
