need_iidda_api <- function(function_name) {
  if (!requireNamespace("iidda.api", quietly = TRUE)) {
    stop(
      "\nThis ", function_name, " function requires the iidda.api package.",
      "\nPlease install iidda.api if you need to use this function.",
      "\nInstallation instructions are here: ",
      "https://canmod.github.io/iidda-tools/iidda.api"
    )
  }
}

in_src_for_this_pkg = function() {
  in_a_pkg = file.exists("DESCRIPTION")
  if (in_a_pkg) {
    pkg_in_wd = read.dcf("DESCRIPTION")[1L, "Package"] |> unname()
    return(pkg_in_wd == "iidda.analysis")
  }
  return(FALSE)
}

cache_data_dictionary = function() {
  if (in_src_for_this_pkg()) {
    dict = pull_data_dictionary()
    if (!is.null(dict)) {
      cached_dict_path = "inst/iidda-data-dictionary.rdata"
      saveRDS(dict, file = cached_dict_path)
    }
  }
  invisible(NULL)
}

## copied from iidda.api
set_dict_names = function(x) {
  setNames(x, vapply(x, getElement, character(1L), "name"))
}

pull_data_dictionary = function() {
  dict = try(iidda.api::ops_staging$data_dictionary(), silent = TRUE)
  if (inherits(dict, "try-error")) return(NULL)
  return(set_dict_names(dict))
}
read_data_dictionary = function() {
  ("iidda-data-dictionary.rdata"
    |> system.file(package = "iidda.analysis")
    |> readRDS()
  )
}

iidda_data_dictionary = function() {
  do_pull = "iidda_try_pull_dict" |> getOption() |> isTRUE()
  dict = if (do_pull) pull_data_dictionary() else NULL
  if (is.null(dict)) dict = read_data_dictionary()
  return(dict)
}
