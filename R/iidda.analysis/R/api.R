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
