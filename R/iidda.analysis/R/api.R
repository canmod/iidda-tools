need_iidda_api <- function(function_name) {
  if (!requireNamespace("iidda.api", quietly = TRUE)) {
    stop(
      "\nThis ", function_name, " function is old.",
      "\nIt requires iidda.api, on which iidda.analysis no longer depends.",
      "\nPlease install iidda.api if you need this function."
    )
  }
}
