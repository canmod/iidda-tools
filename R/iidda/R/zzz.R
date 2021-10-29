.onLoad <- function(lib, pkg) {
  options(
    iidda_perl = TRUE,
    iidda_metadata_schema = "https://github.com/datacite/schema/blob/master/source/json/kernel-4.3/datacite_4.3_schema.json"
  )
}
