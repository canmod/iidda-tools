.onLoad <- function(lib, pkg) {
  options(
    iidda_perl = TRUE,
    iidda_metadata_schema = "https://github.com/datacite/schema/blob/master/source/json/kernel-4.3/datacite_4.3_schema.json",
    iidda_global_data_dictionary = "https://github.com/canmod/iidda/blob/main/global-metadata/data-dictionary.json"
  )

  col_classes_dict = list(
      string = "character",
      integer = "integer",
      number = "numeric",
      factor = "character",
      date = "Date",
      datetime = "POSIXct",
      boolean = "logical"
  )
}
