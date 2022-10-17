LookupHarmonizerInterface = function() {
  self = Interface()
  self$harmonize = function(column = character(0L)) character(length(column))
  return_object(self, "LookupHarmonizerInterface")
}
LookupHarmonizer = function(lookup_list) {
  self = implements(LookupHarmonizerInterface)
  self$.lookup_list = lookup_list
  self$harmonize = function(column = character(0L)) {
    l = self$.lookup_list

  }
}
LookupHarmonizerDataFrame = function(lookup_table, key, value) {
  self = LookupHarmonizer(
    lookup_list = setNames(
      as.list(lookup_table[[value]]),
      lookup_table[[key]]
    )
  )
  return_object(self, "LookupHarmonizerDataFrame")
}


#' @export
col_classes_dict = list(
  string = "character",
  integer = "integer",
  number = "numeric",
  factor = "character",
  date = "Date",
  datetime = "POSIXct",
  boolean = "logical"
)


#' @export
resource_type_dict = list(
  # TODO: populate/harmonize from/with:
  # https://github.com/davidearn/data_work/blob/main/tracking/DataTypes.csv
  CDI = "Communicable Disease Incidence",
  Population = "Population",
  Mortality = "Mortality",
  ACM = "All-Cause Mortality",
  Birth = "Birth",
  Plague = "Plague" ## TODO: should this really be a dataset type? too specific to LBoM?
)

#' @export
freq_to_by = function(freq) {
  switch(freq,
         weekly = "7 days",
         `4-weekly` = "28 days",
         monthly = "1 month",
         stop('the frequency, ', frequency,
              ', given in the metadata is not currently an option'))
}

#' @export
freq_to_days = function(freq) {
  switch(freq,
         weekly = 7,
         `4-weekly` = 28,
         monthly = stop('cannot specify monthly patterns in days'),
         stop('the frequency, ', frequency,
              ', given in the metadata is not currently an option'))
}
