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
  CDI = "Communicable Disease Incidence",
  Population = "Population"
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
