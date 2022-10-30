col_classes_dict = list(
  string = "character",
  integer = "integer",
  number = "numeric",
  factor = "character",
  date = "Date",
  datetime = "POSIXct",
  boolean = "logical"
)

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

#' Frequency to By
#'
#' Convert words describing frequencies to phrases.
#'
#' @param freq one of `"weekly"` (becomes `"7 days"`),
#' `"4-weekly"` (becomes `"28 days"`),
#' `"monthly"` (becomes `"1 month"`)
#'
#' @export
freq_to_by = function(freq) {
  switch(freq,
         weekly = "7 days",
         `4-weekly` = "28 days",
         monthly = "1 month",
         stop('the frequency, ', frequency,
              ', given in the metadata is not currently an option'))
}

#' Frequency to Days
#'
#' Convert words describing frequencies to corresponding numbers of days
#'
#' @param freq one of `"weekly"` (becomes `7`),
#' `"4-weekly"` (becomes `28`),
#' `"monthly"` (returns an error)
#' @export
freq_to_days = function(freq) {
  switch(freq,
         weekly = 7,
         `4-weekly` = 28,
         monthly = stop('cannot specify monthly patterns in days'),
         stop('the frequency, ', frequency,
              ', given in the metadata is not currently an option'))
}
