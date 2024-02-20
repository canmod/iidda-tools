#' End-Dates of Epiweeks
#'
#' @param year Integer vector of years.
#' @param week Integer vector of weeks.
#'
#' @return Date vector of the end-dates of each specified epiweek.
#' @importFrom lubridate weeks
#' @export
epiweek_end_date = function(year, week) {
  as.Date(end_of_epiweek_1(year)) + weeks(week - 1)
}


# Return the last date of the first epiweek in a year.
#
# """
# The first epi week of the year ends, by definition, on
# the first Saturday of January, as long as it falls at least
# four days into the month.
# """
# The Skeeter, 66: 1
# https://www.cmmcp.org/sites/g/files/vyhlif2966/f/uploads/spring_skeeter_06.pdf
#
end_of_epiweek_1_per_year <- function(year) {
  # Create a date object for January 1 of the specified year
  january_1 <- as.Date(paste(year, "01-01", sep = "-"), format = "%Y-%m-%d")

  # Calculate the day of the week for January 1 (Sunday = 1, Saturday = 7)
  day_of_week <- as.numeric(format(january_1, "%u"))

  # Calculate the number of days to add to reach the first Saturday
  days_to_saturday <- ifelse(day_of_week <= 6, 6 - day_of_week, 13 - day_of_week)

  # Calculate the date of the first Saturday in January
  first_saturday <- january_1 + days_to_saturday

  # Check if the first Saturday is at least four days into the month
  if (as.numeric(format(first_saturday, "%d")) >= 4) {
    return(first_saturday)
  } else {
    # If it's not at least four days into the month, add a week to get the next Saturday
    first_saturday <- first_saturday + 7
    return(first_saturday)
  }
}
end_of_epiweek_1_per_year_mem = memoise(end_of_epiweek_1_per_year)
end_of_epiweek_1_per_year_vec = Vectorize(end_of_epiweek_1_per_year_mem)
end_of_epiweek_1 = function(x) {
  as.Date(end_of_epiweek_1_per_year_vec(x))
}
