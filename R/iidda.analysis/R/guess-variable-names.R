## Variable Name Argument Guess
## 
## This package is designed to work well with data that follow a particular
## data dictionary, and so it is often easy to figure out what the variables
## should be used for particular purposes. This arg_guess function is used
## in functions defined in resolve.R.
## 
## @param dat Some object that could contain arguments that name variables.
## @param arg Character vector giving the name of an argument that should
## contain the name of a variable (e.g., series_variable)
## @param type Type of variable
## @param guess_fn See "Underlying guess functions for variable_guess" below
## @param check_dat_in_std See "Error handlers for variable_guess" below
## @param check_std_in_dat See "Error handlers for variable_guess" below
arg_guess = function(dat, arg
    , guess_fn = first_std_in_dat
    , check_dat_in_std = check_dat
    , check_std_in_dat = check_std
  ) {
  type = sub("_variable$", "", arg)
  value = variable_guess(dat
    , std(type)
    , type
    , guess_fn
    , check_dat_in_std
    , check_std_in_dat
  )
  attr(value, "guess") = TRUE
  "Guessed that %s is the %s" |> sprintf(value, arg) |> message()
  return(value)
}

default_guess = function(guess, available, arg, is_new) {
  type = sub("_variable$", "", arg)
  check_dat_in_std = check_default
  check_std_in_dat = check_available
  if (is_new) check_dat_in_std = check_std_in_dat = check_nothing
  
  value = variable_guess(dat = guess, std = available, type = type
    , guess_fn = first_dat_in_std
    , check_dat_in_std = check_dat_in_std
    , check_std_in_dat = check_std_in_dat
  )
  attr(value, "guess") = TRUE
  "Guessed that %s is the %s" |> sprintf(value, arg) |> message()
  return(value)
}

## Guessing Variable Names
## 
## The variable_guess S3 method is the workhorse
## for making variable name guesses.
## 
## @param dat Some object that could contain arguments that name variables.
## @param std Character vector giving standard variable names for this type.
## @param type Type of variable
## @param guess_fn See "Underlying guess functions for variable_guess" below
## @param check_dat_in_std See "Error handlers for variable_guess" below
## @param check_std_in_dat See "Error handlers for variable_guess" below
variable_guess = function(dat, std, type
    , guess_fn = first_std_in_dat
    , check_dat_in_std = check_dat
    , check_std_in_dat = check_std
  ) {
  UseMethod("variable_guess")
}

#' @export
variable_guess.NULL = function(dat, std, type
    , guess_fn = first_std_in_dat
    , check_dat_in_std = check_dat
    , check_std_in_dat = check_std
  ) {
  ("Cannot guess %s_variable given that data were not supplied."
    |> sprintf(type)
    |> stop()
  )
}

#' @export
variable_guess.list = function(dat, std, type
    , guess_fn = first_std_in_dat
    , check_dat_in_std = check_dat
    , check_std_in_dat = check_std
  ) {
  guesses = (dat
    |> lapply(variable_guess, std, type
      , guess_fn
      , check_dat_in_std
      , check_std_in_dat
    )
    |> unique()
  )
  if (length(guesses) == 1L) return(guesses[[1L]])
  ("Cannot guesss what %s_variable should be used for all %s checked datasets."
    |> paste("Please explicitly supply a value for the %s_variable argument.")
    |> sprintf(type, length(dat), type)
    |> stop()
  )
}

#' @export
variable_guess.data.frame = function(dat, std, type
    , guess_fn = first_std_in_dat
    , check_dat_in_std = check_dat
    , check_std_in_dat = check_std
  ) {
  variable_guess(names(dat), std, type
    , guess_fn
    , check_dat_in_std
    , check_std_in_dat
  )
}

#' @export
variable_guess.character = function(dat
    , std
    , type
    , guess_fn = first_std_in_dat
    , check_dat_in_std = check_dat
    , check_std_in_dat = check_std
  ) {

  dat_in_std = dat %in% std
  check_dat_in_std(dat_in_std, type)

  std_in_dat = std %in% dat
  check_std_in_dat(std_in_dat, type)

  guess_fn(dat, std, dat_in_std, std_in_dat)
}



## ---------------------------------
## Error handlers for variable_guess
## ---------------------------------
##
## All such functions must contain two arguments:
## @param check_vec Logical vector over the vectors in either dat or std.
## @param type String giving the type of variable being checked.
## 
## @return NULL -- called for the side effect of throwing errors, warnings,
## and/or messages if necessary.
check_nothing = function(check_vec, type) invisible(NULL)
make_check = function(..., action) {
  template = paste(...)
  function(check_vec, type) {
    if (!any(check_vec)) template |> sprintf(type) |> action()
    invisible(NULL)
  }
}
check_dat = make_check(
    "Data do not contain any recognized %s variables."
  , "Please supply a variable name explicitly for your dataset."
  , action = stop
)
check_std = make_check(
    "Need to specify a value for the %s_variable argument."
  , action = stop
)
check_default = make_check(
    "None of the default guesses for the %s_variable argument are in the data."
  , action = stop
)
check_available = make_check(
    "The data do not contain any of the defaut guesses for the %s_variable."
  , action = stop
)
# check_dat = function(check_vec, type) {
#   if (!any(check_vec)) {
#     ("Data do not contain any recognized %s variables."
#       |> paste("Please supply a variable name explicitly for your dataset.")
#       |> sprintf(type)
#       |> stop()
#     )
#   }
#   invisible(NULL)
# }
# check_std = function(check_vec, type) {
#   if (!any(check_vec)) {
#     ("Need to specify a value for the %s_variable argument."
#       |> sprintf(type)
#       |> stop()
#     )
#   }
#   invisible(NULL)
# }


## ---------------------------------------------
## Underlying guess functions for variable_guess
## ---------------------------------------------
##
## All such functions need to have the following arguments:
## @param dat Character vector of variable names in the data.
## @param std Character vector of standard variable names of a particular type.
## @param dat_in_std Logical vector over dat indicating what variables in the
## data are also standard variables of a particular type.
## @param std_in_dat Logical vector over std indicating what standard variables
## of a particular type that are also in the data.
## 
## @return String giving the guess at a name of a variable of a particular type.
first_std_in_dat = function(dat, std, dat_in_std, std_in_dat) {
  i = which(std_in_dat)
  std[min(i)]
}
first_std_not_in_dat = function(dat, std, dat_in_std, std_in_dat) {
  i = which(!std_in_dat)
  std[min(i)]
}
first_dat_in_std = function(dat, std, dat_in_std, std_in_dat) {
  i = which(dat_in_std)
  dat[min(i)]
}
first_dat_in_std_or_first_std = function(dat, std, dat_in_std, std_in_dat) {
  if (any(dat_in_std)) {
    return(first_dat_in_std(dat, std, dat_in_std, std_in_dat))
  }
  std[[1L]]
}
all_dat_in_std = function(dat, std, dat_in_std, std_in_dat) {
  i = which(dat_in_std)
  dat[i]
}
first_dat = function(dat, std, dat_in_std, std_in_dat) dat[1L]





## Return all standard names for a particular type of variable.
## These standard names are defined in std.R by functions with
## the following name formula: `std_{type}_variables`.
## And so, for example, std("series") is equivalent to std_series_variables(),
## but the former is easier to handle programmatically
std = function(type) {
  fn_nm = name_std(type)
  if (!exists(fn_nm)) stop("Non-standard variable type, ", type)
  fn = get(fn_nm)
  fn()
}

name_std = function(type) {
  suffix = "_variables"
  pattern = paste0(suffix, "$")
  if (grepl(pattern, type)) suffix = ""
  sprintf("std_%s%s", type, suffix)
}

exists_std = function(type) {
  (type
    |> name_std()
    |> exists()
    |> isTRUE()
  )
}

list_std = function() {
  ("iidda.analysis"
    |> getNamespace()
    |> ls(pattern = "^std_[_a-z]+_variables$")
  )
}


is_guess = function(data, which) {
  (data
    |> get_iidda_attr(which)
    |> attr("guess")
    |> isTRUE()
  )
}



## special guessers
guess_grouping_columns = function(grouping_columns, data, non_grouping_columns = character()) {
  if (is.null(grouping_columns)) {
    grouping_columns = variable_guess(
        data
      , setdiff(std_grouping_variables(), non_grouping_columns)
      , "grouping"
      , all_dat_in_std
      , check_dat
      , check_nothing
    )
  }
  return(grouping_columns)
}



#' Validate time variables
#'
#' Validate if variable is a date data type in the data set.
#'
#' @param var_nm string of variable name
#' @param data data frame
#'
#' @return boolean of validation status
#' @export
valid_time_vars = function(var_nm, data) {
  (var_nm %in% names(data)) & inherits(data[[var_nm]], "Date")
}
