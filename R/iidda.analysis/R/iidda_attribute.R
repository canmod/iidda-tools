
#' @export
iidda_defaults_rm = function(data) {
  attr(data, "iidda") = NULL
  return(data)
}


iidda_defaults_cp = function(data_1, data_2) {
  attr(data_2, "iidda") = attr(data_1, "iidda")
  return(data_2)
}

## like iidda_defaults for data frames but
## only updates those that appear in ...
## if they are not NULL
iidda_defaults_if = function(data, ...) {
  attrs = attr(data, "iidda")
  if (is.null(attrs)) attrs = list()
  defaults = list(...) |> Filter(f = Negate(is.null))
  for (nm in names(defaults)) attrs[[nm]] = defaults[[nm]]
  structure(data, iidda = attrs)
}

return_iidda = function(data
    , data_with_defaults = NULL
    , more_iidda_attrs = list()
  ) {
  if (!is.null(data_with_defaults)) {
    data = iidda_defaults_cp(data_with_defaults, data)
  }
  if (is.recursive(more_iidda_attrs)) {
    iidda_attrs = attr(data, "iidda")
    if (is.recursive(iidda_attrs)) {
      attr(data, "iidda") = c(iidda_attrs, more_iidda_attrs)
    } else {
      stop("The `iidda` attributes in a dataset are not valid.")
    }
  } else {
    stop("The `more_iidda_attrs` argument must be a list.")
  }
  return(data)
}

return_data_prep_function = function(data) {
  structure(data, class = "iidda_data_prep_function")
}

#' @export
iidda_defaults = function(object, ...) UseMethod("iidda_defaults")

#' @export
iidda_defaults.environment = function(object, ...) {
  defaults = list(...)
  for (nm in names(defaults)) {
    assign(nm, defaults[[nm]], envir = object)
  }
  return(object)
}

#' @export
iidda_defaults.function = function(object, ...) {
  iidda_defaults(environment(object), ...)
  return(object)
}

#' @export
iidda_defaults.data.frame = function(object, ...) {
  defaults = list(...)
  for (nm in names(defaults)) {
    attr(object, "iidda")[[nm]] = defaults[[nm]]
  }
  return(object)
}
