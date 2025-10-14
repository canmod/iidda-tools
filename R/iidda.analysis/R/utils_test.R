is_string = function(x) is.character(x) & (length(x) == 1L)
is_function_of_data = function(x) {
  if (!is.function(x)) return(FALSE)
  names(formals(x))[[1L]] == "data"
}
