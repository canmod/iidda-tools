# modified versions of functions from rapiclient. allows us to depend
# on official rapiclient and not our fork of it.
# it is used in iidda.api/R/bindings.R

.api_args = function(formals, environment) {
    arg_names <- if (is.null(names(formals))) character() else names(formals)
    arg_names <- arg_names[!arg_names %in% c("...", ".__body__")]
    args <- mget(arg_names, environment)
    expand_url_args = function(x) {
      lst = do.call(c, lapply(x, function(z) as.list(z)))

      ## silence warning about passing NULL to structure
      if (is.null(lst)) return(lst)

      nmd_lst = structure(lst, names = rep(names(x), sapply(x, length)))
      return(nmd_lst)
    }
    expand_url_args(args)
}

set_default_args_list <- function (f, arguments) {
  saved_attributes = attributes(f)
  formals(f)[names(arguments)] <- arguments
  attributes(f) = saved_attributes
  f
}
