## Resolve arguments that contain the name of a variable in a dataset
## 
## A call to this function often occurs at the beginning of a data prep or
## plotting function. When it is called, all arguments that end with 
## `_variable` are resolved. In most cases this will mean that they will 
## have either a non-NULL value or an error will be thrown, although these
## outcomes can be avoided defining character vectors called ign_variables, 
## opt_variables, or new_variables in the body of the constructor function
## that generates the data prep or plotting function.
## 
## @param data Data frame being processed.
## @param flush_arg_guesses Should guesses at variable names that are stored
## in the iidda attribute have their tag of 'guess' removed?
## @param ign_variables Variables that should be completely ignored
## when resolve_var_args is called
## @param opt_variables Variables that do not need to be in the
## data when resolve_var_args is called
## @param new_variables Variables that should not be in the
## data when resolve_var_args is called
## 
## @return Version of `data` with updated metadata in an attribute called iidda.
resolve_var_args = function(data
    , flush_arg_guesses = TRUE
    , ign_variables = character()
    , opt_variables = character()
    , new_variables = character()
  ) {
  meth_env = parent.frame() ## ... argument list of the data transformer
  force(meth_env)
  data = resolve_var_args_util(data
    , flush_arg_guesses
    , ign_variables
    , opt_variables
    , new_variables
    , meth_env
  )
  return(data)
}

resolve_specific_arg = function(data, arg, default, flush_arg_guesses = TRUE) {
  meth_env = parent.frame() ## ... argument list of the data transformer
  force(meth_env)
  data = resolve_specific_arg_util(data
    , arg
    , default
    , flush_arg_guesses
    , meth_env
  )
  return(data)
}

resolve_hier_args = function(data, hierarchical_variable) {
  meth_env = parent.frame() ## ... argument list of the data transformer
  force(meth_env)
  data = resolve_var_args_util(data
      , flush_arg_guesses = FALSE
      , ign_variables = c("nesting_variable", "basal_variable")
      , meth_env = meth_env
    )
    data = resolve_specific_arg_util(data
      , "basal_variable"
      , sprintf("basal_%s", hierarchical_variable)
      , meth_env = meth_env
    )
    ## check_vars_in_data(data, "basal_variable", get("basal_variable", envir = meth_env))
    data = resolve_specific_arg_util(data
      , "nesting_variable"
      , sprintf("nesting_%s", hierarchical_variable)
      , meth_env = meth_env
    )
    check_vars_in_data(data, "nesting_variable", get("nesting_variable", envir = meth_env))
    return(data)
}




resolve_var_args_util = function(data
    , flush_arg_guesses = TRUE
    , ign_variables = character()
    , opt_variables = character()
    , new_variables = character()
    , meth_env = emptyenv()
  ) {

  obj_env = parent.env(meth_env) ## ... env where data transformer is created

  args = (meth_env
    |> ls() ## important that resolution rules only apply to 'variables'
    |> grep(pattern = "_variable$", value = TRUE)
    |> setdiff(ign_variables)
  )

  for (arg in args) {

    value_in_meth = get(arg, envir = meth_env) ## first choice (most explicit)
    value_in_obj = try(get(arg, envir = obj_env), silent = TRUE) ## second choice (pretty implicit)
    value_in_data = attr(data, "iidda")[[arg]] ## third choice (pretty implicit)

    if (inherits(value_in_obj, "try-error")) value_in_obj = NULL

    how_arg_was_found = ""

    ## try to find the column, named by the arg, in order of precedence
    if (!is.null(value_in_meth)) {
      iidda_arg_conflict(arg, value_in_meth, c(value_in_obj, value_in_data))
      attr(data, "iidda")[[arg]] = value_in_meth
      how_arg_was_found = "meth"
    } else if (!is.null(value_in_obj)) {
      iidda_arg_conflict(arg, value_in_obj, value_in_data)
      attr(data, "iidda")[[arg]] = value_in_obj
      assign(arg, value_in_obj, envir = meth_env)
      how_arg_was_found = "obj"
    } else if (!is.null(value_in_data)) {
      assign(arg, value_in_data, envir = meth_env)
      how_arg_was_found = "data"
    } else { ## guessing
      if (arg %in% opt_variables) {
        value_guessed = arg_guess(data, arg
          , guess_fn = first_dat_in_std_or_first_std
          , check_dat_in_std = check_nothing
          , check_std_in_dat = check_nothing
        )
      } else {
        value_guessed = arg_guess(data, arg)
      }
      attr(data, "iidda")[[arg]] = value_guessed
      assign(arg, value_guessed, envir = meth_env)
      how_arg_was_found = "guess"
    }

    ## check that the column, named by the arg, either
    ## should or should not exist in the data
    value_to_use = get(arg, envir = meth_env)
    if (!arg %in% opt_variables) {
      is_new = arg %in% new_variables
      if (( value_to_use %in% names(data)) &  is_new) {
        msg(
            "Column", value_to_use, "for the", arg
          , "was going to be created, but is already in the data."
          , "The best way to fix this problem is probably by attaching"
          , "the name for this", arg
          , "to the data using the iidda_defaults() function,"
          , "and removing any other places where this", arg, "is set."
        ) |> stop()
      }
      if (!is_new) check_vars_in_data(data, arg, value_to_use)
    }
  }
  if (flush_arg_guesses) data = resolve_arg_guesses(data)
  return(data)
}


## 
## 
## arg : string giving the name of the argument to resolve (e.g., cases_this_period)
## default : default value for arg
resolve_specific_arg_util = function(data, arg, default, flush_arg_guesses = TRUE, meth_env = emptyenv()) {
  obj_env = parent.env(meth_env) ## ... the object that contains the method

  value_in_meth = get(arg, envir = meth_env) ## first choice (most explicit)
  value_in_obj = try(get(arg, envir = obj_env), silent = TRUE) ## second choice (pretty implicit)
  value_in_data = attr(data, "iidda")[[arg]] ## third choice (pretty implicit)

  if (inherits(value_in_obj, "try-error")) value_in_obj = NULL

  if (!is.null(value_in_meth)) {
    iidda_arg_conflict(arg, value_in_meth, c(value_in_obj, value_in_data))
    attr(data, "iidda")[[arg]] = value_in_meth
  } else if (!is.null(value_in_obj)) {
    iidda_arg_conflict(arg, value_in_obj, value_in_data)
    attr(data, "iidda")[[arg]] = value_in_obj
    assign(arg, value_in_obj, envir = meth_env)
  } else if (!is.null(value_in_data)) {
    assign(arg, value_in_data, envir = meth_env)
  } else if (is.null(default)) {
    sprintf(
        "Cannot find a value for the argument, %s. Please specify one."
      , arg
    ) |> stop()
  } else {
    ## `default == guess` in this context
    default = arg_guess(default, arg, first_std_in_dat)
    attr(data, "iidda")[[arg]] = default
    assign(arg, default, envir = meth_env)
  }

  if (flush_arg_guesses) data = resolve_arg_guesses(data)
  return(data)
}




resolve_plot_values = function(...) {
  args = list(...) |> lapply(as.character) |> unlist(TRUE, FALSE)
  env = parent.frame()
  for (arg in args) {
    value = get(arg, envir = env)
    if (!is.function(value)) {
      assign(arg, DegenerateChooser(value), envir = env)
    } else if (!is_function_of_data(value)) {
      msg(
          sprintf("The function supplied to argument, %s,", arg)
        , "must be have a first argument called 'data'."
        , "Alternatively, you may directly supply the output of such a"
        , "function, which is simpler if it doesn't depend on the data."
      ) |> stop()
    }
  }
}


# vars : character vector of variable names
# args : character vector of types of variable
# length(vars) == length(args)
check_vars_in_data = function(data, args, vars) {
  bad = !vars %in% names(data)
  if (any(bad)) {
    ("Column %s for the %s is not in the data."
      |> sprintf(vars[bad], args[bad])
      |> msg()
      |> stop()
    )
    stop("Column ", var_nm, " for the ", args, " is not in data.")
  }
}

iidda_arg_conflict = function(arg_name, primary_arg_value, other_arg_values) {
  for (value in other_arg_values) {
    if (!is.null(value)) if (!identical(primary_arg_value, value)) {
      ## TODO: point to help, after writing it
      warning("Choosing ", primary_arg_value, " over ", value, " for ", arg_name)
    }
  }
}
