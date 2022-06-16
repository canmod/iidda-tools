#' Fill Regex Template
#'
#' Resolve a length-1 character vector containing a regex template into a
#' regular expression for matching age bound information in disease category
#' names
#'
#' @params re_template template that resolve to regular expressions
#' for matching age information contained in category names
#' @param which_bound resolve the template to match lower or upper bounds,
#' neither (the default), or single
#' @export
fill_re_template = function(re_template, which_bound = 'neither') {
  stopifnot(is.character(re_template))
  is_single_bin =
    grepl("\\%\\{left\\}s", re_template) |
    grepl("\\%\\{right\\}s", re_template)
  if (is_single_bin) {
    which_bound = 'single'
  }
  switch(
    which_bound,
    lower = sprintf_named(re_template, lower_left = '(', lower_right = ')', upper_left = '', upper_right = '', .check = FALSE),
    upper = sprintf_named(re_template, lower_left = '', lower_right = '', upper_left = '(', upper_right = ')', .check = FALSE),
    neither = sprintf_named(re_template, lower_left = '', lower_right = '', upper_left = '', upper_right = '', .check = FALSE),
    single = sprintf_named(re_template, left = '(', right = ')', .check = FALSE)
  )
}

#' Wrap Age Patterns
#'
#' Wrap list of regular expressions for matching age bounds in disease
#' category names, so that the resulting regular expressions can be used
#' for different purposes (extraction, removal, or validation)
#'
#' @param patterns vector of regular expressions for matching age bound
#' information in disease category names
#' @param purpose character string indicating the purpose of the resulting
#' regular expression
#' @param prefix pattern to match at the beginning of the string that marks
#' the beginning of age information
#' @export
wrap_age_patterns = function(
    patterns,
    purpose = c('extraction', 'removal', 'validate'),
    prefix = '' # ".*\\.age\\." would be a good option for LBoM extraction
  ) {
  switch(
    purpose,
    extraction = paste0(
      '(?:', prefix, ')(?:',
      paste0(unlist(patterns), collapse = '|'),
      ')'
    ),
    removal = paste0(
      '(', prefix, ')?(',
      paste0(unlist(patterns), collapse = '|'),
      ')'
    ),
    validate = paste0(
      re_start,
      '(', prefix, ')(',
      paste0(paste0('(\\.', unlist(patterns), ')'), collapse = '|'),
      ')?$'
    )
  )
}

#' Fill Template and Wrap the Results
#'
#' Convenience function to do \code{fill_re_template} and
#' \code{wrap_age_patterns} in one step
#' @export
fill_and_wrap = function(re_templates, which_bound, purpose, prefix = '') {
  (re_templates
   %>% lapply(fill_re_template, which_bound)
   %>% wrap_age_patterns(purpose, prefix)
  )
}

#' Matched Age Bound
#'
#' Process output from regmatches to return the correct age bound.
#' Used in the lookup function created by \code{make_age_hash_table}
#'
#' @param x character vector from the list output of regmatches,
#' containing regex matches of age bound information contained in
#' disease category names. each \code{x} corresponds to a single
#' category.
#' @return
#' @export
return_matched_age_bound = function(x) {

  # ignore the first item, because it is just the full input string,
  # and we want substrings that were matched by the regex
  x = x[-1]

  # if no matching substrings are found or if there are more than one
  # matching substring, return a blank string
  not_blank = nchar(x) > 0
  if(!any(not_blank)) return('')
  out = x[not_blank]
  if(length(out) != 1L) return('')

  # otherwise return the matched age bound
  # as a length-one character string
  out
}

#' Make Age Hash Table
#'
#' Create a lookup function that takes a character vector of disease category
#' names and returns a vector of equal length containing either the lower
#' or upper age bounds contained in the categories. If no bound is present
#' then \code{NA} is returned.
#'
#' @param categories character vector of disease category names
#' @param re_templates list of templates that resolve to regular expressions
#' for matching age information contained in category names
#' @param which_bound should the lookup function return lower or upper age
#' bounds
#' @return vector containing either the lower or upper age bounds contained
#' in the categories
#' @export
make_age_hash_table = function(categories, re_templates, which_bound = c('lower', 'upper', 'neither', 'single'), prefix = '') {
  return_matched_age_bound = return_matched_age_bound
  lookup_hash = (re_templates
   %>% lapply(fill_re_template, which_bound)
   %>% wrap_age_patterns(purpose = 'extraction', prefix)
   %>% regexec(categories, perl = TRUE)
   %>% regmatches(x = categories)
   %>% lapply(return_matched_age_bound)
   %>% setNames(categories)
   %>% list2env(hash = TRUE)
  )
  function(cats_to_lookup) {
    (cats_to_lookup
     %>% lapply(function(x) lookup_hash[[x]])
     %>% unlist_char_list
     %>% as.integer
    )
  }
}

#' Remove Age
#'
#' Remove age information from a vector of category names
#'
#' @param categories vector of category names
#' @param re_templates list of templates that resolve to regular expressions
#' for matching age information contained in category names
#' @export
remove_age = function(categories, re_templates, prefix = '') {
  re = (re_templates
        %>% lapply(fill_re_template)
        %>% wrap_age_patterns('removal', prefix)
  )
  sub(re, '', categories, perl = TRUE)
}

#' @importFrom memoise memoise
#' @rdname remove_age
#' @export
memoise_remove_age <- memoise::memoise(remove_age)

# should the remaining be put into functions?


# # get the names of all 'diseases'
# # (includes information about age ranges,
# # so i prefer to call them categories or cats).
# all_cats = all_disease_names(masterList)
#
# # the following were built specifically for the mortality
# # data and may need to be generalized for additional data
# # sets
#
# # need to separate disease and age information from the category names.
# # so we need to use regular expressions. because we need to use similar
# # regular expressions for different purposes, we create templates of
# # regular expressions that can be modified for different purposes. the
# # following list contains regular expression templates for matching
# # different ways to encode age information in the category names.
# re_templates = list(
#   .to. = "%{lower_left}s[0-9]+%{lower_right}s\\.to\\.%{upper_left}s[0-9]+%{upper_right}s(?:\\.years)?",
#   .to = "%{lower_left}s[0-9]+%{lower_right}s\\.to%{upper_left}s[0-9]+%{upper_right}s",
#   .plus = "%{lower_left}s[0-9]+%{lower_right}s\\.plus",
#   less.than. = "less\\.than\\.%{upper_left}s[0-9]+%{upper_right}s",
#   under. = "under\\.%{upper_left}s[0-9]+%{upper_right}s(\\.year(s?))?",
#   all.ages = "all\\.ages",
#   .and.upwards = "%{lower_left}s[0-9]+%{lower_right}s\\.and\\.upwards")
#
# # the examples below will make it clear what all of this means. but first,
# # we show how to use the fill_re_template function in utilities.R to generate
# # the patterns used for matching
# re_patterns = lapply(re_templates, fill_re_template)
#
#
# # because the dataset is big and there are many types of bounds
# # to look up, we use these functions to create lookup functions
# # for extracting upper and lower bounds of age ranges.
# upper_lookup = make_age_hash_table(all_cats, re_templates, 'upper')
# lower_lookup = make_age_hash_table(all_cats, re_templates, 'lower')
