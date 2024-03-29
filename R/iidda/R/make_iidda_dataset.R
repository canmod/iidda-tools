#' Create New IIDDA Dataset from Single File
#'
#' @param single_file path to single data file
#' @param new_repo path to new IIDDA repository
#' @param lifecycle character vector giving the lifecycle state
#' (https://github.com/davidearn/iidda/blob/main/LIFECYCLE.md)
#' Probably 'Unreleased', but it could in
#' principle be 'Static', 'Dynamic', or 'Superseded'.
#' @return No return value. Call to produce a new directory structure in a new
#' IIDDA git repository containing a single source data file.
#' @export
iidda_from_single_file = function(single_file, new_repo, lifecycle) {
  iidda_extensions = c('csv', 'xls', 'xlsx', 'pdf')
  # extension pattern from TMB in case it is useful: "\\.[^\\.]*$"
  # another extension removal pattern (remove anything after the last dot):
  #     sub(pattern = "(.*)\\..*$", replacement = "\\1", x)
  ext_pat =
    paste0(
      '\\.(',
      paste0(iidda_extensions, collapse = '|'),
      ')$',
      collapse = '')

  # e.g. "file.xlsx"
  base_source_file = basename(single_file)

  # e.g. file"
  new_data_name = sub(ext_pat, '', base_source_file, ignore.case = TRUE)

  # e.g. "/path/to/repo/data/file"
  new_data_path = file.path(new_repo, 'data', new_data_name)

  # e.g. "/path/to/repo/data/file/conversion-scripts"
  # TODO: conversion-scripts should now be inside derived-data
  # TODO: derived-data also contains sub-directories for 'products'
  # https://github.com/canmod/iidda-tools/issues/10
  scripts_path = file.path(new_data_path, 'conversion-scripts')

  # e.g. "/path/to/repo/data/file/derived-data"
  derived_path = file.path(new_data_path, 'derived-data')

  # e.g. "/path/to/repo/data/file/source-data"
  source_path = file.path(new_data_path, 'source-data')

  # e.g. "/path/to/repo/data/file/source-data/file.xlsx"
  new_source_file = file.path(source_path, base_source_file)

  dir.create(new_data_path)
  dir.create(scripts_path)
  dir.create(derived_path)
  dir.create(source_path)

  file.copy(single_file, new_source_file)

  initialize_iidda_readme(new_data_path, new_data_name, base_source_file, lifecycle)
}

# Make IIDDA Dataset README File
#
initialize_iidda_readme = function(
  new_data_path, new_data_name, base_source_file, lifecycle) {

  stopifnot(lifecycle %in% c('Unreleased', 'Static', 'Dynamic', 'Superseded'))



  readme_path = file.path(new_data_path, 'README.md')
  new_data_name = new_data_name
  source_url = file.path('https://raw.githubusercontent.com',
                         'davidearn/iidda/master/data',
                         new_data_name, 'source-data', base_source_file)

  # strip leading and training slashes
  #dataset_iidda_path = gsub('(^/)(/$)', '', dataset_iidda_path)

  readme_text = sprintf_named(
    iidda::readme_classic_iidda,
    new_data_name = new_data_name,
    source_url = source_url,
    lifecycle = lifecycle)
  con = file(readme_path, 'w')
  cat(readme_text, file = con)
  close(con)
}
