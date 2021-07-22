#' Create New IIDDA Dataset from Single File
#'
#' @param classic_file path to single data file
#' @param new_repo path to new IIDDA repository
#' @return No return value. Call to produce a new directory structure in a new
#' IIDDA git repository containing a single source data file.
iidda_from_single_file = function(classic_file, new_repo) {
  iidda_extensions = c('csv', 'xls', 'xlsx', 'pdf')
  ext_pat =
    paste0(
      '\\.(',
      paste0(iidda_extensions, collapse = '|'),
      ')$',
      collapse = '')

  base_source_file = basename(classic_file)
  new_data_name = sub(ext_pat, '', base_source_file, ignore.case = TRUE)
  new_data_path = file.path(new_repo, 'data', new_data_name)
  scripts_path = file.path(new_data_path, 'conversion-scripts')
  derived_path = file.path(new_data_path, 'derived-data')
  source_path = file.path(new_data_path, 'source-data')
  new_source_file = file.path(source_path, base_source_file)

  dir.create(new_data_path)
  dir.create(scripts_path)
  dir.create(derived_path)
  dir.create(source_path)

  file.copy(classic_file, new_source_file)
}
