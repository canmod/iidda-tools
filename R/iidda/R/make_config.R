#' Create IIDDA Config File
#'
#' @param path path for storing config file
#' @param iidda_owner TODO
#' @param iidda_repo TODO
#' @param github_token TODO
#' @param .overwrite should existing config.json files be overwritten
#'
#' @importFrom jsonlite toJSON write_json
#' @export
make_config = function(
  path = file.path(getwd(), 'config.json'),
  iidda_owner = '',
  iidda_repo = '',
  github_token = '',
  .overwrite = FALSE
) {
  config = as.list(environment())
  if (!.overwrite & file.exists(path)) {
    stop('there is an existing config.json file. consider using .overwrite = TRUE')
  } else {
    config$.overwrite = NULL
    write_json(config, path, auto_unbox = TRUE, pretty = TRUE)
  }
  invisible()
}
