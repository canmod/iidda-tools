#' Construct an URL to Download Single Files from GitHub
#'
#' Uses the Raw GitHub API
#'
#' @param user User or Organization of the repo
#' @param repo Repository name
#' @param path Path to the file that you want to obtain
#' @param token OAuth personal access token (only required for private repos)
#' @param branch Name of the branch (defaults to 'master')
#' @export
raw_github = function(user, repo, path, token = NULL, branch = 'master') {
  paste(
    'raw.githubusercontent.com',
    user, repo, branch, path,
    sep = '/')
  # TODO: do we need URLencode/URLdecode for robustness??
}

#between_strings = function(s, start, end) {
#  p = paste0('.*', start, '(.+)', end, '.*')
#  gsub(p, "\\1", s)
#}

#' Convert GitHub URLs into Raw Format (not working)
#'
#' @param urls TODO
#' @param branch TODO
git_path_to_raw_github = function(urls, branch = 'master') {
  parsed_urls = urls %>%
    gsub(pattern = 'https://github.com/', replacement = '') %>%
    strsplit('/')
  users = sapply(parsed_urls, getElement, 1L)
  repos = sapply(parsed_urls, getElement, 2L)
  paths = lapply(parsed_urls, `[`, -c(1, 2)) %>%
    lapply(as.list) %>%
    lapply(do.call, what = file.path) %>%
    sapply(function(x) ifelse(identical(x, character(0)), '', x))
  mapply(raw_github, users, repos, paths, 'master', USE.NAMES = FALSE)
}


blob_to_raw = function(urls) {
  urls %>%
    gsub(pattern = 'github.com', replacement = 'raw.githubusercontent.com') %>%
    gsub(pattern = '/blob/', replacement = '/')
}
