#' Construct an URL to Download Single Files from GitHub
#'
#' Uses the Raw GitHub API
#'
#' @param owner User or Organization of the repo
#' @param repo Repository name
#' @param path Path to the file that you want to obtain
#' @param user Your username (only required for private repos)
#' @param token OAuth personal access token (only required for private repos)
#' @param branch Name of the branch (defaults to 'master')
#' @export
raw_github = function(owner, repo, path, user = NULL, token = NULL, branch = 'master') {
  output = paste(
    'raw.githubusercontent.com',
    user, repo, branch, path,
    sep = '/')
  return(output)
  login_query = ""
  if(!is.null(user)) {
    stopifnot(!is.null(token))
    login_query = sprintf_named(
      "?login=%{user}s&token=%{token}s",
      user = user, token = token)
  }

  #curl -H 'Authorization: token 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' -H 'Accept: application/vnd.github.v3.raw' -O -L https://api.github.com/repos/davidearn/data_work/contents/README.md

  #"https://api.github.com/repos/davidearn/data_work/contents/README.md?token=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"

  url_template = paste0(
    "https://raw.github.com/%{owner}s/%{repo}s/",
    "%{branch}s/%{path}s%{login_query}s", sep = '')
  sprintf_named(url_template, owner = owner, repo = repo, branch = branch,
                path = rm_leading_slash(path),
                login_query = login_query)
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

#' @export
blob_to_raw = function(urls) {
  urls %>%
    gsub(pattern = 'github.com', replacement = 'raw.githubusercontent.com') %>%
    gsub(pattern = '/blob/', replacement = '/')
}

#' @export
strip_blob_github = function(urls) {
  ## https://stackoverflow.com/a/64147124/2047693
  sub("^https://github.com/[A-z0-9\\.-]+/[A-z0-9\\.-]+/blob/(main|master)/", "", urls)
}

#' @export
strip_raw_github = function(urls) {
  stop('todo ... but see strip_blob_github')
}
