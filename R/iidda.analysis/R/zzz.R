.onLoad <- function(lib, pkg) {
  options(
      iidda_try_pull_dict = TRUE
    ## FIXME: do we need these anymore?
    , iidda_owner = "canmod"
    , iidda_repo = "iidda-staging"
  )
  cache_data_dictionary()
}
