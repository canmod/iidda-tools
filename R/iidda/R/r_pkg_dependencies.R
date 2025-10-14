#' Package Versions
#'
#' @param dir Directory containing the R scripts
#' @param file Name of R file containing the
#'
#' @return Character vector containing the version numbers of packages
#' loaded by `library` commands without any surrounding whitespace that
#' appear in `R` files (recursively) in `dir`
#' @export
pkg_versions = function(dir, file) {
  library_calls = (dir
    |> file.path("*.R")
    |> Sys.glob()
    |> setdiff("r-pkg-check.R")
    |> lapply(readLines)
    |> lapply(grep, pattern = "library\\([a-zA-Z][a-zA-Z0-9._]*\\)", value = TRUE)
    |> unlist()
    |> unique()
  )

  ops = options(conflicts.policy = list(error = FALSE, warn = FALSE))
  on.exit(options(ops))

  for (line in library_calls) eval(parse(text = line))

  info = sessionInfo()
  pkgs = c(names(info$loadedOnly), names(info$otherPkgs))
  vers = (pkgs
    |> lapply(packageVersion)
    |> lapply(as.character)
    |> setNames(pkgs)
    |> unlist()
  )

  dput(vers, file = file)
  return(vers)
}
