#' Pipeline Exploration Quick-Start
#'
#' Create an R script providing a place to start when exploring an IIDDA
#' pipeline.
#'
#' The R script has the following:
#'
#' 1. Example code for printing out the data sources and datasets
#' in the IIDDA pipeline repository.
#' 2. Code for finding the paths to datasets and to the scripts for generating
#' them.
#' 3. Code for generating and/or reading in a user-selected IIDDA
#' dataset.
#'
#' Once the data are read in, the user is free to do whatever they want to
#' with it.
#'
#' @param script_filename Name for the generated script.
#' @param exploration_project_path Path to the folder for containing the script.
#' If this path doesn't exist, then it is created. If \code{script_filename}
#' exists in \code{exploration_project_path}, an error is returned.
#' @param pipeline_repo_root Path to the folder of a cloned IIDDA pipeline
#' repository.
#' @param ... Additional arguments to pass to \code{\link{file.copy}}. A
#' useful argument here is `overwrite`, which indicates whether an existing
#' exploration script should be overwritten.
#'
#' @export
pipeline_exploration_starter = function(
    script_filename,
    exploration_project_path,
    ...
) {
  script_path = file.path(
    exploration_project_path,
    assert_ext(script_filename, "R")
  )
  path = assert_path_does_not_exist(proj_path(script_path))
  template = system.file("pipeline_exploration_starter.R", package = "iidda")
  file.copy(template, path, ...)
}

#' Project Root
#'
#' Find the root path of an IIDDA-associated project (or any project
#' with a file of a specific name in the root).
#'
#' Recursively walk up the file tree from `start_dir` until `filename` is
#' found, and return the path to the directory containing `filename`. If
#' `filename` is not found, return `default_root`
#'
#' @param filename String giving the name of the file that identifies the project.
#' @param start_dir Optional directory from which to start looking for `filename`.
#' @param default_root Project root to use if `filename` is not found.
#'
#' @export
proj_root <- function(filename = ".iidda", start_dir = getwd(), default_root = start_dir) {
  # Check if the file exists in the current directory
  if (file.exists(file.path(start_dir, filename))) return(start_dir)

  # If we have reached the root directory, return the original starting directory
  parent_dir <- normalizePath(file.path(start_dir, ".."))
  if (identical(parent_dir, start_dir)) return(default_root)

  # Recursively search in the parent directory
  Recall(filename, parent_dir, default_root)
}

#' @describeIn proj_root Is a particular directory inside a project as
#' indicated by `filename`.
#' @export
in_proj <- function(filename = ".iidda", start_dir = getwd()) {
  # Normalize the directory path
  start_dir <- normalizePath(start_dir, mustWork = TRUE)

  # Check if the file exists in the current directory
  # and if it does that it is a file and not a directory itself.
  path = file.path(start_dir, filename)
  if (file.exists(path) & isFALSE(file.info(path)$isdir)) return(TRUE)

  # Get the parent directory
  parent_dir <- dirname(start_dir)

  # If the current directory is the root, stop the recursion
  if (parent_dir == start_dir) return(FALSE)

  # Recur with the parent directory using Recall
  Recall(filename, parent_dir)
}

#' @export
in_git_repo = function() {
  system("git rev-parse --is-inside-work-tree", intern = TRUE) == "true"
}

#' @export
remote_iidda_git = function() {
  if (!in_proj()) stop("Not in an iidda data repository.")
  if (!in_git_repo()) stop("Not inside a git repository.")
  resp = system("git config --get remote.origin.url", intern = TRUE)
  if (length(resp) == 0L) stop("Repository does not have a remote origin url.{")
  resp = sub("^.*:", "", resp)
  user = basename(dirname(resp))
  repo = tools::file_path_sans_ext(basename(resp))
  sprintf("https://github.com/%s/%s", user, repo)
}


is_absolute_path <- function(path) {
  # Determine the OS type
  os_type <- .Platform$OS.type

  if (os_type == "windows") {
    # Check for Windows absolute path (e.g., "C:/path")
    return(grepl("^[a-zA-Z]:", path))
  } else {
    # Assume Unix-like system, check for Unix absolute path (e.g., "/path")
    return(grepl("^/", path))
  }
}

to_relative_path = function(path, containing_path) {
  path = proj_path(path) |> strip_trailing_slash()
  containing_path = proj_path(containing_path) |> strip_trailing_slash()
  if (!startsWith(path, containing_path)) {
    stop("Containing path does not contain the path")
  }
  sub(sprintf("^%s/", containing_path), "", path)
}

#' @export
relative_paths = function(paths, containing_path = proj_root()) {
  vapply(paths
    , to_relative_path
    , character(1L)
    , containing_path
    , USE.NAMES = FALSE
  )
}

#' @export
proj_path = function(...) {
  path = file.path(...)
  if (is_absolute_path(path)) return(path)
  file.path(proj_root(), path)
}

assert_proj_path = function(path) assert_path(proj_path(path))

#' Set Extension
#'
#' @param paths Character vector giving file paths.
#' @param ext String giving the file extension to add to the \code{paths}.
#'
#' @export
set_ext = function(paths, ext) {
  if (nchar(assert_string(ext, "extension")) == 0L) {
    return(strip_trailing_slash(paths))
  }
  paste(strip_trailing_slash(paths), ext, sep = ".")
}

#' List File ID
#'
#' @param ... Path components to directory containing the resources.
#' @param ext Optional string giving the file extension of the resources. If
#' missing then all resources are given.
#' @importFrom tools file_path_sans_ext list_files_with_exts
#'
#' @return List of matching files without their extensions.
#'
#' @export
list_file_id = function(..., ext) {
  path = assert_proj_path(file.path(...))
  if (missing(ext)) {
    files = list.files(path)
  } else {
    files = tools::list_files_with_exts(path, ext, full.names = FALSE)
  }
  tools::file_path_sans_ext(files)
}

#' List Resources IDs
#'
#' @param source Source ID.
#' @param type Type of resource.
#'
#' @export
list_resource_ids = function(source
    , type = c("TidyDatasets", "PrepScripts", "Scans", "Digitizations", "AccessScripts")
  ) {
  pipeline_dir = assert_proj_path("pipelines")
  file = set_ext(match.arg(type), "csv")
  read.csv(file.path(pipeline_dir, source, "tracking", file))[[1L]]
}

get_dependency_csv = function(source
    , type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
  ) {
  type = switch(match.arg(type)
    , PrepScripts = "PrepDependencies"
    , Scans = "ScanDependencies"
    , Digitizations = "DigitizationDependencies"
    , AccessScripts = "AccessDependencies"
  )
  pipeline_dir = assert_proj_path("pipelines")
  file = set_ext(type, "csv")
  read.csv(file.path(pipeline_dir, source, "tracking", file))
}

#' List Dependency IDs
#'
#' @param source Source ID.
#' @param dataset Dataset ID.
#' @param type Type of resource.
#'
#' @export
list_dependency_ids = function(source, dataset
    , type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
  ) {
  tracking = get_dependency_csv(source, type)
  tracking[[1L]][tracking[[2L]] == dataset]
}

#' @export
list_dependency_ids_for_source = function(source
    , type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
  ) {
  tracking = get_dependency_csv(source, type)
  tracking[[1L]]
}

#' @export
list_prerequisite_paths = function(source, dataset) {
  pipeline_dir = assert_proj_path("pipelines")
  file = "Prerequisites.csv"
  path = file.path(pipeline_dir, source, "tracking", file)
  if (!file.exists(path)) return(character())
  tracking = read.csv(path)
  i = tracking[["tidy_dataset"]] == dataset
  pre_source = tracking[["prerequisite_source"]][i]
  pre_dataset = tracking[["prerequisite_dataset"]][i]
  file = vapply(pre_dataset, set_ext, character(1L), "csv", USE.NAMES = FALSE)
  file.path("derived-data", pre_source, pre_dataset, file)
}

#' List Dependency Paths
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#' @param type Type of resource.
#'
#' @export
list_dependency_paths = function(source, dataset
    , type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
  ) {
  dependencies = list_dependency_ids(source, dataset, type)
  pipeline_dir = assert_proj_path("pipelines")
  file = set_ext(type, "csv")
  tracking = read.csv(file.path(pipeline_dir, source, "tracking", file))
  urls = tracking[[grep("^path_[a-z]+", names(tracking))]][tracking[[1L]] %in% dependencies]
  vapply(strip_blob_github(urls), assert_proj_path, character(1L), USE.NAMES = FALSE)
}


#' List Source IDs
#'
#' @export
list_source_ids = function() list.files(assert_proj_path("pipelines"))

#' List Dataset IDs by Source
#'
#' @export
list_dataset_ids_by_source = function() {
  sources = list_source_ids()
  sapply(sources
    , list_dataset_ids
    , simplify = FALSE
  )
}

#' List Dataset IDs
#'
#' @param source Source ID.
#'
#' @export
list_dataset_ids = function(source) list_resource_ids(source, "TidyDatasets")

#' List Prep Script IDs
#'
#' @param source Source ID.
#'
#' @export
list_prep_script_ids = function(source) list_resource_ids(source, "PrepScripts")


#' Get Main Script
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#'
#' @export
get_main_script = function(source, dataset) {
  assert_string(
    list_dependency_paths(source, dataset, "PrepScripts"),
    "prep script"
  )
}

#' Get all Dependencies
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#'
#' @export
get_all_dependencies = function(source, dataset) {
  pipeline_deps = unlist(lapply(c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
    , list_dependency_paths
    , source = source
    , dataset = dataset
  ))
  prerequisite_deps = list_prerequisite_paths(source, dataset)
  return(c(pipeline_deps, prerequisite_deps))
}



#' Get Dataset path
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#' @param ext Dataset file extension.
#'
#' @export
get_dataset_path = function(source, dataset, ext = "csv") {
  path = file.path(
    "derived-data",
    source,
    dataset,
    set_ext(assert_dataset(dataset, source), ext)
  )

  # don't check if the resulting path exists because
  # the dataset may not have been created yet
  proj_path(path)
}

#' Get Source Path
#'
#' @param source Source ID.
#'
#' @export
get_source_path = function(source) {
  proj_path(file.path("pipelines", source))
}


strip_trailing_slash = function(paths) {
  paths = as.character(paths)
  d = dirname(paths)
  b = basename(paths)
  if (b == paths) {
    return(b)
  } else if (d == "/") {
    return(paste(d, b, sep = ""))
  }
  file.path(d, b)
}

assert_string = function(x, thing) {
  x = as.character(x)
  if (length(x) == 0L) stop("Could not find a single ", thing)
  if (length(x) != 1L) {
      stop(
        "Can only work with one ",
        thing,
        " at a time.",
        "You may need to call this function more than once."
      )
  }
  x
}

assert_source = function(source, pipeline_dir) {
  if (!assert_string(source, "source") %in% list.files(assert_dir(pipeline_dir))) {
    stop("Source, ", source, ", is not found in ", pipeline_dir)
  }
  source
}

assert_dataset = function(dataset, source) {
  datasets = list_resource_ids(source, "TidyDatasets")
  if (!assert_string(dataset, "dataset") %in% datasets) {
    stop("Cannot find the dataset, ", dataset)
  }
  dataset
}

assert_dir = function(path) {
  if (!isTRUE(all(dir.exists(assert_string(path, "path"))))) {
    stop("Path ", path, " is not a directory")
  }
  path
}

assert_path = function(path) {
  if (!isTRUE(all(file.exists(assert_string(path, "path"))))) {
    stop("Path ", path, " does not exist")
  }
  path
}

assert_path_does_not_exist = function(path) {
  if (file.exists(path)) {
    stop("Path ", path, " already exists")
  }
  path
}

assert_paths = function(paths) vapply(paths, assert_path, character(1L))

assert_ext = function(file, ext) {
  if (tools::file_ext(file) != ext) {
    stop("File ", file, " does not have the following extension: ", ext)
  }
  file
}


#' Prep Script Outcomes
#'
#' @return Data frame with all prep script outcomes in the project.
#' @export
all_prep_script_outcomes = function() {
  pipeline_dir = assert_proj_path("pipelines")
  all_outcomes = list.files(pipeline_dir, recursive = TRUE, pattern = "PrepScriptOutcomes.csv", full.names = TRUE)
  lapply(all_outcomes, iidda::read_data_frame) |> dplyr::bind_rows()
}

#' @describeIn all_prep_script_outcomes Data frame with all successful prep
#' script outcomes
#' @export
successful_prep_script_outcomes = function() {
  dplyr::filter(all_prep_script_outcomes(), execution_status == "succeeded")
}

#' @describeIn all_prep_script_outcomes Data frame with all failed prep
#' script outcomes
#' @export
failed_prep_script_outcomes = function() {
  dplyr::filter(all_prep_script_outcomes(), execution_status == "failed")
}

#' @param tar_name Name of a tar archive to be created with log files of failed
#' prep script outcomes.
#' @describeIn all_prep_script_outcomes Tar archive with log files of failed
#' prep script outcomes.
#' @export
error_tar = function(tar_name) {
  d = tempdir()
  file.copy(failed_prep_script_outcomes()$log_file_path, d)
  file.copy(failed_prep_script_outcomes()$err_file_path, d)
  cwd = getwd()
  on.exit({setwd(cwd)})
  tar_path = file.path(path.expand(cwd), tar_name)
  f = list.files(d, pattern = "*.log", full.names = FALSE)
  setwd(d)
  tar(tarfile = tar_path, files = f)
}


