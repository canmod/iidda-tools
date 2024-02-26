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
#'
#' @export
pipeline_exploration_starter = function(
    script_filename,
    exploration_project_path,
    pipeline_repo_root = getwd()
) {
  if (!dir.exists(exploration_project_path)) {
    dir.create(exploration_project_path, recursive = TRUE)
  }
  script_path = file.path(
    exploration_project_path,
    assert_ext(script_filename, "R")
  )
  path = assert_path_does_not_exist(add_root(script_path, pipeline_repo_root))
  template = system.file("pipeline_exploration_starter.R", package = "iidda")
  file.copy(template, path)
}

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
#' @param root Optional project root.
#' @importFrom tools file_path_sans_ext list_files_with_exts
#'
#' @return List of matching files without their extensions.
#'
#' @export
list_file_id = function(..., ext, root) {
  path = add_root_and_check(file.path(...), root)
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
#' @param root Path to the root of the repository.
#'
#' @export
list_resource_ids = function(source
    , type = c("TidyDatasets", "PrepScripts", "Scans", "Digitizations", "AccessScripts")
    , root
  ) {
  pipeline_dir = add_root_and_check("pipelines", root)
  file = set_ext(match.arg(type), "csv")
  read.csv(file.path(pipeline_dir, source, "tracking", file))[[1L]]
}

#' List Dependency IDs
#'
#' @param source Source ID.
#' @param dataset Dataset ID.
#' @param type Type of resource.
#' @param root Path to the root of the repository.
#'
#' @export
list_dependency_ids = function(source, dataset
    , type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
    , root
  ) {
  type = switch(match.arg(type)
    , PrepScripts = "PrepDependencies"
    , Scans = "ScanDependencies"
    , Digitizations = "DigitizationDependencies"
    , AccessScripts = "AccessDependencies"
  )
  pipeline_dir = add_root_and_check("pipelines", root)
  file = set_ext(type, "csv")
  tracking = read.csv(file.path(pipeline_dir, source, "tracking", file))
  tracking[[1L]][tracking[[2L]] == dataset]
}

#' List Dependency Paths
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#' @param type Type of resource.
#' @param root Path to the root of the repository.
#'
#' @export
list_dependency_paths = function(source, dataset
    , type = c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
    , root
  ) {
  dependencies = list_dependency_ids(source, dataset, type, root)
  pipeline_dir = add_root_and_check("pipelines", root)
  file = set_ext(type, "csv")
  tracking = read.csv(file.path(pipeline_dir, source, "tracking", file))
  urls = tracking[[grep("^path_[a-z]+", names(tracking))]][tracking[[1L]] %in% dependencies]
  vapply(strip_blob_github(urls), add_root_and_check, character(1L), USE.NAMES = FALSE)
}

#' List Source IDs
#'
#' @param root Path to the root of the repository.
#' @export
list_source_ids = function(root) list.files(add_root_and_check("pipelines", root))

#' List Dataset IDs by Source
#'
#' @param root Path to the root of the repository.
#' @export
list_dataset_ids_by_source = function(root) {
  sources = list_source_ids(root)
  sapply(sources
    , list_dataset_ids
    , root = root
    , simplify = FALSE
  )
}

#' List Dataset IDs
#'
#' @param source Source ID.
#' @param root Path to the root of the repository.
#'
#' @export
list_dataset_ids = function(source, root) {
  list_resource_ids(source, "TidyDatasets", root)
}

#' List Prep Script IDs
#'
#' @param source Source ID.
#' @param root Path to the root of the repository.
#'
#' @export
list_prep_script_ids = function(source, root) {
  list_resource_ids(source, "PrepScripts", root)
}


#' Get Main Script
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#' @param root Path to the root of the repository.
#'
#' @export
get_main_script = function(source, dataset, root) {
  assert_string(
    list_dependency_paths(source, dataset, "PrepScripts", root),
    "prep script"
  )
}

#' Get all Dependencies
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#' @param root Path to the root of the repository.
#'
#' @export
get_all_dependencies = function(source, dataset, root) {
  unlist(lapply(c("PrepScripts", "Scans", "Digitizations", "AccessScripts")
    , list_dependency_paths
    , source = source
    , dataset = dataset
    , root = root
  ))
}

#' Get Dataset path
#'
#' @param source Source ID.
#' @param dataset dataset ID.
#' @param root Path to the root of the repository.
#' @param ext Dataset file extension.
#'
#' @export
get_dataset_path = function(source, dataset, root, ext = "csv") {
  path = file.path(
    "derived-data",
    source,
    dataset,
    set_ext(assert_dataset(dataset, source, root), ext)
  )

  # don't check if the resulting path exists because
  # the dataset may not have been created yet
  add_root(path, root)
}

#' Get Source Path
#'
#' @param source Source ID.
#' @param root Path to the root of the repository.
#'
#' @export
get_source_path = function(source, root) {
  add_root(
    file.path(
      "pipelines",
      source
    ),
    root
  )
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

assert_dataset = function(dataset, source, root) {
  datasets = list_resource_ids(source, "TidyDatasets", root)
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


add_root_and_check = function(path, root) {
  assert_path(add_root(path, root))
}

add_root = function(path, root) {
  if (!missing(root)) {
    return(file.path(assert_string(root, "root directory"), path))
  }
  path
}
