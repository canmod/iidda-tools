#' Register Prep Script
#'
#' Convenience function for a one-time setup of all metadata required for
#' a new prep script. The assumptions are that (1) the prep script is a `.R`
#' file in the `prep-scripts` directory of a directory within the `pipelines`
#' directory and (2) that this script produces a csv file in the
#' `derived-datasets` directory with the same `basename()` as this `.R` file.
#' Messages are printed with paths to newly created and/or existing metadata,
#' derived data, and dependency files that should be checked manually. Sometimes
#' it is helpful to delete some of these files and rerun `register_prep_script`.
#' However, this `register_prep_script` function should not be used in a
#' script that is intended to be run multiple times, as going forward the
#' metadata and dependency files should be edited manually.
#'
#' @param script_path Path to the prep-script being registered.
#' @param type Type of the dataset being produced (e.g., CDI, Mortality).
#' TODO: Give a list of acceptable values. Should be programmatically produced.
#' @export
register_prep_script = function(script_path, type) {
  if (length(script_path) != 1L) stop("exactly one path must be in a prep-scripts directory within pipelines")
  if (!file.exists(script_path)) stop("prep script file does not exist")
  if (tools::file_ext(script_path) != "R") stop("can only automate registration of prep scripts written in r")
  script_id = dataset_id = (script_path
    |> basename()
    |> tools::file_path_sans_ext()
  )
  derived_path = sprintf("derived-data/%s.csv", dataset_id)
  lines = readLines(script_path)
  p = data.frame(x = character())
  quoted_frame = strcapture('"([^"]+)"', lines, proto = p)
  quoted = (quoted_frame
    |> unlist(use.names = FALSE)
    |> na.omit()
    |> unique()
  )
  if (!derived_path %in% quoted) {
    stop(
        "expecting that the script will write a csv file to the following path, "
      , "but this path is not mentioned in the script:\n"
      , derived_path
    )
  }
  mentioned_files = quoted[file.exists(quoted)]
  dep_paths = setdiff(mentioned_files, derived_path)
  res_paths = mentioned_files[startsWith(mentioned_files, "pipelines")]
  source_ids = (res_paths
    |> c(script_path)
    |> dirname()
    |> dirname()
    |> basename()
    |> unique()
  )

  message("-----------------------------------------------------")
  message("please edit or remove the following files as required")
  message("-----------------------------------------------------")

  if (file.exists(derived_path)) {
    message(
        "the following dataset already exists so previously saved data is being used to inform the metadata:\n    "
      , derived_path
    )
    message()
  } else {
    message(
        "creating the following dataset to potentially learn about metadata from the data:\n    "
      , derived_path
    )
    data_env = new.env(parent = parent.frame())
    sys.source(script_path, envir = data_env)
    rm(list = ls(data_env), envir = data_env)
  }
  data = read_data_frame(derived_path)
  fields = names(data)
  valid_fields = ("metadata/columns"
    |> list.files()
    |> tools::file_path_sans_ext()
  )
  invalid_fields = setdiff(fields, valid_fields)
  if (length(invalid_fields) > 0L) {
    stop(
        "data contain the following fields that are not declared "
      , "in the metadata/columns directory. please choose one of "
      , "the existing fields. if you really must, please add metadata "
      , "for new fields to metadata/columns:\n"
      , paste(invalid_fields, collapse = "\n")
    )
  }
  field_paths = sprintf("metadata/columns/%s.json", fields)
  date_fields = c("period_start_date", "period_end_date")
  if (all(date_fields %in% fields)) {
    periods = data[date_fields] |> unlist() |> range()
  } else {
    periods = rep("", times = 2L)
  }

  dependencies = c(script_path, dep_paths, field_paths)

  for (id in source_ids) {
    .trash = make_source_metadata(id, "", "")
    .trash = make_resource_metadata(id)
  }
  .trash = make_dataset_metadata(dataset_id
    , type = type
    , period_start_date = periods[1L]
    , period_end_date = periods[2L]
  )
  .trash = make_dataset_dependencies(dataset_id, dependencies)
}



glob_ids = function(paths) {
  Sys.glob(paths) |> basename() |> tools::file_path_sans_ext()
}
column_paths = function(...) sprintf("metadata/columns/%s.json", c(...))

default_metadata = function(type) {
  tracking_schema = read_json("global-metadata/tracking-schema.json", simplifyVector = TRUE)
  metadata_schema = (tracking_schema[c("PrepScripts", "Scans", "Digitizations", "AccessScripts")])
  names(metadata_schema) = (vapply(metadata_schema, getElement, character(1L), 1L)
    |> unname()
    |> sprintf(fmt = "%ss")
    |> gsub(pattern = "_", replacement = "-")
  )
  default = metadata_schema[[type]]
  setNames(rep(list(""), length(default)), default)
}

valid_metadata_path = function(filepath, pattern) {
  valid_filepath = grepl(pattern, filepath)
  if (!valid_filepath) stop("invalid metadata filepath generated: ", filepath)
}
check_and_save_metadata = function(fields, metadata, filepath) {
  invalid_fields = setdiff(names(fields), names(metadata))
  if (length(invalid_fields) > 0L) {
    stop(
        "The following invalid json metadata fields were supplied:\n"
      , paste(invalid_fields, collapse = " ")
    )
  }
  metadata[names(fields)] = fields
  if (file.exists(filepath)) {
    message("metadata already exists at: ", filepath)
  } else {
    message("creating new metadata at: ", filepath)
    write_json(metadata, filepath, pretty = TRUE, auto_unbox = TRUE)
  }
  return(metadata)
}

build_metadata_filepath = function(name, type) {
  dirpath = file.path("metadata", type)
  filename = sprintf("%s.json", name)
  filepath = file.path(dirpath, filename)
  dir.create(dirpath, recursive = TRUE, showWarnings = FALSE)
  return(filepath)
}

#' Make Resource Metadata
#'
#' Make one json metadata file for each resource (i.e., prep/access script or
#' digitization/scan of data)) in a source pipeline associated with a
#' data source (i.e., a sub-directory of `pipelines`). Existing metadata
#' files will not be overwritten.
#'
#' @param source Source ID.
#' @export
make_resource_metadata = function(source) {
  types = c("prep-scripts", "access-scripts", "scans", "digitization")
  for (type in types) make_resource_metadata_type(source, type)
}


make_resource_metadata_type = function(source, type) {
  type_col = gsub("-", "_", gsub("s$", "", type))
  resource_paths = (file.path("pipelines", source, type, "*")
    |> Sys.glob()
    |> grep(pattern = "[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+$", invert = TRUE, value = TRUE)
  )
  extensions = tools::file_ext(resource_paths)
  metadata_paths = sprintf("%s.json", resource_paths)
  resource_ids = resource_paths |> basename() |> tools::file_path_sans_ext()
  default = default_metadata(type)
  pat = sprintf("^pipelines/[a-zA-Z0-9_-]+/%s/[a-zA-Z0-9_-]+.[a-zA-Z0-9_-]+.json$", type)
  for (i in seq_along(resource_ids)) {
    valid_metadata_path(metadata_paths[i], pat)
    check_and_save_metadata(
        fields = list(resource_ids[i], source, extensions[i]) |> setNames(c(type_col, "source", "extension"))
      , metadata = default
      , filepath = metadata_paths[i]
    )
  }
}

## FIXME: not used?
make_one_resource_metadata = function(resource, type) {
  type_col = gsub("-", "_", gsub("s$", "", type))

  resource_path = (file.path("pipelines", "*", type, sprintf("%s.*", resource))
    |> Sys.glob()
    |> grep(pattern = "[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+$", invert = TRUE, value = TRUE)
  )
  source = resource_path |> dirname() |> dirname() |> basename()
  extension = tools::file_ext(resource_path)
  metadata_path = sprintf("%s.json", resource_path)
  resource_id = resource_path |> basename() |> tools::file_path_sans_ext()
  stopifnot(identical(resource_id, resource))
  default = default_metadata(type)

  check_and_save_metadata(
      fields = list(resource_id, source, extension) |> setNames(c(type_col, "source", "extension"))
    , metadata = default
    , filepath = metadata_path
  )
}

#' Make Source Metadata
#'
#' Make a json file associated with a new data source (i.e., a sub-directory
#' of `pipelines`).
#'
#' @param source Source ID.
#' @param organization Organization from which the source was obtained.
#' @param location Location for which data was collected.
#' @param ... Additional metadata fields to provide. If invalid fields are
#' supplied, an error message will be given.
#' @export
make_source_metadata = function(source, organization, location, ...) {
  fields = list(...)
  filepath = build_metadata_filepath(source, "sources")
  metadata = list(source = source
    , organization = organization
    , type = ""
    , location = location
    , years = ""
    , breakdown = ""
    , urls = ""
    , date_of_url_access = ""
    , notes = ""
  )
  pat = "^metadata/sources/[a-zA-Z0-9_-]+.json$"
  valid_metadata_path(filepath, pat)
  check_and_save_metadata(fields, metadata, filepath)
}

#' Make Source Directory
#'
#' Make a sub-directory of `pipelines` containing a data and/or code source.
#'
#' @param source Source ID.
#' @param files Character vector of files that are either already in the
#' pipeline or that should be added.
#' @export
make_source_directory = function(source, files) {
  source_dir = file.path("pipelines", source)
  dir.create(source_dir, showWarnings = FALSE)

  source_files = file.path(source_dir, files)
  source_subdirs = dirname(source_files)
  lapply(source_subdirs, dir.create, showWarnings = FALSE)
  for (file in source_files) {
    if (!file.exists(file)) {
      file.create(file, showWarnings = FALSE)
    }
  }
  make_resource_metadata(source)
  return(source_dir)
}

#' Make Dataset Metadata
#'
#' @param tidy_dataset Dataset ID for which metadata is being produced.
#' @param type Type of dataset (e.g., CDI, Mortality).
#' @param ... Additional metadata fields to provide. If invalid fields are
#' supplied, an error message will be given.
#' @export
make_dataset_metadata = function(tidy_dataset, type, ...) {
  fields = list(...)
  filepath = build_metadata_filepath(tidy_dataset, "tidy-datasets")
  metadata = list(
      tidy_dataset = tidy_dataset
    , type = type
    , period_start_date = ""
    , period_end_date = ""
    , title = ""
    , description = ""
    , publisher = "unpublished"
    , publicationYear = "unpublished"
    , current_version = "unversioned"
  )
  pat = "^metadata/tidy-datasets/[a-zA-Z0-9_-]+.json$"
  valid_metadata_path(filepath, pat)
  check_and_save_metadata(fields, metadata, filepath)
}

#' Make Dataset Dependencies
#'
#' Create a dependency file for a dataset. This file is created once and
#' any edits should be made manually to the created file.
#'
#' @param tidy_dataset Dataset ID for which dependencies are being
#' declared.
#' @param paths Relative paths to dependencies.
#' @export
make_dataset_dependencies = function(tidy_dataset, paths) {
  dataset_dir = file.path("dataset-dependencies", tidy_dataset)
  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
  dep_file = sprintf("%s.d", tidy_dataset)
  dep_filepath = file.path(dataset_dir, dep_file)
  pat = "^dataset-dependencies/[a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+.d$"
  valid_metadata_path(dep_filepath, pat)
  if (file.exists(dep_filepath)) {
    message("updating dependency file that already exists at: ", dep_filepath)
  } else {
    message("creating new dependency file at: ", dep_filepath)
    file.create(dep_filepath, showWarnings = FALSE)
  }
  dep_lines = (dep_filepath
    |> readLines()
    |> c(paths)
    |> unique()
  )
  .trash = writeLines(dep_lines, dep_filepath)
  return(dep_lines)
}

#' Make Compilation Dependencies
#'
#' Create a dependency file and prep script for a dataset that is a compilation
#' of other datasets. These files are created once and any edits should be
#' made manually to the created files.
#'
#' @param compilation_dataset Dataset ID for which dependencies are being
#' declared.
#' @param dataset_paths Relative paths to dependencies.
#' @export
make_compilation_dependencies = function(compilation_dataset, dataset_paths) {
  prep_script = sprintf("pipelines/*/prep-scripts/%s.R", compilation_dataset) |> Sys.glob()
  columns = (dataset_paths
    |> lapply(iidda::read_data_columns)
    |> unlist()
    |> unique()
    |> column_paths()
  )
  lines = readLines(prep_script)
  if (all(is_empty(lines))) {
    lines = c(
        "# -----------------------------"
      , sprintf("dataset_id = '%s'", compilation_dataset)
      , "# -----------------------------"
      , ""
      , "library(iidda)"
      , ""
      , "metadata = get_dataset_metadata(dataset_id)"
      , "data = read_prerequisite_data(dataset_id)"
      , "write_tidy_data(data, metadata)"
    )
    writeLines(lines, prep_script)
  }
  make_dataset_dependencies(compilation_dataset, c(prep_script, dataset_paths, columns))
}
