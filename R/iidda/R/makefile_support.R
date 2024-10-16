#' CSV to JSON Files
#'
#' Create a directory of JSON files from a CSV file.
#'
#' @param csv_path Path to the CSV file.
#' @param json_dir Path to the directory for saving the JSON files.
#' @param name_field Name of the field in the CSV file that contains
#' the names for each JSON file. All values in this field must be unique.
#' @param use_extension If there is a column in the CSV file called
#' `extension`, should it be used to produced json filenames of the form
#' `value-in-name-field.value-in-extension-field.json`?
#'
#' @export
csv_to_json_files = function(csv_path, json_dir, name_field, use_extension = FALSE) {
  csv_data = read_data_frame(csv_path) # select(-Unnamed..11)
  data_to_json_files(csv_data, json_dir, name_field, use_extension)
}

#' Data to JSON Files
#'
#' Create a directory of JSON files from a data frame.
#'
#' @param data Data frame
#' @inheritParams csv_to_json_files
#'
#' @export
data_to_json_files = function(data, json_dir, name_field, use_extension = FALSE) {
  csv_list = toJSON(data, auto_unbox = TRUE, pretty = TRUE) |> parse_json()
  if (!dir.exists(json_dir)) dir.create(json_dir, recursive = TRUE)
  for (i in seq_along(csv_list)) {
    filename = csv_list[[i]][[name_field]]
    if (use_extension) {
      filename = sprintf("%s.%s", filename, csv_list[[i]][["extension"]])
    }
    path = sprintf("%s/%s.json", json_dir, filename)
    write_json(csv_list[[i]], path, auto_unbox = TRUE, pretty = TRUE)
  }
}

#' JSON Files to CSV
#'
#' Create a CSV file from a set of JSON files.
#'
#' @param json_paths Vector of paths to JSON files.
#' @param csv_path Path for saving the resulting CSV file.
#'
#' @export
json_files_to_csv = function(json_paths, csv_path) {
  (json_paths
    |> json_files_to_data()
    |> write_csv(file = csv_path)
  )
}

#' JSON Files to Data
#'
#' Create a data frame from a set of JSON files.
#'
#' @inheritParams json_files_to_csv
#'
#' @export
json_files_to_data = function(json_paths) {
  (json_paths
    |> lapply(read_json)
    |> toJSON(auto_unbox = TRUE)
    |> fromJSON()
  )
}

resolve_tracking_table = function(data, type) {
  schema = getOption("iidda_tracking_schema") |> read_json(simplifyVector = TRUE)
  missing_fields = setdiff(schema[[type]], names(data))
  for (field in missing_fields) data[[field]] = ""
  data[, schema[[type]], drop = FALSE]
}

assert_tracking_table = function(data) {
  schema = getOption("iidda_tracking_schema") |> read_json(simplifyVector = TRUE)
  possible_types = (schema
    |> vapply(\(x) identical(x[1:2], names(tab)[1:2]), logical(1L))
    |> which()
    |> names()
  )
  l = length(possible_types)
  if (l == 0L) {
    stop("tracking data columns did not match any known types")
  } else if (l > 1L) {
    stop("something went dredfully wrong")
  }
  resolve_tracking_table(data, possible_types)
}

empty_tracking_table = function(type) {
  schema = getOption("iidda_tracking_schema") |> read_json(simplifyVector = TRUE)
  setNames(
      rep(list(character(0L)), length(schema[[type]]))
    , schema[[type]]
  ) |> as.data.frame()
}

assert_tracking_type = function(data, type) {
  if (!is.data.frame(data)) return(empty_tracking_table(type))
  resolve_tracking_table(data, type)
}

read_dependency_file_lines = function(dataset, pattern) {
  lines = ("dataset-dependencies/%s/%s.d"
    |> sprintf(dataset, dataset)
    |> readLines()
  )
  grep(pattern, lines, value = TRUE)
}

#' Read Resource Metadata
#'
#' @param dataset IIDDA dataset ID.
#' @param pattern Regular expression pattern for filtering candidate paths
#' to be read from.
#'
#' @export
read_resource_metadata = function(dataset, pattern) {
  ## TODO: check that json file exists and give
  ## appropriate message if not
  lines = read_dependency_file_lines(dataset, pattern)
  sprintf("%s.json", lines) |> json_files_to_data()
}

#' Read Column Metadata
#'
#' @inheritParams read_resource_metadata
#' @export
read_column_metadata = function(dataset, pattern) {
  lines = read_dependency_file_lines(dataset, pattern)
  lines |> json_files_to_data()
}

#' Read Prerequisite Paths
#'
#' @inheritParams read_resource_metadata
#' @export
read_prerequisite_paths = function(dataset, pattern) {
  lines = read_dependency_file_lines(dataset, pattern)
  Sys.glob(lines)
}

#' Read Prerequisite Metadata
#'
#' @inheritParams read_resource_metadata
#' @param pattern Regular expression pattern for filtering candidate
#' paths to metadata.
#' @export
read_prerequisite_metadata = function(dataset, pattern) {
  read_resource_metadata(dataset, pattern)
}

#' Read Prerequisite Data
#'
#' @param dataset_id IIDDA dataset ID.
#' @param numeric_column_for_report Optional numeric column name to specify
#' for producing a report using \code{\link{non_numeric_report}}.
#'
#' @export
read_prerequisite_data = function(dataset_id, numeric_column_for_report = NULL) {
  paths = read_prerequisite_paths(dataset_id
    , pattern = "derived-data/[a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\\.csv"
  )
  data_list = list()
  for (csv_path in paths) {
    if (file.exists(csv_path)) {
        original_dataset_id = tools::file_path_sans_ext(basename(csv_path))
        df = (read_data_frame(csv_path)
              %>% mutate(original_dataset_id = original_dataset_id)
              #%>% mutate(dataset_id = source_id)
        )

        #df = mutate(df, across(starts_with("cases"), as.character))

        data_list = append(data_list, list(df))
      } else {
        warning(paste("CSV file does not exist:", csv_path))
      }
  }
  output = bind_rows(data_list)
  if (isFALSE(is_empty(numeric_column_for_report))) {
    non_numeric_report(output, numeric_column_for_report, dataset_id)
  }
  return(output)
}

#' Read Global Metadata
#'
#' @param id ID to the `type` of entity.
#' @param type Type of entity.
#' @export
read_global_metadata = function(id, type = c("columns", "organization", "sources", "tidy-datasets")) {
  sprintf("metadata/%s/%s.json", type, id) |> json_files_to_data()
}

#' Read Lookup
#'
#' @param lookup_id IIDDA ID associated with an item in a `lookup-tables`
#' directory in an IIDDA repository.
#'
#' @export
read_lookup = function(lookup_id) {
  path = file.path("lookup-tables", sprintf("%s.csv", lookup_id))
  trash = fix_csv(path)
  read_data_frame(path)
}
