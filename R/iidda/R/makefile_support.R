#' @export
csv_to_json_files = function(csv_path, json_dir, name_field, use_extension = FALSE) {
  csv_data = read_data_frame(csv_path) # select(-Unnamed..11)
  csv_list = toJSON(csv_data, auto_unbox = TRUE, pretty = TRUE) |> parse_json()
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

#' @export
json_files_to_csv = function(json_paths, csv_path) {
  (json_paths
    |> json_files_to_data()
    |> write_csv(file = csv_path)
  )
}

#' @export
json_files_to_data = function(json_paths) {
  (json_paths
    |> lapply(read_json)
    |> toJSON(auto_unbox = TRUE) |> fromJSON()
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

#' @export
read_resource_metadata = function(dataset, pattern) {
  ## TODO: check that json file exists and give
  ## appropriate message if not
  lines = read_dependency_file_lines(dataset, pattern)
  sprintf("%s.json", lines) |> json_files_to_data()
}

#' @export
read_column_metadata = function(dataset, pattern) {
  lines = read_dependency_file_lines(dataset, pattern)
  lines |> json_files_to_data()
}

#' @export
read_prerequisite_paths = function(dataset, pattern) {
  lines = read_dependency_file_lines(dataset, pattern)
  Sys.glob(lines)
}

#' @export
read_prerequisite_metadata = function(dataset, derived_pattern, metadata_pattern, column_pattern) {
  read_resource_metadata(dataset, metadata_pattern)
}

#' @export
read_prerequisite_data = function(dataset_id) {
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
  return(bind_rows(data_list))
}

#' @export
read_global_metadata = function(id, type) {
  sprintf("metadata/%s/%s.json", type, id) |> json_files_to_data()
}
