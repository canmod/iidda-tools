#' Mock API Hook
#'
#' @param repo_path Path to an IIDDA repository.
#'
#' @export
mock_api_hook = function(repo_path) {
  list(
      raw_csv = function(dataset_ids, ...) {
        paths = sprintf("derived-data/%s/%s.csv", dataset_ids, dataset_ids)
        (repo_path
          |> file.path(paths)
          |> lapply(read_data_frame)
          |> lapply(parse_columns, mock_api_hook(repo_path)$data_dictionary())
          |> setNames(dataset_ids)
          |> dplyr::bind_rows(.id = "dataset_id")
        )
      }
    , lookup_tables = function(lookup_type = "location") {
        path = sprintf("lookup-tables/%s.csv", lookup_type)
        (repo_path
          |> file.path(path)
          |> read_data_frame()
        )
      }
    , metadata = function(...) {
        stop("need to use the real api when getting metadata. cannot use this mock api")
    }
    , data_dictionary = function() {
        paths = (repo_path
          |> file.path("metadata/columns/*.json")
          |> Sys.glob()
        )
        setNames(
            lapply(paths, jsonlite::read_json)
          , tools::file_path_sans_ext(basename(paths))
        )
    }
  )
}

#' Parse Columns
#'
#' Set the types of a dataset with all character-valued columns using a
#' data dictionary that defines the types.
#'
#' @param data Data frame with all character-valued columns.
#' @param data_dictionary List of lists giving a data dictionary.
#'
#' @export
parse_columns = function(data, data_dictionary) {
  dict = data_dictionary
  if (!all(names(data) %in% names(dict))) return(data)
  for (cc in names(data)) {
    if (dict[[cc]]$type == "string" & dict[[cc]]$format == "num_missing") {
      data[[cc]] = readr::parse_number(data[[cc]])
    }
    if (dict[[cc]]$type == "date" & dict[[cc]]$format == "ISO8601") {
      data[[cc]] = readr::parse_date(data[[cc]])
    }
  }
  data
}
