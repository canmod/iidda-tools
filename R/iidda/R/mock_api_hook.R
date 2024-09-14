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
          #lapply(iidda.api:::parse_columns)
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
  )
}
