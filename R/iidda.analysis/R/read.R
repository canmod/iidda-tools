#' Read IIDDA Dataset into a Dataframe
#'
#' @param dataset_id ID for a dataset in the IIDDA
#'
#' @importFrom iidda sprintf_named
#' @export
read_iidda_dataset = function(dataset_id) {
  api_template = "%{api_url}s/datasets/%{dataset_id}s?response_type=raw_csv"
  dataset_request = sprintf_named(
    api_template,
    api_url = getOption('iidda_api_base_url'),
    dataset_id = dataset_id
  )
  read.csv(dataset_request)
}
