# Make Mock Ops List for Developers
#
# @param tidy_source iidda data source id
# @param local_iidda_path path to an iidda repository
# @keywords internal
make_mock_api = function(tidy_source, local_iidda_path) {

  ## HACK!
  pop_sources = c(
    `pop_ca_1871-1921_10-yearly_prov_age_sex` = "cen_ca_1921",
    `pop_ca_1921-71_an_age_prov_sex` = "pop_ca_1921-71_an_age_prov_sex",
    `pop_ca_1971-2021_an_age_prov_sex` = "pop_ca_1971-2021_an_age_prov_sex"
  )

  list(
    raw_csv = function(dataset_ids) {
      if (all(dataset_ids %in% names(pop_sources))) tidy_source = pop_sources[dataset_ids]
      staged_paths = file.path(local_iidda_path
      , "derived-data"
      , tidy_source
      , dataset_ids
      , sprintf("%s.csv", dataset_ids)
      )
      (staged_paths
        |> lapply(readr::read_csv
          , col_types = readr::cols(.default = "c")
          , na = character()
        )
        |> bind_rows()
      )
    },
    lookup_tables = function(lookup_type) {
      staged_path = file.path(local_iidda_path
      , "lookup-tables"
      , sprintf("%s.csv", lookup_type)
      )
      readr::read_csv(staged_path, col_types = readr::cols(.default = "c"), na = character())
    }
  )
}
