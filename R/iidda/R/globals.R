utils::globalVariables(
  c(
    ## stuff used in 1956-2000 prep scripts
    "sheet", "product",

    ## iidda data dictionary column names
    "access_script", "disease_family", "disease", "disease_subclass", "icd_7", "icd_9",
    "icd_7_subclass", "icd_9_subclass", "disease_level", "column",
    "title", "type", "format", "description", "pattern", "organization",
    "name", "email", "Notes", "data_enterer", "digitized", "path_digitized_data",
    "path_original_data", "digitization", "period_start_date", "period_end_date",
    "location", "iso_3166", "iso_3166_2", "url", "province_territory",
    "notes", "source", "years", "start_date", "end_date", "frequency",
    "breakdown", "urls", "date_of_url_access", "tidy_dataset", "path_prep_script",
    "path_tidy_data", "publisher", "publicationYear", "current_version",
    "staging_url_prefix", "scan", "access_script", "prep_script",

    ## intermediate column names in time-scale helpers
    "frequency",

    ## potentially out-of-date stuff
    "type", "breakdown",  ## get_canmod_digitization_metadata should be superseded by iidda.api

    ## stuff i don't understand
    "setNames", ## something this standard should always be visible, no?

    ## stuff that might be 'wrong'
    "val", "data_present", ## code smell in disease_coverage_heatmap
    "name", "value", ## pivot_longer refactor would fix this
    "str", "x1", "y1", "xy", "xy1", "xy2", "meta", "angle" ## code smell in edge_detect_school_term_grid
  )
)
