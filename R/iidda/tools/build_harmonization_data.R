# The `tools` directory contains CSV files that clarify how historical
# inconsistencies in naming can be harmonized. For example, NFLD and
# Newfoundland can both be represented using the `iso-3166-2` standard
# as CA-NL. The `build_harmonization_data.R` script can be used to push
# changes to these CSV files to the `data` directory of this package so
# that it can leverage the latest harmonization rules.

# read and save lookup tables -------------------------------

harm_files = list.files('tools', full.names = TRUE, pattern = '*.csv')
lookup_names = tools::file_path_sans_ext(basename(harm_files))
harmonization_lookup_tables = setNames(
  lapply(harm_files, read.csv),
  lookup_names
)
save(
  harmonization_lookup_tables,
  file = "data/harmonization_lookup_tables.rda"
)

# utilities for synonym lookups --------------------------------

lookup_to_synonym_list = function(
    lookup_table, from_col, to_col, empty_cols = character(0L)
  ) {
  lookup_table = lookup_table[!iidda::is_empty(lookup_table[[from_col]]), ]
  for (i in seq_along(empty_cols)) {
    lookup_table = lookup_table[iidda::is_empty(lookup_table[[empty_cols[i]]]), ]
  }
  unifier = lookup_table[[from_col]]
  (lookup_table
    %>% split(unifier)
    %>% lapply(getElement, to_col)
    %>% lapply(list)
    %>% lapply(setNames, to_col)
  )
}


standards = list()

# harmonize the location field --------------------------------

standards$iso_3166 = lookup_to_synonym_list(
  lookup_table = harmonization_lookup_tables$location,
  from_col = "iso_3166",
  to_col = "location",
  empty_cols = "iso_3166_2"
)
standards$iso_3166_inclusive = lookup_to_synonym_list(
  lookup_table = harmonization_lookup_tables$location,
  from_col = "iso_3166",
  to_col = "location"
)
standards$iso_3166_2 = lookup_to_synonym_list(
  lookup_table = harmonization_lookup_tables$location,
  from_col = "iso_3166_2",
  to_col = "location"
)

# harmonize the sex field --------------------------------

standards$iso_5218 = lookup_to_synonym_list(
  lookup_table = harmonization_lookup_tables$sex,
  from_col = "iso_5218",
  to_col = "sex"
)

save(
  standards,
  file = "data/standards.rda"
)
