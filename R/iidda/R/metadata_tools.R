#' @export
read_tracking_tables = function(path) {
  valid_colnames = c("access_script", "disease_family", "disease", "disease_subclass", "icd_7", "icd_9",
    "icd_7_subclass", "icd_9_subclass", "disease_level", "column",
    "title", "type", "format", "description", "pattern", "organization",
    "name", "email", "Notes", "data_enterer", "digitized", "path_digitized_data",
    "path_original_data", "digitization", "period_start_date", "period_end_date",
    "location", "iso_3166", "iso_3166_2", "url", "province_territory",
    "notes", "source", "years", "start_date", "end_date", "frequency",
    "breakdown", "urls", "date_of_url_access", "tidy_dataset", "path_prep_script",
    "path_tidy_data", "publisher", "publicationYear", "current_version",
    "staging_url_prefix", "scan", "access_script", "prep_script"
  )
  paths = file.path(path, list.files(path, pattern = '.csv'))
  (paths
    %>% lapply(read.csv, check.names = FALSE)
    %>% setNames(tools::file_path_sans_ext(basename(paths)))
    %>% lapply(function(x) select(x, any_of(valid_colnames)))
    #%>% lapply(drop_empty_cols)
    %>% lapply(drop_empty_rows)
  )
}

#' Read Tracking Metadata
#'
#' Read in CSV files that contain the single-source-of-truth for metadata
#' to be used in a data prep script.
#'
#' This function currently assumes that a single tidy dataset is being
#' produced from a single digitized file.
#'
#' @param tidy_dataset key to the tidy dataset being produced by the script
#' @param digitization key to the digitization being used by the script
#' @param original_format should the original tracking table format be used?
#' @param for_lbom are these data being read for the LBoM repo?
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate relocate select semi_join left_join right_join rename
#' @importFrom tibble column_to_rownames remove_rownames
#' @export
get_tracking_metadata = function(tidy_dataset, digitization, tracking_path, original_format = TRUE, for_lbom = FALSE) {
  current_tidy_dataset = tidy_dataset
  current_digitization = digitization

  if (!original_format) {
    paths = file.path(tracking_path, list.files(tracking_path, pattern = '.csv'))
    d = (paths
      %>% lapply(read.csv, check.names = FALSE, colClasses = "character")
      %>% setNames(tools::file_path_sans_ext(basename(paths)))
      #%>% lapply(function(x) select(x, any_of(valid_colnames)))
      #%>% lapply(drop_empty_cols)
      %>% lapply(drop_empty_rows)
    )
    lookup_tidy_dataset = data.frame(tidy_dataset = current_tidy_dataset)
    lookup_digitization = data.frame(digitization = current_digitization)
    dependency_re = "^([A-Z]{1}[a-z]*)Dependencies$"
    dep_files = grep(dependency_re, names(d), value = TRUE)
    dep_id = sub(dependency_re, "\\1", dep_files)
    source_file_re = sprintf("^%s.*(?<!Dependencies)$", dep_id)
    source_files = vapply(
      source_file_re,
      grep,
      character(1L),
      names(d),
      value = TRUE, perl = TRUE, USE.NAMES = FALSE
    )

    filtered_tidy_datasets = semi_join(
      d$TidyDatasets,
      lookup_tidy_dataset,
      by = "tidy_dataset"
    )
    filtered_dependencies = mapply(
      semi_join,
      d[dep_files],
      MoreArgs = list(
        y = lookup_tidy_dataset,
        by = "tidy_dataset"
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = TRUE
    )
    dependency_keys = (filtered_dependencies
      %>% lapply(names)
      %>% lapply(getElement, 1L) ## require key to be in first column!!
    )
    # this mapply filter needs to be explicit about the 'by' argument
    filtered_source_files = mapply(
      semi_join,
      d[source_files],
      filtered_dependencies,
      dependency_keys  # by keys
    )
    filtered_digitizations = semi_join(
      d$Digitizations,
      lookup_digitization,
      "digitization"
    )
    filtered_source_info = semi_join(
      d$Sources,
      filtered_digitizations,
      "source"
    )
    filtered_columns = (d$Schema
       %>% filter(tidy_dataset == current_tidy_dataset)
       %>% left_join(d$Columns, by = "column")
    )

    metadata = list(
      TidyDataset = filtered_tidy_datasets,
      Digitization = filtered_source_files$Digitizations,
      Source = filtered_source_info,
      Originals = rename(filtered_source_files$Scans, original = scan),  ## IsOriginalOf
      Columns = filtered_columns,
      PrepScript = filtered_source_files$PrepScripts,
      AccessScripts = filtered_source_files$AccessScripts
    )

  } else {

    d = read_tracking_tables(tracking_path)

    metadata = list(
      TidyDataset = (d$TidyDatasets
                     %>% filter(tidy_dataset == current_tidy_dataset)
      ),
      Digitization = (d$Digitizations
                      %>% filter(digitization == current_digitization)
      ),
      Source = (d$Originals
                %>% filter(digitization == current_digitization)
                %>% semi_join(x = d$Sources, by = "source")
      ),
      Originals = (d$Originals
                   %>% filter(digitization == current_digitization)
                   %>% mutate(original = basename(path_original_data))
                   %>% relocate(original, .before = source)
      ),
      # deleted reference to tables.csv, move info from tables.csv to tidydataset.csv
      Columns = (d$Schema
                 %>% filter(tidy_dataset == current_tidy_dataset)
                 %>% left_join(d$Columns, by = "column")
      )
    )
  }

  metadata$Columns = (metadata$Columns
    %>% split(metadata$Columns$tidy_dataset)
    %>% lapply(remove_rownames)
    %>% lapply(column_to_rownames, var = "column")
  )

  if (nrow(metadata$Originals) > 0L) {
    metadata$Originals = split(metadata$Originals, metadata$Originals$original)

    ## are characteristics used anywhere? these are just causing problems, no?
    metadata$Characteristics = (metadata$Originals
      %>% bind_rows
      %>% summarise(
        type = summarise_strings(type),
        disease = summarise_strings(disease),
        location = summarise_strings(type),
        years = summarise_integers(years),
        dates = summarise_dates(start_date, end_date),
        frequency = summarise_strings(frequency),
        breakdown = summarise_strings(breakdown)
      )
      %>% as.list
    )
  } else {
    metadata$Originals = list(metadata$Originals)
  }
  if (for_lbom) {
    abs_path = metadata$Digitization$path_digitized_data
    if (!is.null(abs_path)) {
      if (length(abs_path) == 1L & is.character(abs_path)) {
        metadata$lbom_info$relative_path = file.path(".", strip_blob_github(abs_path))
      }
    }
    metadata$lbom_info$data_category = switch(
      metadata$TidyDataset$type,
      Mortality = "mortality",
      ACM = "all-cause-mortality",
      Births = "births",
      Plague = "plague",
      Population = "population"
    )
  }
  metadata
}

#' @export
tracking_tables_with_column = function(metadata, col_nm) {
  (metadata
   %>% lapply(names)
   %>% lapply(`==`, col_nm)
   %>% lapply(any)
   %>% unlist
   %>% which
  )
}

#' @export
tracking_table_keys = list(
  organization = list(
    primary = c('Organizations'),
    foreign = c('Contacts', 'Sources')
  ),
  source = list(
    primary = c('Sources'),
    foreign = c('Originals')
  ),
  digitization = list(
    primary = c('Digitizations'),
    foreign = c('Originals', 'TidyDatasets')
  ),
  tidy_dataset = list(
    primary = c('TidyDatasets'),
    foreign = c('Schema')
  ),
  column = list(
    primary = c('Columns'),
    foreign = c('Schema')
  )
)

#' @export
melt_tracking_table_keys = function(keys) {
  (keys
   %>% lapply(as.data.frame)
   %>% bind_rows(.id = 'key')
  )
}

#' Check Tracking Table Consistency
#'
#' @param path path to tracking tables
check_tracking_tables = function(path) {
  d = read_tracking_tables(path)

}


#' @importFrom tibble rownames_to_column
#' @importFrom jsonlite write_json
#' @export
write_local_data_dictionaries = function(metadata, path) {
  dictionary_data_frames = (metadata$Columns
   %>% lapply(rownames_to_column, var = "name")
   %>% lapply(select, -tidy_dataset)
   # %>% lapply(toJSON, pretty = TRUE)
   # %>% setNames(names(metadata$Columns))
  )
  if (length(dictionary_data_frames) != 1L) {
    stop("cannot create data dictionary from more than one Columns data frame")
  }
  write_json(dictionary_data_frames[[1]], path, pretty = TRUE, auto_unbox = TRUE)
}


#' Add Metadata
#'
#' Add title and description metadata to a table and its columns.
#'
#' @param table dataframe (or dataframe-like object)
#' @param table_metadata named list (or list-like object) such that
#' \code{table_metadata$Title} and \code{table_metadata$Description}
#' are strings containing the title and description of the table
#' @param column_metadata dataframe with rownames equal to the columns
#' in \code{table}, and \code{Title} and \code{Description} columns
#' giving the title and description of each column in \code{table}
#' @return version of \code{table} with added metadata \code{attributes}
#' @export
add_metadata = function(table, table_metadata, column_metadata, product) {
  table = as.data.frame(table)
  table_metadata = as.list(table_metadata)
  check_tidy_data_cols(table, column_metadata)
  attr(table, 'title') = table_metadata$Title
  attr(table, 'description') = table_metadata$Description
  for (column in rownames(column_metadata)) {
    attr(table[[column]], 'label') = column_metadata[column, "title"]
    attr(table[[column]], 'description') = column_metadata[column, "description"]
  }
  table
}

make_related_identifier = function(
    metadata,
    relation_type = c("IsCompiledBy", "IsDerivedFrom", "References")
  ) {

  get_original_data_paths = function(metadata) {
    path_vec = unlist_char_list(
      lapply(metadata$Originals, `[[`, "path_original_data")
    )
    path_vec[!is_empty(path_vec)]
  }

  # From Table 9 in https://schema.datacite.org/meta/kernel-4.3/doc/DataCite-MetadataKernel_v4.3.pdf
  # IsCompiledBy -- for prep scripts and access scripts -- Scripts
  # IsDerivedFrom -- for digitizations -- Digitizations
  # References -- for scans -- Originals
  relation_type = match.arg(relation_type)
  path_vec = switch(relation_type
    , IsCompiledBy = c(
      metadata$PrepScript$path_prep_script,
      metadata$AccessScripts$path_access_script
    )
    , IsDerivedFrom = metadata$Digitization$path_digitized_data
    , References = get_original_data_paths(metadata)
  )
  if (length(path_vec) == 0L) return(list())
  listify_relation = function(x) list(
    relatedIdentifier = x,
    relatedIdentifierType = "URL",
    relationType = relation_type
  )
  lapply(unname(path_vec), listify_relation)
}



#' Make DataCite JSON Metadata
#'
#' @param metadata Output of get_tracking_metadata
#' @param file Path to metadata file
#' @importFrom jsonlite write_json
#' @export
make_data_cite_tidy_data = function(metadata, file) {
  # schema:
  # https://github.com/datacite/schema/blob/master/source/json/kernel-4.3/datacite_4.3_schema.json

  # From Table 9 in https://schema.datacite.org/meta/kernel-4.3/doc/DataCite-MetadataKernel_v4.3.pdf
  # IsCompiledBy -- for prep scripts and access scripts
  # IsDerivedFrom -- for digitizations
  # References -- for scans

  # TODO: remove much of the hard-coding below
  # https://github.com/canmod/iidda-tools/issues/7
  data_cite = list(
    # TODO: move this identifier down to alternateIdentifiers
    # https://github.com/canmod/iidda-tools/issues/8
    identifiers = list(list(
      identifier = metadata$TidyDataset$path_tidy_data,
      identifierType = 'iidda-id'
    )),
    creators = list(
      list(
        # TODO: iidda@mcmaster.ca should be the contact
        # bouncing now -- send a message to sys admin
        # https://github.com/canmod/iidda-tools/issues/9
        name = "McMaster University Theo-Bio Lab",
        nameType = "Organizational"
      )
    ),
    titles = list(
      list(
        title = metadata$TidyDataset$title,
        lang = "en"
      )
    ),
    publisher = metadata$TidyDataset$publisher,
    publicationYear = metadata$TidyDataset$publicationYear,
    subjects = list(),
    contributors = list(list(
      # wish there was a better type than "Other", but this contributor
      # is intended to provide the organization from whom we obtained
      # the original source documents
      contributorType = "Other",
      name = metadata$Source$organization,
      nameType = "Organizational"
    )),
    language = 'en',
    types = list(
      resourceTypeGeneral = "Dataset",
      resourceType = iidda.list::lookup(metadata$TidyDataset$type, resource_type_dict)[[1L]]
    ),
    # TODO: move main identifier here once we get DOI's going
    # https://github.com/canmod/iidda-tools/issues/8
    alternateIdentifiers = list(),
    relatedIdentifiers = c(
      list(),
      make_related_identifier(metadata, "IsCompiledBy"),
      make_related_identifier(metadata, "IsDerivedFrom"),
      make_related_identifier(metadata, "References")
    ),
    sizes = list(),  # TODO: compute automatically from file.info('~/testing_csv.csv')$size,
    formats = list("csv"),
    version = metadata$TidyDataset$current_version,
    rightsList = list(
      list(
        rights = "CC0 1.0 Universal",
        rightsURI = "http://creativecommons.org/publicdomain/zero/1.0/",
        lang = "en-us"
      )
    ),
    descriptions = list(
      list(
        descriptionType = "Abstract",
        lang = "en",
        description = metadata$TidyDataset$description
      ),
      list(
        descriptionType = "Methods",
        lang = "en",
        description = "This data set is a part of a systematic effort to make Canada's historical record of infectious diseases publicly and conveniently available. We are systematically contacting data stewards across Canada to access the disparate source documents that contain Canada's historical record of infectious diseases. We are making scans of these documents conveniently available for all. We are manually entering the information provided by these source documents into Excel spreadsheets, which we are making publicly available. The layout of these spreadsheets are identical to the originals, making it as easy as possible to compare the reproductions with the sources. We are producing reproducible automated processes for converting the digitized spreadsheets into tidy data structures. These tidy data structures contain all of the information in the original source documents, but are more convenient for analysis and discovery."
      )
    ),
    dates = list(list(
      date = iso_8601_dateranges(
        metadata$TidyDataset$period_start_date,
        metadata$TidyDataset$period_end_date
      ),
      dateType = "Collected",
      dateInformation = "Date ranges refer to the start and end dates of the historical period described by these data."
    )),
    fundingReferences = list(),
    geoLocations = list(list(
      geoLocationPlace = metadata$Source$location
    )),
    schemaVersion = "http://datacite.org/schema/kernel-4"
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}

#' @inheritParams make_data_cite_tidy_data
#' @export
make_data_cite_digitization = function(metadata, file) {
  data_cite = list(
    identifier = list()
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}

#' @inheritParams make_data_cite_tidy_data
#' @export
make_data_cite_scans = function(metadata, file) {
  data_cite = list(
    identifier = list()
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}

#' @param tracking_list output of \code{read_tracking_tables}
#' @export
get_canmod_digitization_metadata = function(tracking_list) {
  d = tracking_list
  (d$Sources
    %>% filter(type == "CDI", breakdown == "province",
               grepl('(weekly|4-weekly|quarterly|monthly)', frequency))
    %>% left_join(d$Originals, by = "source", suffix = c("_source", "_orig"))
    %>% left_join(d$Digitizations, by = "digitization", suffix = c("", "_digit"))
    %>% left_join(d$TidyDatasets, by = "digitization", suffix = c("", "_tidy"))
    %>% filter(!is_empty(path_tidy_data))
  )
}

#' IIDDA Data Dictionary
#'
#' Get the global data dictionary for IIDDA
#'
#' This function requires an internet connection.
#'
#' @importFrom jsonlite read_json fromJSON toJSON
#' @export
iidda_data_dictionary = function() {
  # data dictionary location
  global_data_dictionary_url = file.path(
    "https://raw.githubusercontent.com", # api
    "canmod", # github user/org
    "iidda", # github repo
    "main", # github branch
    "global-metadata", # folder
    "data-dictionary.json" # file
  )

  (global_data_dictionary_url
    %>% read_json
    %>% toJSON
    %>% fromJSON
  )
  # try(
  #   read_json(global_data_dictionary_url),
  #   silent = TRUE
  # )

  # (ops$metadata(response_type = 'data_dictionary')
  #  %>% lapply(toJSON)
  #  %>% lapply(fromJSON)
  #  %>% rbind_pages
  #  %>% unique
  #  %>% remove_rownames
  # )
  # api_template = "%{api_url}s/datasets/%{dataset_id}s?response_type=raw_csv"

}
