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
    "staging_url_prefix"
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

filter_dependencies = function(tidy_dataset, tracking_table, dependencies_table) {

}

#' @param original_format should the original tracking table format be used?
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate relocate select semi_join left_join right_join
#' @importFrom tibble column_to_rownames remove_rownames
#' @export
get_tracking_metadata = function(tidy_dataset, digitization, tracking_path, original_format = TRUE) {
  current_tidy_dataset = tidy_dataset
  current_digitization = digitization

  if (!original_format) {
    paths = file.path(tracking_path, list.files(tracking_path, pattern = '.csv'))
    d = (paths
      %>% lapply(read.csv, check.names = FALSE)
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
    filtered_source_files = mapply(
      semi_join,
      d[source_files],
      filtered_dependencies
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
      Originals = rename(filtered_source_files$Scans, original = scan),
      Columns = filtered_columns
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
  metadata$Originals = split(metadata$Originals, metadata$Originals$original)

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
  for(column in rownames(column_metadata)) {
    attr(table[[column]], 'label') = column_metadata[column, "title"]
    attr(table[[column]], 'description') = column_metadata[column, "description"]
  }
  table
}


#' Make DataCite JSON Metadata
#'
#' @param metadata Output of get_tracking_metadata
#' @param file Path to metadata file
#' @importFrom jsonlite write_json
#' @export
make_data_cite_tidy_data = function(metadata, file) {

  # TODO: remove much of the hard-coding below
  # https://github.com/canmod/iidda-tools/issues/7
  data_cite = list(
    # TODO: move this identifier down to alternateIdentifiers
    # https://github.com/canmod/iidda-tools/issues/8
    identifier = list(
      identifier = metadata$TidyDataset$path_tidy_data,
      identifierType = 'iidda_product'
    ),
    creators = list(
      list(
        # TODO: iidda@mcmaster.ca should be the contact
        # bouncing now -- send a message to sys admin
        # https://github.com/canmod/iidda-tools/issues/9
        creatorName = "McMaster University Theo-Bio Lab",
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
    subjects = NULL,
    contributors = list(
      # wish there was a better type than "Other", but this contributor
      # is intended to provide the organization from whom we obtained
      # the original source documents
      contributorType = "Other",
      name = metadata$Source$organization,
      nameType = "Organizational"
    ),
    language = 'en',
    resourceType = list(
      resourceTypeGeneral = "Dataset",
      resourceType = lookup(metadata$Source$type, resource_type_dict)[[1L]]
    ),
    # TODO: move main identifier here once we get DOI's going
    # https://github.com/canmod/iidda-tools/issues/8
    alternateIdentifiers = NULL,
    relatedIdentifiers = list(
      list(
        relatedIdentifier = metadata$Digitization$path_digitized_data,
        relatedIdentifierType = "URL",
        relationType = "IsSourceOf"
      ),
      list(
        relatedIdentifier = sapply(metadata$Originals,`[[`,'path_original_data'),
        relatedIdentifierType = "URL",
        relationType = "IsSourceOf"
      )
    ),
    sizes = NULL,  # TODO: compute automatically from file.info('~/testing_csv.csv')$size,
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
    dates = list(
      date = iso_8601_dateranges(
        metadata$TidyDataset$period_start_date,
        metadata$TidyDataset$period_end_date
      ),
      dateType = "Collected"
    ),
    dateInformation = "Date ranges refer to the start and end dates of the historical period described by these data.",
    fundingReferences = NULL,
    geoLocations = list(
      geoLocationPlace = metadata$Source$location
    )
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
