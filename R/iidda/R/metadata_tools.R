#' @export
read_tracking_tables = function(path) {
  paths = file.path(path, list.files(path, pattern = '.csv'))
  (paths
    %>% lapply(read.csv, check.names = FALSE)
    %>% setNames(tools::file_path_sans_ext(basename(paths)))
    %>% lapply(drop_empty_cols)
    %>% lapply(drop_empty_rows)
  )
}

#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate relocate select semi_join left_join right_join
#' @importFrom tibble column_to_rownames remove_rownames
#' @export
get_tracking_metadata = function(product, tracking_path) {
  current_product = product
  d = read_tracking_tables(tracking_path)

  metadata = list(
    Product = (d$Transformations
               %>% filter(product == current_product)
    ),
    Source = (d$Originals
              %>% filter(product == current_product)
              %>% semi_join(x = d$Sources, by = "source")
    ),
    Originals = (d$Originals
                 %>% filter(product == current_product )
                 %>% mutate(original = basename(path_original_data))
                 %>% relocate(original, .before = source)
    ),
    Tables = (d$Tables
              %>% filter(product == current_product)
              %>% remove_rownames
              %>% column_to_rownames("table")
    ),
    Columns = (d$Tables
               %>% filter(product == current_product)
               %>% select(table)
               %>% right_join(d$Schema, by = "table")
               %>% left_join(d$Columns, by = "column")
    )
  )
  metadata$Columns = (metadata$Columns
                      %>% split(metadata$Columns$table)
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
make_data_cite = function(metadata, file) {

  # TODO: remove much of the hard-coding below
  data_cite = list(
    # TODO: move this identifier down to alternateIdentifiers
    identifier = list(
      identifier = metadata$Product$path_tidy_data,
      identifierType = 'iidda_product'
    ),
    creators = list(
      list(
        # TODO: iidda@mcmaster.ca should be the contact
        # bouncing now -- send a message to sys admin
        creatorName = "McMaster University Theo-Bio Lab",
        nameType = "Organizational"
      )
    ),
    titles = list(
      list(
        title = metadata$Tables$title,
        lang = "en"
      )
    ),
    publisher = metadata$Table$publisher,
    publicationYear = metadata$Tables$publicationYear,
    subjects = NULL,
    contributors = list(
      # I wish there was a better type than "Other", but this contributor
      # is intended to provide the organization from whom we obtained
      # the original source documents
      contributorType = "Other",
      name = metadata$Source$organization,
      nameType = "Organizational"
    ),
    language = 'en',
    resourceType = list(
      resourceTypeGeneral = "Dataset",
      resourceType = "Communicable Disease Incidence"
    ),
    alternateIdentifiers = NULL,
    relatedIdentifiers = NULL,
    sizes = NULL,  # TODO: compute automatically from file.info('~/testing_csv.csv')$size,
    formats = list("csv"),
    version = metadata$Tables$current_version,
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
        description = metadata$Tables$description
      ),
      list(
        descriptionType = "Methods",
        lang = "en",
        description = "This data set is a part of a systematic effort to make Canada's historical record of infectious diseases publicly and conveniently available. We are systematically contacting data stewards across Canada to access the disparate source documents that contain Canada's historical record of infectious diseases. We are making scans of these documents conveniently available for all. We are manually entering the information provided by these source documents into Excel spreadsheets, which we are making publicly available. The layout of these spreadsheets are identical to the originals, making it as easy as possible to compare the reproductions with the sources. We are producing reproducible automated processes for converting the digitized spreadsheets into tidy data structures. These tidy data structures contain all of the information in the original source documents, but are more convenient for analysis and discovery."
      )
    ),
    fundingReferences = NULL,
    geoLocations = list(
      geoLocationPlace = metadata$Source$location
    )
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}
