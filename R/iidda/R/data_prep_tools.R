#' Write Tidy Digitized Data and Metadata
#'
#' @importFrom jsonlite write_json read_json
#' @importFrom dplyr `%>%`
#' @return file names where data were written
#' @export
write_tidy_data = function(tidy_data, metadata) {
  tidy_dataset = metadata$TidyDataset$tidy_dataset

  tidy_dir = strip_blob_github(metadata$TidyDataset$path_tidy_data)
  if(!dir.exists(tidy_dir)) dir.create(tidy_dir, recursive = TRUE)

  tidy_file = file.path(tidy_dir, tidy_dataset %.% 'csv')
  meta_file = file.path(tidy_dir, tidy_dataset %.% 'json')
  dict_file = file.path(tidy_dir, tidy_dataset %_% 'data_dictionary' %.% 'json')
  dial_file = file.path(tidy_dir, tidy_dataset %_% 'csv_dialect' %.% 'json')
  col_file = file.path(tidy_dir, tidy_dataset %_% 'columns' %.% 'json')

  files = nlist(tidy_file, meta_file, dict_file, dial_file, col_file)

  make_data_cite_tidy_data(metadata, meta_file)
  global_dictionary = ('iidda_global_data_dictionary'
   %>% getOption
   %>% blob_to_raw
   %>% read_json
  )
  local_dictionary = (metadata
                      %>% getElement('Columns')
                      %>% getElement(metadata$TidyDataset$tidy_dataset)
                      %>% rownames
                      %>% or_pattern
                      %>% get_with_key(l = global_dictionary, key = 'name')
  )
  write_json(local_dictionary, dict_file, pretty = TRUE, auto_unbox = TRUE)
  columns_file = (metadata
                      %>% getElement('ColumnSummary')
  )
  write_json(columns_file, col_file, pretty = TRUE, auto_unbox = TRUE)
  .trash = list(
    dialect = list(
      csvddfVersion =  "1.2",
      delimiter = ",",
      lineTerminator = "\r\n",
      quoteChar = "\"",
      doubleQuote = "false",
      nullSequence = "",
      skipInitialSpace = "false",
      header =  'true',
      commentChar = "#",
      caseSensitiveHeader = "true"
    )
  ) %>% write_json(dial_file, pretty = TRUE, auto_unbox = TRUE)

  check_metadata_cols(tidy_data, metadata)

  .trash = ('iidda_global_data_dictionary'
            %>% getOption
            %>% blob_to_raw
            %>% read_json
            %>% key_val('name', 'type')
            %>% get_elements(colnames(tidy_data))
            %>% unlist
            %>% lookup(col_classes_dict)
            %>% set_types(data = tidy_data)
            %>% write.table(tidy_file,
                            # CSV Dialect Translation
                            sep = ',',              # delimiter
                            eol = '\r\n',           # lineTerminator
                            qmethod = 'escape',     # quoteChar="\"", doubleQuote=false
                            na = "",                # nullSequence=""
                            col.names = TRUE,       # header=true
                            # skipInitialSpace=false
                            # commentChar='#'
                            # caseSensitiveHeader=true
                            row.names = FALSE
            )
  )
  return(files)
}

#' Read Tidy Data and Metadata files
#'
#' @param tidy_data_path path to folder containing 4 files: tidy data
#' and resulting metadata for each prep script
#' @export
read_tidy_data = function(tidy_data_path) {

  path_tidy_file = list.files(tidy_data_path, pattern="\\.csv.*", full.names = TRUE)
  path_meta_file = grep(list.files(tidy_data_path, pattern ="\\.json.*", full.names = TRUE), pattern = "\\csv_dialect.json|\\_data_dictionary.json", invert = TRUE, value = TRUE)
  path_dict_file = list.files(tidy_data_path, pattern="\\_data_dictionary.json.*", full.names = TRUE)
  path_dial_file = list.files(tidy_data_path, pattern="\\csv_dialect.json.*", full.names = TRUE)

  data_dictionary = (path_dict_file
                     %>% read_json()
  )

  col_classes = (path_dict_file
                 %>% read_json()
                 %>% key_val('name', 'type')
                 %>% unlist
                 %>% lookup(col_classes_dict)
                 %>% unlist()
  )

  csv_dialect = (path_dial_file
                 %>% read_json
                 %>% unlist

  )

  meta_data = (path_meta_file
               %>% read_json
  )

  tidy_dataset = read.table(path_tidy_file,
                            # CSV Dialect Translation
                            header = TRUE,           # header=true
                            sep = ',',               # delimiter
                            quote = "\"",            # quoteChar="\""
                            comment.char='#',        # commentChar='#'
                            na.strings = "",         # nullSequence=""
                            colClasses = col_classes
  )

  return(nlist(tidy_dataset, data_dictionary, csv_dialect, meta_data))
}

#' Convert all missing values to NA
#'
#' @param tidy_data tidy data.frame resulting from data prep scripts
#' @export
empty_to_na = function(tidy_data) {
  (tidy_data
   %>% replace(apply(tidy_data, 2, is_empty) == TRUE, NA)
  )
}

#' ISO-3166 and ISO-3166-2 Codes
#'
#' Converts geographical location information, as it was described in a
#' source document, to equivalent ISO-3166 and ISO-3166-2 codes.
#'
#' @param tidy_data data frame containing a field called \code{location}
#' containing geographical location information extracted from a source
#' document
#' @param locations_iso table containing three columns: \code{location}
#' with all unique location identifiers in the \code{tidy_data},
#' \code{iso_3166} containing equivalent ISO-3166 codes (if applicable), and
#' \code{iso_3166_2} containing equivalent ISO-3166-2 codes (if applicable)
#' @export
iso_3166_codes = function(tidy_data, locations_iso) {
  (tidy_data
    %>% left_join(locations_iso, by = "location")
    %>% relocate(iso_3166_2, .after = location)
    %>% relocate(iso_3166, .after = location)
  )
}

#' @export
iso_codes = function(tidy_data, locations_iso = read.csv("tracking/locations_ISO.csv")) {
  warning('this function is deprecated -- please use iso_3166_codes instead')
  iso_3166_codes(tidy_data, locations_iso)
}

#' @export
iso_8601_dateranges = function(start_date, end_date) {
  paste(iso_8601_dates(start_date), iso_8601_dates(end_date), sep = "/")
}

#' @importFrom lubridate day year month
#' @export
iso_8601_dates = function(dates) {
  paste(
    sprintf('%04d', year(dates)),
    sprintf('%02d', month(dates)),
    sprintf('%02d', day(dates)),
    sep = '-'
  )
  #paste(year(dates), month(dates), day(dates), sep = "-")
}

#' Error if columns in the tidy data are not in metadata Schema
#' and if all values in a column are NA
#'
#' @param tidy_data data.frame resulting from data prep scripts
#' @param metadata Nested named list describing metadata for the tidy data
#' @export
check_metadata_cols = function(tidy_data, metadata) {
  metadata_cols = (metadata
                   %>% getElement('Columns')
                   %>% getElement(metadata$TidyDataset$tidy_dataset)
                   %>% rownames)
  tidy_data_cols = colnames(tidy_data)
  tidy_data_diff = setdiff(tidy_data_cols, metadata_cols)

  if(setequal(metadata_cols, tidy_data_cols) == FALSE) stop(paste("Metadata does not contain columns", tidy_data_diff, "from tidy data", collapse = ' '))

  if(any(colSums(!is.na(tidy_data)) == 0)) stop(paste(names(tidy_data)[sapply(tidy_data, function(x) sum(is.na(x)) == length(x))], "is missing all values", collapse = ' '))
}

#' Error if columns in the metadata Schema are not in tidy data
#'
#' @param table dataframe (or dataframe-like object)
#' @param column_metadata dataframe with rownames equal to the columns
#' in \code{table}, and \code{Title} and \code{Description} columns
#' giving the title and description of each column in \code{table}
#' @export
check_tidy_data_cols = function(table, column_metadata) {
  metadata_cols = rownames(column_metadata)
  tidy_data_cols = colnames(table)
  metadata_diff = setdiff(metadata_cols, tidy_data_cols)

  if(identical(metadata_diff, character(0)) == FALSE) stop(paste("Tidydata does not contain columns", metadata_diff, "from metadata", collapse = ' '))
}

#' Creates a heatmap that shows disease coverage over the years
#'
#' Values are TRUE if that particular disease occurred at least once in a period that ended in that
#' particular year, and FALSE otherwise.
#'
#' @param table dataframe (or dataframe-like object). Tidy dataset of all compiled datasets
#' @param disease_col specifies level of disease (i.e. disease_family, disease, disease_subclass)
#' @export
disease_coverage_heatmap = function(table, disease_col = "disease") {
  (table
   %>% mutate(year = year(period_end_date))
   %>% select(all_of(disease_col), year)
   %>% rename(disease = disease_col)
   %>% distinct()
   %>% mutate(val=1)
   %>% pivot_wider(names_from = disease, values_from = val)
   %>% mutate(across(!year, .fns = is.na))
   %>% mutate(across(!year, .fns = `!`))
   %>% pivot_longer(!year)
   %>% rename(disease = name)
   %>% rename(data_present = value)
   %>% mutate(year = as.integer(year))
   %>% ggplot(aes(year, disease)) +
     geom_tile(aes(fill = data_present))
  )
}

#' Save Results of a Data Prep Script
#'
#' Save the resulting objects of a data prep script into an R data file.
#' The names of the resulting objects are given by the names of the
#' result list.
#'
#' @param result Named list of data resulting from data prep scripts
#' @param metadata Nested named list describing metadata for the result.
#' It must have a \code{$Product[["Path to tidy data"]]} component, which is
#' a GitHub URL describing the ultimate location of the R data file.
#' The GitHub component of the URL will be removed to produce
#' a path that will correspond to the location within a cloned git
#' repository -- note that the path is relative to the top-level of
#' the cloned repository.
#'
#' @export
save_result = function(result, metadata) {
  output_file = strip_blob_github(metadata$Product$`Path to tidy data`)
  save(list = names(result), file = output_file, envir = list2env(result))
}

#' @export
test_result = function(result) {
  md_nms = grep('_metadata$', names(result), value = TRUE)
  stopifnot(length(md_nms) == 1L)
  metadata = result[[md_nms]]
  table_nms = grep('_metadata$', names(result), value = TRUE, invert = TRUE)
  stopifnot(length(table_nms) > 0L)
  stopifnot(is.recursive(metadata))
  stopifnot(is.character(metadata$Product$`Path to tidy data`))
  output_file = strip_blob_github(metadata$Product$`Path to tidy data`)
  e = new.env()
  load(output_file, envir = e)
  previous_result = as.list(e)
  if(length(previous_result) != length(result)) {
    stop('number of resulting objects has changed')
  }

  if(!isTRUE(all.equal(result[md_nms], previous_result[md_nms]))) {
    stop('metadata have changed in some way')
  }
  result = result[table_nms]
  previous_result = previous_result[table_nms]
  mapply(compare_columns, result, previous_result)
}

#' @export
schema_check = function(table, metadata) {
  stop('work in progress')
}

#' @export
read_digitized_data = function (metadata) {
  path = strip_blob_github(metadata$Digitization$path_digitized_data)
  read_func = switch(
    tools::file_ext(path),
    xlsx = xlsx_cells,
    csv = read.csv
  )
  read_func(path)
}

#' @export
combine_weeks = function(cleaned_sheets, sheet_dates, metadata) {
  (cleaned_sheets
   %>% bind_rows(.id = "sheet")
   %>% left_join(sheet_dates, by ="sheet")
   %>% select(-sheet)
   %>% relocate(period_end_date, .after = location)
   %>% relocate(period_start_date, .after = location)
   %>% as.data.frame
   %>% add_metadata(metadata$Tables, metadata$Columns[[product]], product)
  )
}

#' Split Tidy Data
#'
#' Creates 6 tidy data sets with no duplicate data, broken down by period
#' (wk, mt, quarter, year) and province/Canada.
#' @export
split_tidy_data = function(tidy_data){
  (tidy_data
    %>% mutate(period = ifelse(period_end_date == period_start_date +6 | period_end_date == period_start_date +7, "wk", "mt"))
    %>% mutate(period = ifelse(period_start_end_date-period_start_date>40, "quarter", period))
    %>% mutate(period = ifelse(period_end_date-period_start_date > 100, "year", period))
    %>% mutate(is_canada = ifelse(location == "Canada" | location == "CANADA", "canada", "province"))
    %>% mutate(splitting_column = paste(period, is_canada, sep="_"))
    %>% select(-is_canada, -period)
  )
}

#' @export
column_summary = function(column, tidy_data, dataset_name, metadata) {
  column_metadata <- metadata[["Columns"]][[dataset_name]]
  column_metadata_row <- subset(column_metadata, rownames(column_metadata) %in% column)
  if (column_metadata_row[["format"]] == "num_missing") {
    list(range = range(as.numeric(tidy_data[[column]]), na.rm=TRUE), unavailable_values = unique(tidy_data[[column]][is.na(as.numeric(tidy_data[[column]]))]))
  } else if (column_metadata_row[["type"]] == "date") {
    range(tidy_data[[column]], na.rm=TRUE)
  } else {
    as.list(unique(tidy_data[[column]][!is.na(tidy_data[[column]])]))
  }
}

#' @export
add_column_summaries = function(tidy_data, dataset_name, metadata) {
  metadata$ColumnSummary = sapply(
    names(tidy_data),
    column_summary,
    tidy_data,
    dataset_name,
    metadata,
    simplify = FALSE
  )
  metadata
}
