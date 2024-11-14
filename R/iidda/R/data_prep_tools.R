## operations
##
## * get longest time scale in each group
## * get shortest time scale in each group
## * flatten disease hierarchy in each group
## * distribute counts evenly ...
## * sum counts ...
## * find nesting time-scale
##
## questions
##
## * can we put the above (or more) operations together so that
##   we remove double counting for the full archive?
## * if so, can we make an interface that clarifies for the user
##   how to make alternative choices?
##




#' Write Tidy Digitized Data and Metadata
#'
#' @param tidy_data Data frame of prepared data that
#' are ready to be packaged as an IIDDA tidy data set.
#' @param metadata Output of \code{\link{get_tracking_metadata}}.
#' @param tidy_dir If \code{NULL} taken from the \code{metadata}.
#'
#' @importFrom jsonlite write_json read_json
#' @importFrom dplyr `%>%`
#' @return file names where data were written
#' @importFrom utils write.table
#' @export
write_tidy_data = function(tidy_data, metadata, tidy_dir = NULL) {
  tidy_dataset = metadata$TidyDataset$tidy_dataset
  tidy_data = tidy_data[, rownames(metadata$Columns[[tidy_dataset]]), drop = FALSE]

  if (is.null(tidy_dir)) tidy_dir = metadata$TidyDataset$path_tidy_data
  tidy_dir = tidy_dir |> strip_blob_github() |> proj_path()
  if (nchar(tidy_dir) == 0L) stop("probably need to put a path to the tidy dataset in your metadata.")
  if (!dir.exists(tidy_dir)) dir.create(tidy_dir, recursive = TRUE)

  tidy_file = file.path(tidy_dir, tidy_dataset %.% 'csv')
  meta_file = file.path(tidy_dir, tidy_dataset %.% 'json')
  dict_file = file.path(tidy_dir, tidy_dataset %_% 'data_dictionary' %.% 'json')
  dial_file = file.path(tidy_dir, tidy_dataset %_% 'csv_dialect' %.% 'json')
  col_file = file.path(tidy_dir, tidy_dataset %_% 'columns' %.% 'json')
  filter_file = file.path(tidy_dir, tidy_dataset %_% 'filter_group_vals' %.% 'json')

  files = nlist(tidy_file, meta_file, dict_file, dial_file, col_file, filter_file)

  # global_dictionary = ('iidda_global_data_dictionary'
  #  %>% getOption
  #  %>% blob_to_raw
  #  %>% read_json
  # )
  # local_dictionary = (metadata
  #   %>% getElement('Columns')
  #   %>% getElement(metadata$TidyDataset$tidy_dataset)
  #   %>% rownames
  #   %>% or_pattern
  #   %>% get_with_key(l = global_dictionary, key = 'name')
  # )
  local_dictionary = write_local_data_dictionaries(metadata, dict_file)
  #write_json(local_dictionary, dict_file, pretty = TRUE, auto_unbox = TRUE)
  columns_file = getElement(metadata, 'ColumnSummary')
  write_json(columns_file, col_file, pretty = TRUE, auto_unbox = TRUE)

  filter_group_vals_file = getElement(metadata, 'filter_group_values')
  write_json(filter_group_vals_file, filter_file, pretty = TRUE, auto_unbox = TRUE)
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

  .trash = #('iidda_global_data_dictionary'
            #%>% getOption
            #%>% blob_to_raw
            #%>% read_json
          (dict_file
            %>% read_json
            %>% key_val('name', 'type')
            %>% get_elements(colnames(tidy_data))
            %>% unlist
            %>% lookup(col_classes_dict)
            %>% set_types(data = tidy_data)
            %>% write_data_frame(tidy_file)
            #   # CSV Dialect Translation
            #   sep = ',',              # delimiter
            #   eol = '\r\n',           # lineTerminator
            #   qmethod = 'escape',     # quoteChar="\"", doubleQuote=false
            #   na = "",                # nullSequence=""
            #   col.names = TRUE,       # header=true
            #   # skipInitialSpace=false
            #   # commentChar='#'
            #   # caseSensitiveHeader=true
            #   row.names = FALSE
            # )
  )
  metadata$size = paste(
    as.character(file.info(tidy_file)$size),
    "bytes", sep = " "
  )
  (metadata
    |> update_metadata_dates(tidy_data)
    |> update_metadata_locations(tidy_data)
    |> make_data_cite_tidy_data(meta_file)
  )
  return(files)
}

source_data_metadata = function(dataset_id, type) {
  path = sprintf("dataset-dependencies/%s/%s.d", dataset_id, dataset_id)
  pat = sprintf("^pipelines/[a-zA-Z0-9_-]+/%ss", type)
  (path
    |> readLines()
    |> grep(pattern = pat, value = TRUE)
    |> sprintf(fmt = "%s.json")
    |> json_files_to_data()
  )
}

add_source_data_id = function(data, dataset_id, type) {
  id_col = sprintf("%s_id", type)
  if (id_col %in% names(data)) return(data)
  metadata = source_data_metadata(dataset_id, type)
  if (isTRUE(nrow(metadata) > 1L)) {
    if ("period_end_date" %in% names(data)) {
      date_field = "period_end_date"
    } else if ("date" %in% names(data)) {
      date_field = "date"
    }
    start_col = grep("^(period_)?start_date$", names(metadata), value = TRUE)
    end_col = grep("^(period_)?end_date$", names(metadata), value = TRUE)
    start = metadata[[start_col]]
    end = metadata[[end_col]]
    stopifnot(identical(start, sort(start)))
    stopifnot(identical(end, sort(end)))
    stopifnot(all(start < end))
    i = findInterval(
        as.numeric(as.Date(data$period_end_date))
      , as.numeric(as.Date(c(start[1L], end)))
      , all.inside = TRUE
    )
  } else if (isTRUE(nrow(metadata) == 1L)) {
    i = 1L
  } else { # no metadata or it is malformed
    i = 1L
    metadata = list()
    metadata[[type]] = ""
  }
  if (!all(is_empty(metadata[[type]][i]))) {
    data[[id_col]] = metadata[[type]][i]
  }
  data
}

#' Add Provenance
#'
#' Add provenance information to an IIDDA dataset, by creating columns
#' containing the scan and digitization IDs associated with each record.
#'
#' @param tidy_data Data frame in IIDDA tidy form.
#' @param tidy_dataset The IIDDA identifier associated with the dataset for
#' which `tidy_data` serves as an intermediate object during its creation.
#'
#' @export
add_provenance = function(tidy_data, tidy_dataset) {
  (tidy_data
    |> add_source_data_id(tidy_dataset, "scan")
    |> add_source_data_id(tidy_dataset, "digitization")
  )
}

update_metadata_dates = function(metadata, data) {
  min_not_empty = function(x) min(x[!is_empty(x)])
  max_not_empty = function(x) max(x[!is_empty(x)])
  if ("period_start_date" %in% names(data)) {
    metadata$TidyDataset$period_start_date = min_not_empty(data$period_start_date)
  }
  if ("period_end_date" %in% names(data)) {
    metadata$TidyDataset$period_end_date = max_not_empty(data$period_end_date)
  }
  if ("date" %in% names(data)) {
    metadata$TidyDataset$period_start_date = min_not_empty(data$date)
    metadata$TidyDataset$period_end_date = max_not_empty(data$date)
  }
  return(metadata)
}

update_metadata_locations = function(metadata, data) {
  metadata$geo = unique(metadata$Source$location)
  loc = character()
  if ("iso_3166" %in% names(data)) {
    loc = c(loc, unique(data$iso_3166))
  }
  if ("iso_3166_2" %in% names(data)) {
    loc = c(loc, unique(data$iso_3166_2))
  }
  if (length(loc) == 0L) {
    if ("nesting_location" %in% names(data)) {
      loc = c(loc, unique(data$nesting_location))
    }
    if ("location" %in% names(data)) {
      loc = c(loc, unique(data$location))
    }
  }
  loc = loc[!is_empty(loc)]
  if (length(loc) != 0L) metadata$geo = unique(loc)
  return(metadata)
}

#' Read Tidy Data and Metadata files
#'
#' @param tidy_data_path path to folder containing 4 files: tidy data
#' and resulting metadata for each prep script
#' @param just_csv return only the tidy csv file or a list with the csv and
#' its metadata
#' @importFrom utils read.table
#' @export
read_tidy_data = function(tidy_data_path, just_csv = FALSE) {

  path_tidy_file = list.files(tidy_data_path, pattern = "\\.csv.*", full.names = TRUE)
  valid_metadata_types = c(
    "data_dictionary",
    "csv_dialect",
    "columns",
    "filter_group_vals"
  )

  path_meta_file = grep(
    list.files(tidy_data_path, pattern = "\\.json.*", full.names = TRUE),
    pattern = "\\_csv_dialect.json|\\_data_dictionary.json|\\_columns|\\_filter_group_vals.json",
    invert = TRUE,
    value = TRUE
  )
  path_dict_file = list.files(tidy_data_path, pattern = "\\_data_dictionary.json.*", full.names = TRUE)
  path_dial_file = list.files(tidy_data_path, pattern = "\\_csv_dialect.json.*", full.names = TRUE)
  path_col_file = list.files(tidy_data_path, pattern = "\\_columns.json.*", full.names = TRUE)
  path_grp_file = list.files(tidy_data_path, pattern = "\\_filter_group_vals.json.*", full.names = TRUE)
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

  tidy_dataset = read_data_frame(path_tidy_file, col_classes)
  #   read.table(path_tidy_file,
  #                           # CSV Dialect Translation
  #                           header = TRUE,           # header=true
  #                           sep = ',',               # delimiter
  #                           quote = "\"",            # quoteChar="\""
  #                           comment.char='#',        # commentChar='#'
  #                           na.strings = "",         # nullSequence=""
  #                           colClasses = col_classes
  # )
  if (just_csv) return(tidy_dataset)

  return(nlist(tidy_dataset, data_dictionary, csv_dialect, meta_data))
}

#' Convert all missing values to NA
#'
#' @param data data frame resulting from data prep scripts
#' @export
empty_to_na = function(data) {
  (data
   %>% replace(apply(data, 2, is_empty) == TRUE, NA)
  )
}


iso_3166_to_words = function(x) {
  lookup = c(
      CA = "Canada"
    ,`CA-AB` = "Alberta (Canada)"
    ,`CA-BC` = "British Columbia (Canada)"
    ,`CA-MB` = "Manitoba (Canada)"
    ,`CA-NB` = "New Brunswick (Canada)"
    ,`CA-NL` = "Newfoundland and Labrador (Canada)"
    ,`CA-NT` = "Northwest Territories (Canada)"
    ,`CA-NS` = "Nova Scotia (Canada)"
    ,`CA-NU` = "Nunavut (Canada)"
    ,`CA-ON` = "Ontario (Canada)"
    ,`CA-PE` = "Prince Edward Island (Canada)"
    ,`CA-QC` = "Quebec (Canada)"
    ,`CA-SK` = "Saskatchewan (Canada)"
    ,`CA-YK` = "Yukon (Canada)"
  )

  x[x %in% names(lookup)] = lookup[x]
  x
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
  if (missing(locations_iso)) {
    locations_iso = harmonization_lookup_tables$location
  }
  (tidy_data
    %>% left_join(locations_iso, by = "location")
    %>% relocate(iso_3166_2, .after = location)
    %>% relocate(iso_3166, .after = location)
  )
}

#' Iso Codes
#'
#' Superseded by \code{\link{iso_3166_codes}}.
#'
#' @inheritParams iso_3166_codes
#' @importFrom utils read.csv
#' @export
iso_codes = function(tidy_data, locations_iso = read.csv("tracking/locations_ISO.csv")) {
  warning('this function is deprecated -- please use iso_3166_codes instead')
  iso_3166_codes(tidy_data, locations_iso)
}

#' ISO-8601 Date Ranges
#'
#' Converts start and end dates
#' into ISO-8601-compliant date ranges.
#'
#' @param start_date date vector
#' @param end_date date vector
#'
#' @export
iso_8601_dateranges = function(start_date, end_date) {
  paste(iso_8601_dates(start_date), iso_8601_dates(end_date), sep = "/")
}

#' ISO-8601 Dates
#'
#' Convert date vectors into string vectors with ISO-8601
#' compliant format.
#'
#' @param dates date vector
#'
#' @importFrom lubridate day year month
#' @export
iso_8601_dates = function(dates) {
  if (any(is_empty(dates))) {
    stop("dates associated with this tidy dataset are missing from the tracking file.")
  }
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
  tidy_data_diff = setdiff(metadata_cols, tidy_data_cols)

  if(setequal(metadata_cols, tidy_data_cols) == FALSE) {
    stop(paste("\nMetadata does not contain column", tidy_data_diff, "from tidy data", collapse = ''))
  }

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

  if(identical(metadata_diff, character(0)) == FALSE) {
    stop(paste("Tidydata does not contain columns", metadata_diff, "from metadata", collapse = ' '))
  }
}

#' Creates a heatmap that shows disease coverage over the years
#'
#' Values are TRUE if that particular disease occurred at least once in a period that ended in that
#' particular year, and FALSE otherwise.
#'
#' @param table dataframe (or dataframe-like object). Tidy dataset of all compiled datasets
#' @param disease_col specifies level of disease (i.e. disease_family, disease, disease_subclass)
#' @importFrom tidyselect all_of
#' @importFrom dplyr distinct across
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_tile
#'
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

#' Test Results
#'
#' Test the results of a data prep script (not finished).
#'
#' @inheritParams save_result
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

schema_check = function(table, metadata) {
  stop('work in progress')
}

#' Read Digitized Data
#'
#' Read in digitized data to be prepared within the IIDDA project.
#'
#' @inheritParams write_tidy_data
#' @importFrom tidyxl xlsx_cells
#' @importFrom readr read_delim
#' @export
read_digitized_data = function(metadata) {
  path = strip_blob_github(metadata$Digitization$resource_path)

  ## for back-compatibility
  if (is.null(metadata$Digitization$resource_path)) {
    path = strip_blob_github(metadata$Digitization$path_digitized_data)
  }

  path_in_proj = vapply(path, proj_path, character(1L))
  if (length(path_in_proj) > 1L) stop("more than one digitization file")
  read_func = switch(
    tools::file_ext(path_in_proj),
    xlsx = xlsx_cells,
    csv = read.csv,
    rds = readRDS,
    txt = read_delim
  )
  data = read_func(path_in_proj)

  if(tools::file_ext(path_in_proj) == 'xlsx'){
    data = (data
     %>% get_unclear_comments()
    )
  }
  data
}

get_unclear_comments = function(data){
  (data
   %>% mutate(
     comment = strsplit(comment, "\r\n\r\nComment:\r\n    ") %>%
       sapply(function(x) rev(unlist(x))[1], simplify = TRUE)
   )

   %>% mutate(has_unclear_comment = grepl("unclear|uncelar", comment, ignore.case = TRUE),
              character = case_when(
                has_unclear_comment & data_type %in% c("character", "blank") ~
                  ifelse(data_type == "character", sprintf("%s (unclear)", character), "(unclear)"),
                TRUE ~ character
              ))
   %>% select(-has_unclear_comment)
  )
}

#' Collapse xlsx Value Columns
#'
#' Collapse all value columns into a single \code{\link{character}} column
#' for data frames that have one row per cell in an xlsx file.
#'
#' @param data Data frame representing an xlsx file.
#'
#' @export
collapse_xlsx_value_columns = function(data) {
  mutate(data, value = case_when(
    data_type == "error" ~ as.character(error),
    data_type == "logical" ~ as.character(logical),
    data_type == "numeric" ~ as.character(numeric),
    data_type == "date" ~ as.character(date),
    data_type == "character" ~ character,
    data_type == "blank" ~ ""
  ))
}

#' Combine Weeks
#'
#' Combine data from different Excel sheets associated with
#' specific weeks in 1956-2000 Canadian communicable disease
#' incidence data prep pipelines.
#'
#' @inheritParams write_tidy_data
#'
#' @param cleaned_sheets List of data frames -- one for each sheet
#' @param sheet_dates Data frame describing sheet dates (TODO: more info needed)
#' @importFrom dplyr bind_rows
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

#' Identify Scales
#'
#' Identifies time scales (wk, mo, qr, yr) and location types
#' (province or country) within a tidy dataset.
#'
#' @param data Data frame in IIDDA tidy format to add time scale
#' and location scale information.
#' @param location_type_fixer Function that takes a data frame in IIDDA
#' tidy format and adds or fixes the `location_type` field.
#' @param time_scale_identifier Function that takes a data frame in IIDDA
#' tidy format and adds the `time_scale` field.
#'
#' @export
identify_scales = function(data, location_type_fixer = canada_province_scale_finder, time_scale_identifier = identify_time_scales) {
  (data
    |> time_scale_identifier()
    |> location_type_fixer()
  )
}

identify_time_scales = function(data){
  (data
   %>% mutate(time_scale = ifelse(period_end_date == as.Date(period_start_date) + 6, "wk", "2wk"))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 14, "mo", time_scale))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 40, "qr", time_scale))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 260, "3qr", time_scale))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 300, "yr", time_scale))
  )
}
canada_province_scale_finder = function(data) {
  if ("location" %in% names(data)) {
    data$location_type = ifelse(trimws(data$location) %in% c("Canada", "CANADA"), "country", "province")
  }
  data
}

split_data = function(tidy_data){
  (tidy_data
   %>% mutate(period = ifelse(period_end_date == as.Date(period_start_date) +6 | period_end_date == as.Date(period_start_date) +7, "wk", "mt"))
   %>% mutate(period = ifelse(as.Date(period_end_date)-as.Date(period_start_date) >40, "quarterly", period))
   %>% mutate(period = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 100, "year", period))
   %>% mutate(is_canada = ifelse(location == "Canada" | location == "CANADA", "canada", "province"))
   %>% mutate(splitting_column = paste(period, is_canada, sep="_"))
   %>% select(-is_canada, -period)
   %>% split(.$splitting_column)
  )
}


summarise_columns = function(tidy_data, dataset_name, metadata) {
  sapply(
    names(tidy_data),
    iidda:::column_summary,
    tidy_data,
    dataset_name,
    metadata,
    simplify = FALSE
  )
}

column_summary = function(column, tidy_data, dataset_name, metadata) {
  if (!column %in% colnames(tidy_data)) {
    stop(
      "column in the tidy data does not exist in the metadata schema\n",
      "please check Schema.csv and Columns.csv."
    )
  }
  column_metadata <- metadata[["Columns"]][[dataset_name]]
  column_metadata_row <- subset(column_metadata, rownames(column_metadata) %in% column)
  if (nrow(column_metadata_row) != 1L) {
    column_metadata_row <- column_metadata[rownames(column_metadata) %in% column, , drop = FALSE]
  }
  if (!column %in% rownames(column_metadata)) stop("column ", column, " does not seem to be in the dataset metadata")
  if (column_metadata_row[["format"]] == "num_missing") {
    range <- suppressWarnings(
      list(
        range = range(as.numeric(tidy_data[[column]]), na.rm = TRUE),
        unavailable_values = unique(
          tidy_data[[column]][is.na(as.numeric(tidy_data[[column]]))]
        )
      )
    )
    if (identical(is.infinite(range[['range']]),c(TRUE, TRUE))) {
      range[['range']] = c(NA, NA)
      return(range)
    } else {
      return(range)
    }
  } else if (column_metadata_row[["type"]] == "date") {
    range(tidy_data[[column]], na.rm = TRUE)
  } else {
    ## missing values are blank strings
    tidy_data[[column]][is.na(tidy_data[[column]])] = ""
    as.list(unique(tidy_data[[column]]))
  }
}

#' Add Column Summaries
#'
#' Add lists of unique values and ranges of values to a
#' the metadata of an IIDDA data set.
#'
#' @inheritParams write_tidy_data
#' @param dataset_name Character string giving IIDDA identifier
#' of the dataset.
#'
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

#' Add Filter Group Values
#'
#' Add lists of unique sets of values for a given filter group
#'
#' @inheritParams write_tidy_data
#' @param dataset_name Character string giving IIDDA identifier
#' of the dataset.
#'
#' @export
add_filter_group_values = function(tidy_data, dataset_name, metadata) {
  col_set <- c("disease_family", "disease", "disease_subclass")
  df <- tidy_data[col_set[col_set %in% colnames(tidy_data)]] %>% unique()
  df[is.na(df)] <- ""
  metadata$filter_group_values = df
  metadata
}

#' Prepare Mortality Data from Statistics Canada
#'
#' @param data Output of \code{\link{read_digitized_data}} that has been
#' filtered to include only the cell range that contains data.
#'
#' @return Data frame complying with the IIDDA requirements for
#' tidy datasets.
#' @export
statcan_mort_prep = function(data) {
  tidy_data = (data
    %>% collapse_xlsx_value_columns()
    #%>% mutate(character = ifelse(data_type == "date", as.character(date), character))
    #%>% mutate(data_type = ifelse(data_type == "date", "character", data_type))
    %>% select(row, col, value)
    %>% behead("N", location, value)
    %>% behead("W", period, value)
    %>% behead("W", cause, value)
    %>% mutate(period = ifelse(is_empty(period), NA, period))
    %>% mutate(period = na.locf(period, na.rm = FALSE))
    %>% filter(trimws(value) != "#")
    %>% rename(deaths = value)
    %>% select(location, period, cause, deaths)
    %>% mutate(time_scale = ifelse(grepl("All weeks", period), "yr", "wk"))
    %>% mutate(period_end_date = ifelse(time_scale == "wk", period, ""))
    %>% mutate(location_type = ifelse(location == "Canada", "can", "prov"))
  )
  year_end_date = as.character(max(as.Date(filter(tidy_data, time_scale == "wk")$period_end_date)))
  year_start_date = as.character(min(as.Date(filter(tidy_data, time_scale == "wk")$period_end_date)) - days(6L))
  (tidy_data
    %>% mutate(period_end_date = ifelse(time_scale == "yr", year_end_date, period_end_date))
    %>% mutate(period_start_date = ifelse(time_scale == "yr", year_start_date, as.character(as.Date(period_end_date) - days(6L))))
    %>% select(location, period_start_date, period_end_date, cause, deaths, time_scale, location_type)
  )
}

#' Basal Disease
#'
#' @param disease_lookup Table with two columns -- disease and nesting_disease
#' @param disease Disease for which to determine basal disease
#' @param encountered_diseases Character vector of diseases already found.
#' Typically this left at the default value of an empty character vector.
#'
#' @return The root disease that input disease maps to in disease_lookup.
#'
#' @export
basal_disease = function(disease, disease_lookup, encountered_diseases = character()) {
  good_names = c("disease", "nesting_disease")
  is_bad_names = !identical(names(disease_lookup), good_names)
  if (is_bad_names) stop("disease_lookup needs to have columns disease and nesting_disease")
  focal_rows = disease_lookup$disease == disease
  if (!any(focal_rows)) stop(sprintf("disease, %s, not found", disease))
  nesting_disease = disease_lookup$nesting_disease[focal_rows]
  is_tree_missing_nodes = length(nesting_disease) == 0L
  if (is_tree_missing_nodes) stop(paste(disease, "missing tree nodes. check that it is included in 'disease' column"))
  is_duplicate_nodes = length(nesting_disease) > 1L
  if (is_duplicate_nodes) {
    disease_lookup = unique(disease_lookup)
    nesting_disease = unique(nesting_disease)
  }
  if (disease %in% encountered_diseases) stop("not hierarchical")
  is_basal = nesting_disease == ""
  if (is_basal) return(disease)
  encountered_diseases = append(encountered_diseases, disease)
  Recall(nesting_disease, disease_lookup, encountered_diseases)
}


#' Add Basal Disease
#'
#' Add column `basal_disease` to tidy dataset
#'
#' @param data A tidy data set with a `disease` column.
#' @param lookup A lookup table with `disease` and `nesting_disease`
#' columns that describe a global disease hierarchy that will be applied
#' to find the basal disease of each `disease` in data.
#'
#' @return tidy dataset with basal disease
#'
#' @export
add_basal_disease = function(data, lookup) {
  disease_lookup = (lookup
    |> select(disease, nesting_disease)
    |> distinct()
  )

  all_diseases = unique(data$disease)
  is_missing = !all_diseases %in% disease_lookup$disease
  if (any(is_missing)) {
    missing_diseases = (data
      |> select(disease, nesting_disease)
      |> filter(disease %in% all_diseases[is_missing])
      |> distinct()
    )
    disease_lookup = rbind(missing_diseases, disease_lookup)
  }

  with_basal = (data
    |> rowwise()
    |> mutate(basal_disease = basal_disease(disease, disease_lookup))
    |> ungroup()
  )

  with_basal
}


## TODO: user-facing function to flatten the disease hierarchy. should probably
## be in iidda.analysis because it will make use of the api to get a
## disease lookup table.
aggregate_disease_hierarchy = function(data, ...) {
  (data
   |> flatten_disease_hierarchy(...)
   # group by nesting_disease etc ...
  )
}


#' Non-Numeric Report
#'
#' Save the records of a dataset that contain non-numeric data within
#' a specified numeric field. This report will be saved in the
#' `supporting-output/dataset_id` directory.
#'
#' @param data Data frame.
#' @param numeric_column Name of a numeric column in \code{data}.
#' @param dataset_id ID for the dataset that \code{data} will become, likely
#' after further processing.
#'
#' @export
non_numeric_report = function(data, numeric_column, dataset_id) {
  filename = sprintf("supporting-output/%s/non-numeric-%s.csv", dataset_id, numeric_column)
  report = data[is.na(as.numeric(data[[numeric_column]])), ]
  save_report(report, filename)
  report
}

#' Empty Column Report
#'
#' Save the records of a dataset that contain empty values in
#' `columns`. This report will be saved in the
#' `supporting-output/dataset_id` directory.
#'
#' @inheritParams non_numeric_report
#' @param columns Character vector of columns giving the columns to
#' check for emptiness.
#'
#' @export
empty_column_report = function(data, columns, dataset_id) {
  filename = sprintf("supporting-output/%s/empty-%s.csv", dataset_id, paste(columns, collapse = "-"))
  i = rep(FALSE, nrow(data))
  for (column in columns) {
    i = i | is_empty(data[[column]])
  }
  report = data[i, , drop = FALSE]
  save_report(report, filename)
  report
}

save_report = function(report, filename) {
  p = proj_path(filename)
  d = dirname(p)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  write_data_frame(report, p)
}

#' Cell Block
#'
#' Create a data frame for representing a rectangular range of cells
#' in an Excel file. This is useful for adding blank cells that do not
#' get read in by `xlsx_cells`.
#'
#' @param cells_data Data read in using `xlsx_cells`, or just any data frame
#' with integer columns `row` and `col`.
#' @export
cell_block = function(cells_data) {
  nr = diff(range(cells_data$row)) + 1L
  nc = diff(range(cells_data$col)) + 1L
  matrix(NA_character_, nr, nc) |> as.data.frame()
}

#' @export
sum_timescales = function(data, filter_out_bad_time_scales = TRUE) {
  r = (data
    |> filter(iso_3166_2 != '')
    |> group_by(time_scale, year, iso_3166_2, original_dataset_id,# iidda_source_id,
                 historical_disease, historical_disease_subclass, historical_disease_family,
                digitization_id, scan_id)
    # Exclude groups where any cases_this_period is 'unclear'
    |> filter(!any(cases_this_period %in% c(
      'unclear', 'Not available', 'Not reportable',
      'missing', 'not received'
    )))
    |> summarise(
        cases_this_period = sum(as.numeric(cases_this_period))
      , n_periods = n()
    )
    |> ungroup()
  )
  if (filter_out_bad_time_scales) {
    r = (r
      |> filter(
        ((time_scale == "wk") & (n_periods >  51L)) |
        ((time_scale == "mo") & (n_periods == 12L)) |
        ((time_scale == "qr") & (n_periods ==  4L)) |
        ((time_scale == "yr") & (n_periods ==  1L))
      )
    )
  }
  select(r, -n_periods)
}

#' @export
do_time_scale_cross_check = function(sum_of_timescales) {
  grouped_data = (sum_of_timescales
    |> group_by(year, iso_3166_2, historical_disease, historical_disease_subclass,
                 historical_disease_family, original_dataset_id, #iidda_source_id,
                digitization_id, scan_id)
    |> summarise(
      distinct_values = n_distinct(cases_this_period),
      .groups = 'drop'
    )

    |> ungroup()
    |> filter(distinct_values != 1)
  )

  # join back with original data to get all rows for groups with discrepancies
  (sum_of_timescales
     |> semi_join(grouped_data, by = c(
       "year", "iso_3166_2", "historical_disease",
       "historical_disease_subclass", "historical_disease_family",
       #"iidda_source_id",
       "original_dataset_id", "digitization_id", "scan_id"
     ))
     |> pivot_wider(
       names_from = time_scale,
       values_from = cases_this_period,
       names_prefix = "cases_this_period_",
       values_fill = list(cases_this_period = NA)
     )
     |> rowwise()
     |> mutate(
       max_cases = max(c_across(starts_with("cases_this_period_")), na.rm = TRUE),
       min_cases = min(c_across(starts_with("cases_this_period_")), na.rm = TRUE),
       percent_error = ifelse(max_cases == 0, NA, abs((max_cases - min_cases) / max_cases * 100)),
       discrepancy = max_cases - min_cases
     )
     |> ungroup()
     |> select(-max_cases, -min_cases)

     |> filter(discrepancy != 0)

     # adding back 'distinct_values'
     |> left_join(grouped_data |> select(year, iso_3166_2, historical_disease, historical_disease_subclass,
                                          historical_disease_family, original_dataset_id, distinct_values,
                                          digitization_id, scan_id#, iidda_source_id
                                         ),
                   by = c("year", "iso_3166_2", "historical_disease", "historical_disease_subclass",
                          "historical_disease_family", "original_dataset_id", "digitization_id",
                          "scan_id"#, "iidda_source_id"
                          ))

     |> relocate(cases_this_period_wk, .before = cases_this_period_mo)
     |> relocate(original_dataset_id, .after = distinct_values)
     |> relocate(scan_id, .after = original_dataset_id)
     |> relocate(digitization_id, .after = scan_id)
     |> arrange(desc(percent_error))
  )
}
