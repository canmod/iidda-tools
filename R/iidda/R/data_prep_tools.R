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

  if (is.null(tidy_dir)) tidy_dir = metadata$TidyDataset$path_tidy_data
  tidy_dir = strip_blob_github(tidy_dir)
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
  make_data_cite_tidy_data(metadata, meta_file)
  return(files)
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
  tidy_data_diff = setdiff(tidy_data_cols, metadata_cols)

  if(setequal(metadata_cols, tidy_data_cols) == FALSE) stop(paste("Metadata does not contain columns", tidy_data_diff, "from tidy data", collapse = '\n'))

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
  path = strip_blob_github(metadata$Digitization$path_digitized_data)
  read_func = switch(
    tools::file_ext(path),
    xlsx = xlsx_cells,
    csv = read.csv,
    rds = readRDS,
    txt = read_delim
  )
  data = read_func(path)

  if(tools::file_ext(path) == 'xlsx'){
    (data
     %>% mutate(has_unclear_comment = grepl("unclear|uncelar", comment, ignore.case = TRUE),
         character = case_when(
           has_unclear_comment & data_type %in% c("character", "blank") ~
             ifelse(data_type == "character", sprintf("%s (unclear)", character), "(unclear)"),
                  TRUE ~ character
                ))
     %>% select(-has_unclear_comment))
  }
  data
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
#'Identifies time scales (wk, mt, qrtr, yr) and location types (province or country) within a tidy dataset.
#'
#' @param data Data frame in IIDDA tidy format to add time scale
#' and location scale information.
#' @param location_type_fixer Function that takes a data frame in IIDDA
#' tidy format and adds or fixes the `location_type` field.
#'
#' @export
identify_scales = function(data, location_type_fixer = canada_province_scale_finder) {
  (data
    |> identify_time_scales()
    |> location_type_fixer()
  )
}

identify_time_scales = function(data){
  (data
   %>% mutate(time_scale = ifelse(period_end_date == as.Date(period_start_date) + 6, "wk", "2wk"))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 14, "mo", time_scale))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 40, "qr", time_scale))
   %>% mutate(time_scale = ifelse(as.Date(period_end_date)-as.Date(period_start_date) > 100, "yr", time_scale))
  )
}
canada_province_scale_finder = function(data) {
  if ("location" %in% names(data)) {
    data$location_type = ifelse(data$location %in% c("Canada", "CANADA"), "country", "province")
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
  if (!any(focal_rows)) stop(sprintf("disease, %s%, not found", disease))
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

#' Is Leaf Disease
#'
#' Given a set of `disease`-`nesting_disease` pairs that all share the same
#' \code{\link{basal_disease}},
#'
#' @param disease Disease name vector.
#' @param nesting_disease Vector of the same length as \code{disease} giving
#' the nesting diseases of element in \code{disease}.
#'
#' @return True if disease is never a nesting disease (it is a leaf disease),
#' False if disease is a nesting disease.
#'
#' @export
is_leaf_disease = function(disease, nesting_disease) !disease %in% unique(nesting_disease)

#' Flatten Disease Hierarchy
#'
#' Take a tidy data set with a potentially complex disease hierarchy
#' and flatten this hierarchy so that, at any particular time and location,
#' all diseases in the `disease` column have the same `nesting_disease`.
#'
#' @param data A tidy data set with the following minimal set of columns:
#' `disease`, `nesting_disease`, `period_start_date`, `period_end_date`,
#' and `location` (TODO: generalized so that the last three are
#' configurable).
#' @param disease_lookup A lookup table with `disease` and `nesting_disease`
#' columns that describe a global disease hierarchy that will be applied
#' locally to flatten disease hierarchy at each point in time and space
#' in the tidy data set in the `data` argument.
#'
#' @export
flatten_disease_hierarchy = function(data, disease_lookup) {
  disease_lookup =
    (disease_lookup
     |> select(disease, nesting_disease)
     |> distinct())
  (data
    # getting basal disease for all diseases
    |> rowwise()
    |> mutate(basal_disease = basal_disease(disease, disease_lookup))
    |> ungroup()

    # keeping only leaf diseases
    |> group_by(period_start_date, period_end_date, location, basal_disease)
    |> filter(is_leaf_disease(disease, nesting_disease))
    |> ungroup()

    # if there is only the basal disease (no sub-diseases), differentiate by adding '-only'
    |> mutate(disease = ifelse(disease == basal_disease, sprintf("%s-only", disease), disease))
    |> mutate(nesting_disease = basal_disease)
    |> select(-basal_disease)
  )
}
