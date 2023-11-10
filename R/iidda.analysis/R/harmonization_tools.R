#' Create empty table with column names
#'
#' Creates an empty table in a specified directory
#' using columns names from another data frame
#' @param dir_path string indicating path to directory
#' @param lookup_table data frame with column names to include in table
#' @param csv_name string indicating name of the created .csv file
#' @return empty csv file with columns from \code{lookup_table} in the directory if successfully generated
#' @importFrom iidda write_data_frame
#' @export
generate_empty_df = function(dir_path, lookup_table, csv_name){
  if(!dir.exists(dir_path)){
    stop("Path does not exist")
  } else{
    tryCatch({
      # Retrieve lookup table and get its column names (using local directory for now)
      lookup_colnames = colnames(lookup_table)

      # Generate empty data frame with column names
      empty_lookup = setNames(data.frame(matrix(ncol = length(lookup_colnames), nrow = 0)),
                              lookup_colnames)

      # Try-catch making csv file
      write_path = paste0(dir_path, "/", csv_name, ".csv")
      write_data_frame(empty_lookup, write_path)
    }, error = function(e){
      stop("Error while generating user lookup table")
    }, finally = {
      print(paste0("User lookup table generated in ", dir_path))
    })
  }
}

#' Create user-defined lookup table
#'
#' Creates an empty user-defined lookup table in a specified directory
#' @param path string indicating path to directory
#' @param lookup_table_type string indicating type of lookup table
#' @return csv file of empty lookup table with columns from \code{lookup_table_type} in the directory if successful
#' @export
generate_user_table = function(path, lookup_table_type){
  lookup_table = iidda.api::ops$lookup_tables(lookup_type = lookup_table_type)
  if(nrow(lookup_table) == 0){
    stop("Lookup table cannot be found in the API")
  } else{
    table_name = paste0(lookup_table_type, "_user")
    generate_empty_df(path, lookup_table, table_name)
  }
  # Might need another conditional statement for internal server error
}

#' Add entries to user table
#'
#' Adds entries to user-defined lookup table
#' Entries should have names or columns from the user lookup table
#' \code{standards} can be used for entries
#' @param entries dataframe or named list of entries to add
#' @param user_table_path string indicating path to user lookup table
#' @return user lookup table with added entries in the path
#' @importFrom dplyr mutate filter across bind_rows if_all
#' @importFrom tidyselect everything
#' @importFrom readr read_csv
#' @importFrom iidda write_data_frame
#' @export
add_user_entries = function(entries, user_table_path){
  entries_df = as.data.frame(entries) %>%
    mutate(across(everything(), as.character))
  user_table = read_csv(user_table_path)

  if(all(colnames(entries_df) %in% colnames(user_table))){
    updated_df = bind_rows(user_table, entries_df) %>%
      filter(!if_all(.fns = is.na))
    write_data_frame(updated_df, user_table_path)
  } else{
    print("List names or columns not found in user lookup table")
  }
}

#' Column names to join by
#'
#' Defines column names to join by for a type of lookup table
#' @param lookup_type string indicating type of lookup table (disease, location, sex, age group)
#' @return vector of column names to join by for the type of lookup table
#' @export
names_to_join_by = function(lookup_type){
  lookup_type_list = list(location = c("location", "location_type"),
                          sex = c("sex"),
                          disease = c(
                             "historical_disease_family"
                            ,"historical_disease"
                            ,"historical_disease_subclass"
                            #,"pattern_family"
                            #,"pattern"
                            #,"pattern_subclass"
                            #,"exact_family"
                            #,"exact"
                            #,"exact_subclass"
                            ,"icd_7"
                            ,"icd_7_subclass"
                            ,"icd_9"
                            ,"icd_9_subclass"
                            #,"link_family"
                            #,"link"
                            #,"link_subclass"
                            #,"notes"
                          ),
                          age_group = c("age_group", "bin_desc"))

  if (!(lookup_type %in% names(lookup_type_list))){
    stop("Lookup table type not found")
  }

  return(lookup_type_list[[lookup_type]])
}

#' Resolve left_join
#'
#' Resolves any duplicate columns that results after left_join due to shared columns between data frames.
#' Rule: Keeps old values if all newly joined values are NA. Keeps new values otherwise (even if some entries are empty)
#' @param df data frame with duplicate columns ending in \code{.x} and \code{.y}
#' @return data frame with one remaining column for duplicates
#' @importFrom dplyr mutate select rename_with across if_all cur_column
#' @importFrom stringr str_ends str_replace str_remove
#' @importFrom tidyselect ends_with
#' @export
resolve_join = function(df){
  if(any(str_ends(colnames(df), "\\.x")) & any(str_ends(colnames(df), "\\.y"))){ # left_join results in columns ending in .x and .y if shared
    df = (df
          %>% mutate(should_join = !if_all(ends_with(".y"), ~. == "")) # Check whether newly-joined entries are all NA for a row
          %>% mutate(across(ends_with(".y"),
                            ~ ifelse(should_join, ., get(str_replace(cur_column(), "\\.y", ".x"))))) # Apply rule
          %>% select(-"should_join")
          %>% select(-ends_with(".x")) # Keep only one column for duplicate columns
          %>% rename_with(~ str_remove(., "\\.y"), ends_with(".y")))
  }

  return(df)
}

#' Left join for lookup tables
#'
#' Left joins lookup table to data frame of data.
#'
#' @param raw_data Data frame of data to be harmonized.
#' @param lookup_table Data frame of lookup table.
#' @param join_by Vector of strings indicating columns to left_join by
#' (can use \code{\link{names_to_join_by}} or specify manually).
#' @param verbose Print information about the lookup.
#' @return Data frame of newly harmonized and resolved data. Note that all
#' entries in the returned data frame are strings.
#' @importFrom dplyr select left_join across
#' @importFrom tidyselect everything
#' @importFrom tidyr replace_na
#' @export
lookup_join = function(raw_data, lookup_table, join_by, verbose = FALSE){

  # Determine initially which columns to join by for left_join
  if (missing(join_by)) stop("Please specify columns to join by")

  # Check which of join_cols are in the raw data and lookup table
  cols_in_raw = join_by[join_by %in% colnames(raw_data)]
  cols_in_lookup = join_by[join_by %in% colnames(lookup_table)]

  # Find shared columns and remove non-shared columns from lookup table
  # (so that other base columns don't interfere with join)
  shared_cols = intersect(cols_in_lookup, cols_in_raw)

  if (length(shared_cols) == 0){ # return error if dataframes don't share columns of interest
    stop(
"
  Could not find shared columns of interest to join by.
  Please check the data and lookup table, and/or manually specifiy columns
  to join by through join_by argument.
"
    )
  }

  nonshared_join_cols = setdiff(cols_in_lookup, shared_cols)
  update_lookup_table = select(lookup_table, !nonshared_join_cols)

  if (verbose) {
    print("Columns from lookup table that were used: ")
    print(colnames(update_lookup_table))
  }

  update_lookup_table = update_lookup_table[!duplicated(update_lookup_table), ] # Remove any duplicate rows in lookup table after these steps

  # Check if there are any duplicate normalizations defined in lookup table after these steps
  # Specifically, get lookup table without normalizations and check for duplicates in that table
  lookup_table_keys = select(update_lookup_table, shared_cols)
  if(any(duplicated(lookup_table_keys))){
    stop("Multiple normalizations found for some entries in final lookup table.")
  }

  # left_join and resolve any duplicate columns with resolve_join
  harmonized_data = (left_join(raw_data, update_lookup_table, by = shared_cols)
                     %>% mutate(across(everything(), as.character))
                     %>% mutate(across(everything(), ~replace_na(., "")))
                     %>% resolve_join())

  return(harmonized_data)
}

rep_delimiter = function(x, base_delimiter, escape = FALSE, max_repeats = 5, ...) {
  stopifnot(length(base_delimiter) == 1L)
  regex = base_delimiter
  if (escape) regex = paste("\\", regex, sep = "")
  for(n in 1:max_repeats) {
    regex = sprintf("(%s){%s}", regex, n)
    if (!any(grepl(regex, x, ...))) {
      delimiter = sprintf(" %s ", strrep(base_delimiter, n))
      return(delimiter)
    }
  }
  stop("\n"
       , "The base delimiter ", base_delimiter
       , " requires more than ", max_repeats, " repeats\n"
       , "to avoid clashes with the data.\n"
       , "Please either choose another base delimiter\n"
       , "or increase the max_repeats argument."
  )
}

#' Create age bin descriptions
#'
#' Create age bin descriptions for joining age_group lookup table
#' @param age_df data frame of data with age_group column
#' @importFrom dplyr group_by across summarise n ungroup left_join
#' @importFrom tidyselect all_of
#' @return data frame of data with bin_desc column
#' @export
create_bin_desc <- function(age_df){
  if(!("age_group" %in% colnames)){
    stop("Age group not included in data")
  }

  id_identifiers = c("date", "location", "sex", "dataset_id")

  bin_delimiter = rep_delimiter(unique(age_df$age_group), "|", escape = TRUE)

  (age_df_ids = age_df
    %>% group_by(across(all_of(id_identifiers)))
    %>%
      summarise(
        n_bins = n(),
        bin_desc = paste0(sort(age_group), collapse = bin_delimiter)
      )
    %>% ungroup()
  )

  age_df = left_join(age_df, age_df_ids, by = id_identifiers)

  return(age_df)
}

#' Join lookup table
#'
#' Joins lookup table in API to data
#' @param raw_data data frame of table to be harmonized
#' @param lookup_type string indicating type of lookup table from API to join
#' @return data frame of harmonized data with keys from API
#' @export
join_lookup_table = function(raw_data, lookup_type){
  lookup_table = iidda.api::ops$lookup_tables(lookup_type = lookup_type)
  n_row_lookup_table = nrow(lookup_table)
  if((n_row_lookup_table == 0) | is.null(n_row_lookup_table)) {
    stop("Lookup table cannot be found in the API")
  }

  join_by = names_to_join_by(lookup_type)

  # Create bin_desc if age data
  if(lookup_type == "age_group"){
    raw_data = create_bin_desc(raw_data)
  }

  joined_table = lookup_join(raw_data, lookup_table, join_by)
  return(joined_table)
}

#' Join user-defined lookup table
#'
#' Joins user-defined lookup table to data
#' @param raw_data data frame of table to be harmonized
#' @param user_table_path string indicating path to user-defined lookup table
#' @param lookup_type string indicating type of lookup table (disease, location, sex). Used to determine columns to join by if \code{join_by} not specified
#' @param join_by vector of strings indicating columns to join by (optional if \code{lookup_type} is disease, location, or sex)
#' @return data frame of harmonized data with user-defined keys
#' @importFrom readr read_csv
#' @export
join_user_table = function(raw_data, user_table_path, lookup_type, join_by) {
  if (missing(join_by)) join_by = names_to_join_by(lookup_type)

  # Create bin_desc if age data
  if (lookup_type == "age_group") raw_data = create_bin_desc(raw_data)

  user_table = read_csv(user_table_path)
  joined_table = lookup_join(raw_data, user_table, join_by)

  return(joined_table)
}

# because there is no `is.Date()`

#' Test if x is a Date, coerce if not
#'
#' @param x vector of putative dates
#'
#' @return vector with class Date, or error
#' @export
#'
#' @examples
#' d1 <- check_date("1920-01-01")
#' d1
#' class(d1)
#' # returns an error if x can't be coerced to Date easily
#' # check_date("may 29th")

check_date <- function(x){
  if(!inherits(x, "Date")) {x <- as.Date(x)}
  return(x)
}

# Function to create the grid

#' Create a grid of dates starting at the first day in grid unit
#'
#' Wrapper of `seq.Date()` and `lubridate::floor_date`
#'
#' @inheritParams base::seq.Date
#' @inheritParams lubridate::floor_date
#' @param start_date starting date
#' @param end_date end date
#' @param lookback Logical, should the first value start before `start_date`
#'
#' @return vector of Dates at the first of each week, month, year
#'
#' @export
#'
#' @examples
#' grid_dates(start_date = "2023-04-01"
#' , end_date = "2023-05-16")
#'
#' grid_dates(start_date = "2023-04-01"
#' , end_date = "2023-05-16"
#' , lookback = FALSE)
#'
#'
#' grid_dates(start_date = "2020-04-01"
#' , end_date = "2023-05-16"
#' , by = "2 months"
#' , unit = "month")
#' grid_dates(start_date = "2020-04-01"
#' , end_date = "2023-05-16"
#' , by = "2 months")
grid_dates <- function(start_date = "1920-01-01"
                       , end_date = "2020-01-01"
                       , by = "1 week"
                       , unit = "week"
                       , lookback = TRUE
                       , week_start = 7){
  if(!grepl(unit, by)){
    message("there may be a mismatch between your grid units in `by` and `unit`")
  }
  start_date <- check_date(start_date)
  end_date <- check_date(end_date)
  dvec <- lubridate::floor_date(seq(start_date, end_date, by = by), unit = unit)
  if(!lookback){
    dvec <- dvec[dvec > start_date]

  }
  return(dvec)

}

