#' Create user lookup table
#' 
#' Creates an empty user lookup table in a specified directory
#' @param path string indicating path to directory
#' @param lookup_table dataframe of lookup table with columns to include in the user lookup table
#' @return empty lookup table with columns from \code{lookup_table} in the directory if successfully generated
#' @export
generate_user_table = function(path, lookup_table){
  if(!dir.exists(path)){
    print("Path does not exist")
  } else{
    tryCatch({
      # Retrieve lookup table and get its column names (using local directory for now)
      lookup_colnames = colnames(lookup_table)
      
      # Generate empty csv with column names
      empty_lookup = setNames(data.frame(matrix(ncol = length(lookup_colnames), nrow = 0)),
                              lookup_colnames)
      
      # Try-catch making empty data frame
      write_path = paste0(path, "/", lookup_type, "_user.csv")
      write_data_frame(empty_lookup, write_path)
    }, error = function(e){
      print("Error while generating user lookup table")
    }, finally = {
      print(paste0("User lookup table generated in ", path))
    })
  }
}

#' Add entries to user table
#' 
#' Adds entries to user-defined lookup table
#' Entries should have list names or columns in the user lookup table
#' \code{standards} can be used within entries
#' @param entries dataframe or named list of entries to add
#' @param user_table_path string indicating path to user lookup table
#' @return new user lookup table with added entries at the path
#' @importFrom dplyr mutate filter across bind_rows if_all
#' @importFrom tidyselect everything
#' @importFrom readr read_csv
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
#' @param lookup_type string indicating type of lookup table (disease, location, sex)
#' @return vector of column names to join by for the type of lookup table
#' @export
names_to_join_by = function(lookup_type){
  lookup_type_list = list(location = c("location", "location_type"),
                          sex = c("sex"),
                          disease = c("disease_family","disease","disease_subclass",
                                      "pattern_family","pattern","pattern_subclass",
                                      "exact_family","exact","exact_subclass",
                                      "icd_7","icd_7_subclass","icd_9","icd_9_subclass",
                                      "link_family","link","link_subclass","notes"))
  
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

#' Join lookup table
#' 
#' Joins lookup table to data frame of data
#' @param raw_data data frame of data to be harmonized
#' @param lookup_table data frame of lookup table
#' @param lookup_type string indicating lookup table type (disease, location, sex)
#' @param join_by vector of strings indicating columns to left_join by (uses those from names_to_join_by if empty)
#' @return data frame of newly harmonized and resolved data. Note that all entries in the returned data frame are strings
#' @importFrom dplyr select left_join across
#' @importFrom tidyselect everything
#' @importFrom tidyr replace_na
#' @export
join_lookup_table = function(raw_data, lookup_table, lookup_type, join_by = c()){
  
  # Determine initially which columns to join by for left_join
  if(length(join_by) == 0){
    join_cols = names_to_join_by(lookup_type)
    
  } else{
    join_cols = join_by # Use columns defined by user if specified
  }
  
  # Check which of join_cols are in the raw data and lookup table
  cols_in_raw = join_cols[join_cols %in% colnames(raw_data)]
  cols_in_lookup = join_cols[join_cols %in% colnames(lookup_table)]
  
  # Find shared columns and remove non-shared columns from lookup table (so that other base columns don't interfere with join)
  shared_cols = intersect(cols_in_lookup, cols_in_raw)
  
  if(length(shared_cols) == 0){ # return error if dataframes don't share columns of interest
    stop("Could not find shared columns of interest to join by. \n
         Please check the data and lookup table, and/or manually specifiy columns to join by through join_by argument.")
  }
  
  nonshared_join_cols = setdiff(cols_in_lookup, shared_cols)
  update_lookup_table = select(lookup_table, !nonshared_join_cols)
  
  print("Columns from lookup table that were used: ")
  print(colnames(update_lookup_table))
  
  update_lookup_table = update_lookup_table[!duplicated(update_lookup_table), ] # Remove any duplicate rows in lookup table after these steps
  
  # Check if there are any duplicate normalizations defined in lookup table after these steps
  # Specifically, get lookup table without normalizations and check for duplicates in that table
  lookup_table_keys = select(update_lookup_table, shared_cols)
  if(any(duplicated(lookup_table_keys))){
    stop("Multiple normalizations found for some entries in final lookup table.")
  }
  
  # left_join and resolve any duplicate columns with resolve_join
  print(update_lookup_table)
  harmonized_data = (left_join(raw_data, update_lookup_table, by = shared_cols)
                     %>% mutate(across(everything(), as.character))
                     %>% mutate(across(everything(), ~replace_na(., "")))
                     %>% resolve_join())
  
  return(harmonized_data)
}