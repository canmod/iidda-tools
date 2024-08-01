
#' Normalize Location
#'
#' Set geographic order of provinces and territories and remove country-level
#' data.
#'
#' @param data Tidy dataset with an iso_3166_2 column.
#' 
#' @return Tidy dataset without country-level data and with provinces and 
#' territories geographically ordered.
#'
#' @export
normalize_location = function(data){
  geog_order = c(
    ## east
    "CA-NL"
    , "CA-PE"
    , "CA-NS"
    , "CA-NB"
    ## central
    , "CA-QC"
    , "CA-ON"
    ## west
    , "CA-MB"
    , "CA-SK"
    , "CA-AB"
    , "CA-BC"
    ## north
    , "CA-YT"
    , "CA-NT"
    , "CA-NU"
  ) |> sub(pattern = "CA-", replacement = "") |> rev()
  
  (data
  |> mutate(iso_3166_2 = factor(sub("CA-", "", iso_3166_2), geog_order))
  |> filter(!is.na(iso_3166_2))
  )
}


#' Normalize Population
#'
#' 
#' 
#' @param data Tidy dataset with columns period_start_date, period_end_date, 
#' iso_3166_2
#' @param harmonized_population Harmonized population data from API
#'  (load using api_hook$raw_csv(dataset_ids = "pop_ca_1871-2021_harmonized"))
#'  or iidda-staging
#'  (load from 
#'  iidda-staging/derived_data/pop_ca_1871-2021_harmonized/pop_ca_1871-2021_harmonized/pop_ca_1871-2021_harmonized.csv)
#' 
#' @return Tidy dataset joined with harmonized population.
#'
#' @importFrom tidyr starts_with
#' @importFrom readr parse_date
#' @export
normalize_population = function(data, harmonized_population){
  (data
   |> mutate(across(tidyr::starts_with("period_"), 
                    ~ if (!inherits(., "Date")) readr::parse_date(.) else .))
  |> mutate(days_this_period = iidda.analysis::num_days(period_start_date, period_end_date))
  |> mutate(period_mid_date = iidda.analysis::mid_dates(period_start_date, period_end_date, days_this_period))
  |> left_join(harmonized_population |> group_by(iso_3166_2) |> mutate(date = as.Date(date))
               , by = dplyr::join_by(iso_3166_2, closest(period_mid_date >= date))
               , multiple = "all")
  |> select(-date)
  )
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

#' Normalize Disease Hierarchy
#'
#' Take a tidy data set with a potentially complex disease hierarchy
#' and flatten this hierarchy so that, at any particular time and location
#' (or some other context), all diseases in the `disease` column have the
#' same `nesting_disease`.
#'
#' @param data A tidy data set with the following minimal set of columns:
#' `disease`, `nesting_disease`, `basal_disease`, `period_start_date`, 
#' `period_end_date`, and `location`. Note that the latter three can be 
#' modified with `grouping_columns`.
#' @param disease_lookup A lookup table with `disease` and `nesting_disease`
#' columns that describe a global disease hierarchy that will be applied
#' locally to flatten disease hierarchy at each point in time and space
#' in the tidy data set in the `data` argument.
#' @param grouping_columns Character vector of column names to use when
#' grouping to determine the context.
#' @param basal_diseases_to_prune Character vector of `disease`s to
#' remove from `data`.
#' @param find_unaccounted_cases Make new records for instances when the sum of 
#' leaf diseases is less than the reported total for their basal disease. 
#' @param specials_pattern Optional regular expression to use to match
#' `disease` names in `data` that should be added to the lookup table. This
#' is useful for disease names that are not historical and produced for
#' harmonization purposes. The most common example is `"_unaccounted$"`,
#' which is the default. Setting this argument to `NULL` avoids adding
#' any special disease names to the lookup table.
#'
#' @export
normalize_disease_hierarchy = function(data
                                     , disease_lookup
                                     , grouping_columns = c("period_start_date", "period_end_date", "location")
                                     , basal_diseases_to_prune = character()
                                     , find_unaccounted_cases = TRUE
                                     , specials_pattern = "_unaccounted$"
) {
  
  if(!'basal_disease' %in% names(data)) {stop('The column basal_disease is not in your dataframe. 
                                           Add it using iidda::add_basal_disease()')}
  
  # only need the lookup table to infer the hierarchy
  disease_lookup = (disease_lookup
    |> select(disease, nesting_disease)
    |> distinct()
  )
  
  if (find_unaccounted_cases) data = find_unaccounted_cases(data)
  
  if (!is.null(specials_pattern)) {
    specials = (data
      |> filter(grepl(specials_pattern, disease))
      |> select(disease, nesting_disease)
      |> distinct()
    )
    disease_lookup = bind_rows(disease_lookup, specials)
  }
  pruned_lookup = (disease_lookup
     |> filter(!disease %in% basal_diseases_to_prune)
     |> mutate(nesting_disease = ifelse(
       nesting_disease %in% basal_diseases_to_prune
       , ''
       , nesting_disease
     )
     )
  )
  
  (data
    
      # remove AIDS from ontario 1990-2021 source as it is not mutually exclusive from HIV
      |> filter(!(disease == 'AIDS' & original_dataset_id == "cdi_on_1990-2021_wk"))
      
      # prune basal_diseases
      |> mutate(x = disease %in% basal_diseases_to_prune)
      |> mutate(y = nesting_disease %in% basal_diseases_to_prune)
      |> mutate(z = basal_disease %in% basal_diseases_to_prune)
      
      |> filter(!x)
      |> mutate(nesting_disease = ifelse(y, "", nesting_disease))
      |> rowwise()
      |> mutate(basal_disease = ifelse(z, basal_disease(disease, pruned_lookup), basal_disease))
      |> ungroup()
      
      # keeping only leaf diseases
      |> group_by(across(c("basal_disease", all_of(grouping_columns)))) # period_start_date, period_end_date, location, basal_disease)
      |> filter(is_leaf_disease(disease, nesting_disease))
      |> ungroup()
      
      # if there is only the basal disease (no sub-diseases), differentiate by adding '-only'
      # mutate(disease = ifelse(disease == basal_disease, sprintf("%s-only", disease), disease))
      # mutate(nesting_disease = basal_disease)
      |> select(-x, -y, -z)
    
  )
}

time_scale_chooser = function(time_scale, which_fun) {
  time_scale_order = c("wk", "2wk", "mo", "qr", "3qr","yr")
  time_scale = as.character(time_scale)
  bad_scale = !time_scale %in% time_scale_order
  if (any(bad_scale)) {
    these_bad_scales = paste0(time_scale[bad_scale], collapse = ", ")
    stop(
      "\nThese scales where found in the data but are not on the valid list:\n"
      , these_bad_scales,
      , "\nValid scales include these:\n"
      , paste0(time_scale_order, collapse = ", ")
    )
  }
  time_scale_factor = factor(time_scale, levels = time_scale_order)
  r = time_scale[which_fun(as.numeric(time_scale_factor))]
  if (length(r) != 1L) stop("Unable to choose a single time scale.")
  r
}

#' Factor Time Scale
#'
#' @param data A tidy data set with a `time_scale` column.
#'
#' @return A data set with a factored time_scale column.
#'
#' @export
factor_time_scale = function(data){
  if (is.factor(data$time_scale)) {
    return(data)
  }
  time_scale_map = c(wk = "wk", yr = "yr", mo = "mo", `2wk` = "2wk", mt = "mo", qrtr = "qr", qr = "qr", `3qr` = "3qr")
  data$time_scale = time_scale_map[as.character(data$time_scale)]
  order = c("wk", "2wk", "mo", "qr", "3qr","yr")
  
  return(mutate(data, time_scale = factor(data$time_scale, levels = order, ordered = TRUE)))
}



#' Normalize Time Scales
#'
#' Choose a single best `time_scale` for each year in a dataset, grouped by
#' nesting disease. This best `time_scale` is defined as the longest
#' of the shortest time scales in each location and sub-disease.
#'
#' @param data A tidy data set with columns `time_scale`, `period_start_date`
#' and `period_end_date`.
#' @param initial_group Character vector naming columns for defining
#' the initial grouping used to compute the shortest time scales.
#' @param final_group Character vector naming columns for defining the final
#' grouping used to compute the longest of the shortest time scales.
#' @param get_implied_zeros Add zeros that are implied by a '0' reported at a coarser timescale.
#' @param aggregate_if_unavailable If a location is not reporting for the determined
#' 'best timescale', but is reporting at a finer timescale, aggregate this finer
#' timescale to the 'best timescale'.
#'
#' @return A data set only containing records with the optimal time scale.
#'
#' @importFrom lubridate year
#' @export
normalize_time_scales = function(data
                                 , initial_group = c("year", "iso_3166", "iso_3166_2", "disease", "nesting_disease", "basal_disease")
                                 , final_group = c("basal_disease")
                                 , get_implied_zeros = TRUE
                                 , aggregate_if_unavailable = TRUE
) {
  
  if(get_implied_zeros) data = get_implied_zeros(data)
  
  if (length(unique(data$time_scale)) == 1L) return(data)
  
  if (!"period_mid_date" %in% colnames(data)) {
    (data = data
     |> mutate(days_this_period = iidda.analysis::num_days(period_start_date, period_end_date))
     |> mutate(period_mid_date = iidda.analysis::mid_dates(period_start_date, period_end_date, days_this_period))
    )
  }
  
  data = mutate(data, year = lubridate::year(period_mid_date))
  
  new_data = (data
    # remove '_unaccounted' cases when deciding best time_scale
    |> factor_time_scale()
    |> filter(!grepl("_unaccounted$", disease))
    |> group_by(across(all_of(c("year", initial_group))))
    |> mutate(shortest_time_scale = time_scale_chooser(time_scale, which.min))
    |> ungroup()
    |> group_by(across(all_of(c("year", final_group))))
    |> mutate(best_time_scale = time_scale_chooser(shortest_time_scale, which.max))
    |> ungroup()
    |> filter(as.character(time_scale) == best_time_scale)
    |> select(-best_time_scale, -shortest_time_scale)
  )
  
  # adding "unaccounted" data back, at the best_time_scale
  all_new_data = (data
    |> filter(grepl("_unaccounted$", disease))
    |> semi_join(select(new_data, "year", "time_scale", "disease", "nesting_disease", "basal_disease") |> unique(),
                 by = c("year", "time_scale", final_group))
    |> rbind(new_data)
  )
  
  if(aggregate_if_unavailable) {
    
    # coarse scales to aggregate to
    scales = (all_new_data
      |> select(period_start_date, period_end_date, disease, nesting_disease, basal_disease)
      |> unique()
      |> rename(coarser_start_date = period_start_date,
                coarser_end_date = period_end_date)
    )
    
    # data which isn't available at 'best_time_scale' for the year, but is
    # available at a finer timescale
    data_to_aggregate = (data
       |> factor_time_scale()
       |> left_join(select(all_new_data, "year","disease", "nesting_disease", "basal_disease", "time_scale") |> unique(),
                    by = c("year", "disease", "nesting_disease", "basal_disease"),
                    suffix = c('_old', '_new'))
       |> filter(time_scale_old < time_scale_new)
       |> mutate(period_start_date = as.Date(period_start_date),
                 period_end_date = as.Date(period_end_date))
       
       # keep only data which isn't available at the 'best time scale' (which is now the timescale in all_new_data)
       |> anti_join(select(all_new_data,"iso_3166_2", "year","disease", "nesting_disease", "basal_disease", "time_scale") |> unique()
                    , by = c('time_scale_new' = 'time_scale', 'disease', 'year', 'nesting_disease','basal_disease', 'iso_3166_2'))
    )
    
    aggregated_unavailable_data = (scales
       |> inner_join(data_to_aggregate, by = c("disease", "nesting_disease", "basal_disease"), relationship = 'many-to-many')
       |> filter(period_end_date > coarser_start_date & period_end_date <= coarser_end_date)
       |> select(names(data_to_aggregate), coarser_start_date, coarser_end_date)
       
       |> group_by(iso_3166, iso_3166_2, disease, nesting_disease, basal_disease, coarser_start_date, coarser_end_date)
       |> mutate(cases_coarse_period = sum(as.numeric(cases_this_period)))
    )
    
    if ("population" %in% colnames(aggregated_unavailable_data)) {
      aggregated_unavailable_data = (aggregated_unavailable_data %>%
        mutate(population = round(mean(as.numeric(population), na.rm = TRUE))))
    }
    
    if ("population_reporting" %in% colnames(aggregated_unavailable_data)) {
      aggregated_unavailable_data = (aggregated_unavailable_data %>%
        mutate(population_reporting = round(mean(as.numeric(population_reporting), na.rm = TRUE))))
    }
    
    aggregated_unavailable_data = 
      (aggregated_unavailable_data
       |> ungroup()
       
       |> select(-cases_this_period, -period_start_date, -period_end_date,
                 -days_this_period, -period_mid_date)
       |> rename(time_scale = time_scale_new,
                 cases_this_period = cases_coarse_period,
                 period_start_date = coarser_start_date,
                 period_end_date = coarser_end_date)
       
       |> distinct(iso_3166, iso_3166_2, disease, nesting_disease, basal_disease,
                   period_start_date, period_end_date, .keep_all = TRUE)
       
       # add back days_this_period and period_mid_date for the coarser start and end dates
       |> mutate(days_this_period = iidda.analysis::num_days(period_start_date, period_end_date))
       |> mutate(period_mid_date = iidda.analysis::mid_dates(period_start_date, period_end_date, days_this_period))
       |> select(-time_scale_old)
       |> mutate(record_origin = 'derived-aggregated-timescales')
       
       |> mutate(original_dataset_id = '',
                 historical_disease = '',
                 dataset_id = '')
    )
    
    if(!"record_origin" %in% names(data)) all_new_data = mutate(all_new_data, record_origin = 'historical')
    
    final = (all_new_data
       |> rbind(aggregated_unavailable_data)
    )
    
    return(final)
  } else{
    return(all_new_data)
  }
}

#' Get Implied Zeros
#'
#' Add zeros to data set that are implied by a '0' reported at a coarser timescale.
#' 
#'@param data A tidy data set with the following minimal set of columns:
#' `disease`, `nesting_disease`, `year`, `original_dataset_id`, `iso_3166_2`,
#' `basal_disease`, `time_scale`, `period_start_date`, `period_end_date`, 
#' `period_mid_date`, `days_this_period`, `dataset_id`
#' @return A tidy data set with inferred 0s.
#'
#' @importfrom lubridate year
#' @export
get_implied_zeros = function(data){
  
  if (!"period_mid_date" %in% colnames(data)) {
    (data = data
     |> mutate(days_this_period = iidda.analysis::num_days(period_start_date, period_end_date))
     |> mutate(period_mid_date = iidda.analysis::mid_dates(period_start_date, period_end_date, days_this_period))
    )
  }
  
  starting_data = (data
     |> mutate(year = lubridate::year(as.Date(period_mid_date)))
     |> factor_time_scale()
     
     |> group_by(iso_3166_2, disease, year, original_dataset_id)
     |> mutate(all_zero = ifelse(sum(as.numeric(cases_this_period)) == 0, TRUE, FALSE))
     |> ungroup()
     
     |> group_by(disease, year, original_dataset_id)
     |> mutate(finest_timescale = min(time_scale))
     |> ungroup()
  )
  
  scales = (starting_data
      |> filter(time_scale == finest_timescale)
      |> distinct(disease, nesting_disease, basal_disease,
                  year, time_scale, period_start_date, period_end_date, 
                  period_mid_date, days_this_period, original_dataset_id)
  )
  
  # records for which all_zero = true and finest_timescale isn't available
  get_new_zeros = (starting_data                
     |> filter(time_scale > finest_timescale, all_zero)
     
     # filter for timescales that are not in the original data
     |> anti_join(starting_data
                  , by = c('iso_3166_2', 'year', 'finest_timescale' = 'time_scale',
                           'disease', 'nesting_disease', 'basal_disease', 'dataset_id'
                  ))
     
     |> select(-period_start_date, -period_end_date, -period_mid_date,
               -days_this_period)
  ) 
  
  # for rows in get_new_records, find the periods (i.e. start and end dates)
  # for the finest_timescale for a given year, disease, and original_dataset_id
  new_zeros = (get_new_zeros
     |> left_join(scales, by = c('disease', 'year', 'finest_timescale' = 'time_scale',
                                 'nesting_disease', 'basal_disease', 'original_dataset_id'), 
                  relationship = "many-to-many")
     |> select(-time_scale, -year, -all_zero)
     |> rename(time_scale = finest_timescale)
     
     |> mutate(record_origin = 'derived-implied-zero')
     
     |> mutate(original_dataset_id = '',
               historical_disease = '',
               dataset_id = '')
  )
  
  if(!"record_origin" %in% names(data)) data = mutate(data, record_origin = 'historical')
  
  # join back to original data
  (data
    |> rbind(new_zeros)
  )
}


#' Find Unaccounted Cases
#'
#' Make new records for instances when the sum of leaf diseases is less than
#' the reported total for their basal disease. The difference between these
#' counts gets disease name 'basal_disease'_unaccounted'.
#'
#'
#' @param data A tidy data set with a `basal_disease` column.
#'
#' @return A data set containing records that are the difference between a
#' reported total for a basal_disease and the sum of their leaf diseases.
#'
#' @export
find_unaccounted_cases = function(data){
  
  # check if sum of leaf diseases = reported sum of basal disease
  sum_of_leaf = (
    data
    |> filter(disease != nesting_disease)
    |> group_by(iso_3166, iso_3166_2, period_start_date, period_end_date, nesting_disease, basal_disease)
    |> filter(!disease %in% unique(nesting_disease))
    |> mutate(cases_this_period = sum(as.numeric(cases_this_period)))
    |> ungroup()
    |> distinct(iso_3166, iso_3166_2, period_start_date, period_end_date, 
                nesting_disease, original_dataset_id, cases_this_period)
  )
  
  reported_totals = (
    data
    |> filter(nesting_disease == '' | is.na(nesting_disease))
    |> filter(disease %in% sum_of_leaf$nesting_disease)
    |> select(-nesting_disease)
    |> rename(nesting_disease = disease)
  )
  
  # if sum of leaf diseases is < reported sum of basal disease,
  # make new sub-disease called 'disease-name'_unaccounted, which contains
  # the difference between sum of leaf diseases and the reported sum of the disease
  unaccounted_data =
    (inner_join(sum_of_leaf, reported_totals, by =
                  c('iso_3166', 'iso_3166_2', 'period_start_date', 
                    'period_end_date', 'nesting_disease', 'original_dataset_id'),
                suffix = c('_sum', '_reported'))
     
     |> mutate(cases_this_period_reported = as.numeric(cases_this_period_reported),
                cases_this_period_sum = as.numeric(cases_this_period_sum))
     
     |> filter(cases_this_period_sum < cases_this_period_reported)
     |> mutate(cases_this_period = cases_this_period_reported - cases_this_period_sum)
     |> mutate(disease = paste(nesting_disease, 'unaccounted', sep = '_'))
     
     |> select(-cases_this_period_reported, -cases_this_period_sum)
     
     |> mutate(original_dataset_id = '',
               historical_disease = '',
               dataset_id = '')
     |> mutate(record_origin = 'derived-unaccounted-cases')
    )
  
  if(!"record_origin" %in% names(data)) data = mutate(data, record_origin = 'historical')
  
  (data
    |> rbind(unaccounted_data)
  )
}


#' Normalize Duplicate Sources
#'
#' Filter out overlapping sources for the same `disease/nesting_disease/basal_disease`, 
#' `period_start_date`, `period_end_date` , and `iso_3166_2`, with the choice to
#' keep either national level data (i.e. from Statistics Canada / Dominion 
#' Bureau of Statistics / Health Canada) or provincial level data (from a 
#' provincial ministry of Health).
#'
#' @param data A tidy data set with columns `dataset_id` , `period_start_date`,
#' `period_end_date` , `disease` , `nesting_disease` , `basal_disease`, and 
#' `time_scale`.
#' @param preferred_jurisdiction 'national' or 'provincial', indicating which 
#' jurisdiction level will be kept if these sources overlap. 
#'
#' @return A data set with no overlapping sources. 
#'
#' @export
normalize_duplicate_sources = function(data, preferred_jurisdiction = 'national'){
  
  if (!preferred_jurisdiction %in% c('provincial', 'national')) {
    stop("preferred_jurisdiction must be either 'provincial' or 'national'")
  }
  
  opposite_jurisdiction = ifelse(preferred_jurisdiction == 'national', 'provincial', 'national')
  
  overlap_info = (data
    |> mutate(source_jurisdiction_level = ifelse(grepl('_ca_', dataset_id), 'national', 'provincial'))
    
    |> mutate(days_this_period = iidda.analysis::num_days(period_start_date, period_end_date))
    |> mutate(period_mid_date = as.Date(iidda.analysis::mid_dates(period_start_date, period_end_date, days_this_period)))
    
    |> mutate(tag = ifelse(time_scale == 'yr',
                           paste(iso_3166_2, disease, time_scale, format(period_mid_date, "%Y"), sep = '-'),
                           paste(iso_3166_2, disease, time_scale, format(period_mid_date, "%b-%Y"), sep = '-'))
    )
    
    |> mutate(nesting_tag = ifelse(nesting_disease != '',
                                   ifelse(time_scale == 'yr',
                                          paste(iso_3166_2, nesting_disease, time_scale, format(period_mid_date, "%Y"), sep = '-'),
                                          paste(iso_3166_2, nesting_disease, time_scale, format(period_mid_date, "%b-%Y"), sep = '-')),
                                   '')
    )
    
    |> mutate(basal_tag = ifelse(nesting_disease != basal_disease & disease != basal_disease,
                                 ifelse(time_scale == 'yr', 
                                        paste(iso_3166_2, basal_disease, time_scale, format(period_mid_date, "%Y"), sep = '-'),
                                        paste(iso_3166_2, basal_disease, time_scale, format(period_mid_date, "%b-%Y"), sep = '-')),
                                 '')
                  )
  )
  
  preferred = filter(overlap_info, source_jurisdiction_level == preferred_jurisdiction)
  other = filter(overlap_info, source_jurisdiction_level == opposite_jurisdiction)
  
  tag = anti_join(other, preferred, by = 'tag')
  nesting_tag = anti_join(tag, preferred, by = c('nesting_tag' = 'tag'))
  normalized_sources = (anti_join(nesting_tag, preferred, by = c('basal_tag' = 'tag'))
                        |> rbind(preferred)
                        #|> select(names(data))
  )
  
  normalized_sources |> select(-source_jurisdiction_level, -tag, -nesting_tag, -basal_tag)
  
}
