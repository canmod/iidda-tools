
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
#' @importFrom iidda is_empty
#' @concept normalization
#' @export
normalize_location = function(data) filter(data, !is_empty(iso_3166_2))

#' Order Canadian Provinces Geographically
#'
#' @param data Dataset containing an `iso_3166_2` field with Canadian province
#' and territory codes.
#'
#' @concept normalization
#' @export
ca_iso_3166_2 = function(data) {
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
  mutate(data, iso_3166_2 = factor(sub("CA-", "", iso_3166_2), geog_order))
}


#' Normalize Population
#'
#' @param data Tidy dataset with columns period_start_date, period_end_date
#' iso_3166_2.
#' @param harmonized_population Harmonized population data with
#' columns date, iso_3166_2, and population (other columns will be dropped).
#'
#' @return Tidy dataset joined with harmonized population.
#'
#' @importFrom tidyr starts_with
#' @importFrom readr parse_date
#' @concept normalization
#' @export
normalize_population = function(data, harmonized_population) {
  pop = (harmonized_population
    |> select(iso_3166_2, date, population)
    |> group_by(iso_3166_2, date)
    |> mutate(
        date = as.Date(date)
      , population = as.numeric(population)
      , n = n()
    )
    |> ungroup()
  )
  if (any(pop$n > 1L)) stop("population dataset contains multiple records for some year-location pairs.")
  (data
    |> mutate(across(tidyr::starts_with("period_"), as.Date))
    |> mutate(
        days_this_period = iidda.analysis::num_days(
            period_start_date
          , period_end_date
        )
    )
    |> mutate(
        period_mid_date = iidda.analysis::mid_dates(
            period_start_date
          , period_end_date
          , days_this_period
       )
    )
    |> left_join(pop
       , by = dplyr::join_by(iso_3166_2, closest(period_mid_date >= date))
       , relationship = "many-to-one"
    )
    |> select(-date, -n)
  )
}


#' Is Leaf Disease
#'
#' Given a set of `disease`-`nesting_disease` pairs that all share the same
#' \code{\link{basal_disease}}, identify
#'
#' @param disease Disease name vector.
#' @param nesting_disease Vector of the same length as \code{disease} giving
#' the nesting diseases of element in \code{disease}.
#'
#' @return True if disease is never a nesting disease (it is a leaf disease),
#' False if disease is a nesting disease.
#' @concept normalization
#' @export
is_leaf_disease = function(disease, nesting_disease) {
  x = !disease %in% unique(nesting_disease)
  #if (any(disease %in% "typhoid-paratyphoid-fever")) browser()
  x
}

# Function to precompute all paths in the hierarchy
precompute_paths <- function(hierarchy) {
  # Recursive function to trace the path of a disease up to the root
  trace_path <- function(disease, hierarchy) {
    parent <- hierarchy$nesting_disease[hierarchy$disease == disease]

    # If there is no parent (NA or empty string), return the disease itself
    if (length(parent) == 0 || is.na(parent) || parent == "") {
      return(c(disease))
    } else {
      # Recursively find the path for the parent and append this disease
      return(c(trace_path(parent, hierarchy), disease))
    }
  }

  # Precompute the path for every disease in the hierarchy
  unique_diseases <- unique(hierarchy$disease)
  all_paths <- lapply(unique_diseases, trace_path, hierarchy = hierarchy)
  names(all_paths) <- unique_diseases

  return(all_paths)
}

# Function to identify leaf diseases in a subset using precomputed paths
find_leaves_with_precomputed <- function(subset, precomputed_paths) {
  # A disease is a leaf if it is NOT in the path of any other disease in the subset
  is_leaf <- sapply(subset, function(disease) {
    # Get the path for this disease from the precomputed paths
    path <- precomputed_paths[[disease]]

    # Check if this disease appears in any other disease's path
    no_other_has_as_ancestor <- !any(sapply(subset[subset != disease], function(other_disease) {
      disease %in% precomputed_paths[[other_disease]]
    }))

    # Return TRUE if no other disease in the subset includes this one in its path
    return(no_other_has_as_ancestor)
  })

  # Return the leaf diseases
  return(subset[is_leaf])
}


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
#' @concept normalization
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
  paths = precompute_paths(pruned_lookup)

  (data

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
      |> group_by(across(c("basal_disease", all_of(grouping_columns))))
      #filter(is_leaf_disease(disease, nesting_disease))
      |> filter(disease %in% find_leaves_with_precomputed(disease, paths))
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
#' @concept normalization
#' @export
factor_time_scale = function(data){
  ## TODO: consider renaming to time_scale_as_factor or something??
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
#' @concept normalization
#' @importFrom lubridate year
#' @export
normalize_time_scales = function(data
                                 , initial_group = c("year", "iso_3166", "iso_3166_2", "disease", "nesting_disease", "basal_disease")
                                 , final_group = c("basal_disease")
                                 , get_implied_zeros = TRUE
                                 , aggregate_if_unavailable = TRUE
) {

  if (any(c("iso_3166", "iso_3166_2") %in% final_group)) {
    if (aggregate_if_unavailable) {
      message("turning off aggregation_if_unavailable because location information is in the final_group. please read the help file for normalized_time_scales about aggregation_if_unavailable.")
      aggregate_if_unavailable = FALSE
    }
  }

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
  minimal_columns = c("year", "time_scale", "disease", "nesting_disease", "basal_disease")
  by_columns = c("year", "time_scale", intersect(final_group, minimal_columns)) |> unique()
  all_new_data = (data
    |> filter(grepl("_unaccounted$", disease))
    |> semi_join(select(new_data, minimal_columns) |> unique(),
                 by = by_columns)
    |> rbind(new_data)
  )

  if (aggregate_if_unavailable) {

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

    final = bind_rows(all_new_data, aggregated_unavailable_data)

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
#' @concept normalization
#' @importFrom lubridate year
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
     |> mutate(all_zero = isTRUE(sum(as.numeric(cases_this_period)) == 0))
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
  bind_rows(data, new_zeros)
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
#' @concept normalization
#' @importFrom iidda source_from_digitization_id
#' @export
find_unaccounted_cases = function(data){

  data = mutate(data, source_id = source_from_digitization_id(digitization_id))
  # check if sum of leaf diseases = reported sum of basal disease
  sum_of_leaf = (
    data
    |> filter(disease != nesting_disease)
    |> group_by(iso_3166, iso_3166_2, period_start_date, period_end_date, nesting_disease, basal_disease)
    |> filter(!disease %in% unique(nesting_disease))
    |> mutate(cases_this_period = sum(as.numeric(cases_this_period)))
    |> ungroup()
    |> distinct(iso_3166, iso_3166_2, period_start_date, period_end_date,
                nesting_disease, source_id, cases_this_period)
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
#' @concept normalization
#' @export
normalize_duplicate_sources = function(data, preferred_jurisdiction = 'national'){

  if (!preferred_jurisdiction %in% c('provincial', 'national')) {
    stop("preferred_jurisdiction must be either 'provincial' or 'national'")
  }

  sep = " *** "  ## for separating different fields to produce a tag

  opposite_jurisdiction = ifelse(preferred_jurisdiction == 'national', 'provincial', 'national')
  if (!"original_dataset_id" %in% names(data)) {
    if ("dataset_id" %in% names(data)) {
      data$original_dataset_id = data$dataset_id
    } else {
      stop("missing dataset id information")
    }
  }

  overlap_info = (data
    |> mutate(source_jurisdiction_level = ifelse(grepl('_ca_', original_dataset_id), 'national', 'provincial'))

    |> mutate(days_this_period = iidda.analysis::num_days(period_start_date, period_end_date))
    |> mutate(period_mid_date = as.Date(iidda.analysis::mid_dates(period_start_date, period_end_date, days_this_period)))

    |> mutate(tag = ifelse(time_scale == 'yr',
                           paste(iso_3166_2, disease, time_scale, format(period_mid_date, "%Y"), sep = sep),
                           paste(iso_3166_2, disease, time_scale, format(period_mid_date, "%b-%Y"), sep = sep))
    )

    |> mutate(nesting_tag = ifelse(nesting_disease != '',
                                   ifelse(time_scale == 'yr',
                                          paste(iso_3166_2, nesting_disease, time_scale, format(period_mid_date, "%Y"), sep = sep),
                                          paste(iso_3166_2, nesting_disease, time_scale, format(period_mid_date, "%b-%Y"), sep = sep)),
                                   '')
    )

    |> mutate(basal_tag = ifelse(nesting_disease != basal_disease & disease != basal_disease,
                                 ifelse(time_scale == 'yr',
                                        paste(iso_3166_2, basal_disease, time_scale, format(period_mid_date, "%Y"), sep = sep),
                                        paste(iso_3166_2, basal_disease, time_scale, format(period_mid_date, "%b-%Y"), sep = sep)),
                                 '')
                  )
  )

  preferred = filter(overlap_info, source_jurisdiction_level == preferred_jurisdiction)
  other = filter(overlap_info, source_jurisdiction_level == opposite_jurisdiction)

  tag = anti_join(other, preferred, by = 'tag')
  nesting_tag = anti_join(tag, preferred, by = c('nesting_tag' = 'tag'))
  normalized_sources = (anti_join(nesting_tag, preferred, by = c('basal_tag' = 'tag'))
                        |> rbind(preferred)
  )

  normalized_sources |> select(-source_jurisdiction_level, -tag, -nesting_tag, -basal_tag)

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
#' @concept normalization
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

