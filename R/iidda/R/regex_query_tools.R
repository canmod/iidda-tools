#' Get field names for iidda queries
#'
#' @param response_type Character, one of `metadata`, `columns`, or `data_dictionary`
#' @param data_type Character, one of `cdi`, `CDI`, or `Communicable Disease
#'   Incidence` for disease incidence time series datasets, or `pop`, `Pop`,  or
#'   `Population` for historical demographic census datasets"
#'   
#' @return A Vector of field names, or if `response_type = "data_dictionary"`, a named list
#' @export
#'
#' @examples
#' 
#' query_options(response_type = "metadata"
#'               , data_type = "Communicable Disease Incidence")
#'              
#' query_options(response_type = "columns"
#'               , data_type = "Population")
#'
#' # error for disallowed searches
#' # query_options(response_type = "disease"
#' # , data_type = "Communicable Disease Incidence") 
#'
#' # in some cases, abbreviations allowed
#' query_options(response_type = "columns"
#'               , data_type = "cdi") 
#' # data dictionary returns a named list
#' query_options(response_type = "data_dictionary"
#'               , data_type = "pop")

query_options <- function(response_type = c("metadata", "columns", "data_dictionary")
                          , data_type = c("Communicable Disease Incidence", "Population")){
  
  # check strings  
  if(!(response_type %in% c("metadata", "columns", "data_dictionary"))){
    stop(message = "`response_type` must be one of `metadata` `columns`, or `data_dictionary`")}
  
  data_type[data_type %in% c("cdi", "CDI")]<-"Communicable Disease Incidence"
  data_type[data_type %in% c("pop", "Pop")] <-"Population"
  if(!(data_type %in% c("Communicable Disease Incidence", "Population"))){
    stop(message = "`datatype` must be one of `cdi`, `CDI`, or `Communicable Disease Incidence` for disease incidence time series datasets, or `pop`, `Pop`,  or `Population` for historical demographic census datasets")}
  # query metadata
  met <- iidda.api::ops$metadata(
    response_type = response_type
    , metadata_search = data_type
    , key = ".types .resourceType"
  )
  # return appropriate object
  if(response_type =="data_dictionary"){
    return(met %>% 
             unlist(recursive = FALSE) %>%
             unname %>%
             unique %>%
             na.omit)
  }
  else{return(unique(unlist(lapply(met, names))))}
}






#' Get unique tokens from iidda metadata
#'
#' @param entries List returned by \code{iidda.api::ops$metadata}
#' @param metadata_search Character, field from which unique tokens are desired
#'
#' @return Character vector of unique tokens for a given field from all iidda datasets

unique_entries <- function(entries, metadata_search){
  entries %>%
    lapply(function(ds){
      ds[[metadata_search]] %>%
        unlist %>% 
        unique
    }) %>%
    unname %>% 
    unlist
}



#' Query unique tokens for a field in iidda metadata
#'
#' @inheritParams query_options
#' @param metadata_search Character, name of a metadata field
#' @param string_comparison Character, one of `"exact"` or `"contains"`
#'
#' @return Character vector of unique tokens for a given field from all iidda datasets
#' @export
#'
#' @examples
#' 
#' unique_field_entries(response_type = "columns"
#'                      , metadata_search = "disease"
#'                      , string_comparison = "contains")
#'                      
unique_field_entries <- function(response_type = c("metadata"
                                                   , "columns"
                                                   , "data_dictionary")
                                 , metadata_search = "disease"
                                 , string_comparison = "contains"
){
  entries <- iidda.api::ops$metadata(response_type = response_type
                                     , metadata_search = metadata_search
                                     , string_comparison = string_comparison)
  ue <- unique_entries(entries, metadata_search = metadata_search)
  return(ue)
}



#' Obtain tokens matching regex strings
#'
#' @param strings Character vector of regex strings to match
#' @param tokens Character vector of tokens to match against
#'
#' @return Charaacter vector of unique matching tokens
#' @export
#'
#'
#' @examples
#' token_matcher(strings =c(".*sis.*", ".*itis.*")
#'     , tokens = unique_field_entries(response_type = "columns"
#'                                , metadata_search = "disease"
#'                                , string_comparison = "contains"))
token_matcher <- function(strings, tokens){
  lapply(strings, function(x){
    stringr::str_match_all(tokens, x)
  }) %>% 
    unlist %>% 
    unique 
  
}



#' Get data where any field(s) match inputted strings
#'
#' @inheritParams token_matcher
#' @param fields Character vector, names of field(s) to match
#' @inheritParams query_options
#' @param ... Optional arguments passed to iidda.api functions
#'
#' @return Flat data with unique entries matching 
#'
#' @export
#' 
#' @details the `filter` function searches using a logical "OR" between strings within fields, but an "AND" between fields. Searches are fast and so more complex searches may best be handled on the fly, filtering or combining the data returned by individual, simpler searches. 
#' 
#' @seealso \code{query_options()}
#'
#' @examples
#' # get some viral diseases
#' strings <- c(".*vir.*", ".*Vir.*")
#' fields <- c("disease")
#' ttr <- flexi_filter(strings, fields)
#' 
#' head(ttr)
#' 
#' # some things might be listed as viral under disease_subclass instead.
#' # concatenate data from two `flexi_filter` calls
#' all_vir <- bind_rows(ttr
#' , flexi_filter(strings, fields = "disease_subclass"))
#' 
#' 
#' head(all_vir)
#' nrow(all_vir)
#' # unfortunately place names are kind of a mess. 
#' # Probably easiest to find the intersection between two datasets to filter 
#' # by both disease and location
#' 
#' #Nova Scotia
#' nsDat <- flexi_filter(strings = c("Nova.*", "(^[nN].*)([[:punct:]]|[[:space:]])[sS]+.*")
#'      , fields = "location")
#' 
#' head(nsDat)
#' nrow(nsDat)
#' 
#' # viruses in nova scotia
#' 
#' ns_vir <- all_vir %>% inner_join(nsDat)
#' 
#' head(ns_vir)
#' nrow(ns_vir)
flexi_filter <- function(strings
                         , fields
                         , response_type = "columns"
                         , data_type = "Communicable Disease Incidence"
                         , ...
){
  # could be good to check strings
  # this was used in earlier fn:
  # data_type[data_type %in% c("cdi", "CDI")]<-"Communicable Disease Incidence"
  # data_type[data_type %in% c("pop", "Pop")] <-"Population"
  # if(!(data_type %in% c("Communicable Disease Incidence", "Population"))){
  #   stop(message = "`datatype` must be one of `cdi`, `CDI`, or `Communicable Disease Incidence` for disease incidence time series datasets, or `pop`, `Pop`,  or `Population` for historical demographic census datasets")}
  
  # get the matching tokens
  strings_to_search <-c(sapply(fields, function(field){
    tokes <- token_matcher(strings
                           , tokens = unique_field_entries(response_type = response_type
                                                           , metadata_search = field
                                                           , string_comparison = "contains"))
    setNames(list(tokes), field)
  }
  , USE.NAMES = FALSE
  ))
  # download the associated data
  do.call(iidda.api::ops$filter
          , c(list(resource_type = data_type
                   , response_type = "csv")
              , strings_to_search)
  )
  
}



