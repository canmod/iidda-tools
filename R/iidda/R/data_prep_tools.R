#' Write Tidy Digitized Data and Metadata
#'
#' @importFrom jsonlite write_json read_json
#' @importFrom dplyr `%>%`
#' @return file names where data were written
#' @export
write_tidy_data = function(tidy_data, metadata) {
  product = metadata$Product$product

  tidy_dir = strip_blob_github(metadata$Product$path_tidy_data)
  if(!dir.exists(tidy_dir)) dir.create(tidy_dir, recursive = TRUE)

  tidy_file = file.path(tidy_dir, product %.% 'csv')
  meta_file = file.path(tidy_dir, product %.% 'json')
  dict_file = file.path(tidy_dir, product %_% 'data_dictionary' %.% 'json')
  dial_file = file.path(tidy_dir, product %_% 'csv_dialect' %.% 'json')
  files = nlist(tidy_file, meta_file, dict_file, dial_file)

  make_data_cite(metadata, meta_file)
  global_dictionary = ('iidda_global_data_dictionary'
                       %>% getOption
                       %>% blob_to_raw
                       %>% read_json
  )
  local_dictionary = (metadata
                      %>% getElement('Columns')
                      %>% getElement(metadata$Product$product)
                      %>% rownames
                      %>% or_pattern
                      %>% get_with_key(l = global_dictionary, key = 'name')
  )
  write_json(local_dictionary, dict_file, pretty = TRUE, auto_unbox = TRUE)
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
  (metadata$Digitization$path_digitized_data
   %>% strip_blob_github
   %>% xlsx_cells
  )
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
