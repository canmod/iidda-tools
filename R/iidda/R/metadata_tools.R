#' Read Tracking Tables
#'
#' Read metadata tracking tables for an IIDDA project.
#'
#' @param path Path containing tracking tables.
#'
#' @export
read_tracking_tables = function(path) {
  valid_colnames = c("access_script", "disease_family", "disease", "disease_subclass", "icd_7", "icd_9",
    "icd_7_subclass", "icd_9_subclass", "disease_level", "column",
    "title", "type", "format", "description", "pattern", "organization",
    "name", "email", "Notes", "data_enterer", "digitized", "path_digitized_data",
    "path_original_data", "digitization", "period_start_date", "period_end_date",
    "location", "iso_3166", "iso_3166_2", "url", "province_territory",
    "notes", "source", "years", "start_date", "end_date", "frequency",
    "breakdown", "urls", "date_of_url_access", "tidy_dataset", "path_prep_script",
    "path_tidy_data", "publisher", "publicationYear", "current_version",
    "staging_url_prefix", "scan", "access_script", "prep_script", "digitization_priority",
    "data_type", "name", "description", "web_resource", "source_tag"
  )
  path_in_proj = proj_path(path)
  paths = file.path(path_in_proj, list.files(path_in_proj, pattern = '.csv'))
  output = (paths
    %>% lapply(read.csv, check.names = FALSE)
    %>% setNames(tools::file_path_sans_ext(basename(paths)))
    #%>% lapply(function(x) select(x, any_of(valid_colnames)))
    #%>% lapply(drop_empty_cols)
    %>% lapply(drop_empty_rows)
  )
  valid_tabs = vapply(output
    , \(tab) all(names(tab) %in% valid_colnames)
    , logical(1L)
  )
  if (any(!valid_tabs)) {
    for (tnm in names(output)[!valid_tabs]) {
      tmp = "Tracking table %s has invalid column names. Valid column names include:\n   %s"
      warn_msg = sprintf(tmp, tnm, paste0(valid_colnames, collapse = ", "))
      warning(warn_msg)
    }
  }
  return(output)
}

#' Read Tracking Metadata
#'
#' Read in CSV files that contain the single-source-of-truth for metadata
#' to be used in a data prep script.
#'
#' This function currently assumes that a single tidy dataset is being
#' produced from a single digitized file.
#'
#' @param tidy_dataset key to the tidy dataset being produced by the script
#' @param digitization key to the digitization being used by the script
#' @param tracking_path string giving path to the tracking data
#' @param original_format should the original tracking table format be used?
#' @param for_lbom are these data being read for the LBoM repo?
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate relocate select semi_join left_join right_join rename
#' @importFrom tibble column_to_rownames remove_rownames
#' @export
get_tracking_metadata = function(tidy_dataset, digitization, tracking_path, original_format = TRUE, for_lbom = FALSE) {
  if (in_proj()) {
    metadata = get_dataset_metadata(tidy_dataset)
    if (for_lbom) metadata = process_lbom(metadata)
    return(metadata)
  }
  current_tidy_dataset = tidy_dataset
  current_digitization = digitization
  tracking_path = proj_path(tracking_path)

  if (!original_format) {
    paths = file.path(tracking_path, list.files(tracking_path, pattern = '.csv'))
    table_list = (paths
      %>% lapply(read.csv, check.names = FALSE, colClasses = "character")
      %>% setNames(tools::file_path_sans_ext(basename(paths)))
      #%>% lapply(function(x) select(x, any_of(valid_colnames)))
      #%>% lapply(drop_empty_cols)
      %>% lapply(drop_empty_rows)
    )
    metadata = filter_new_format(table_list, current_tidy_dataset, current_digitization)
  } else {

    d = read_tracking_tables(tracking_path)

    metadata = list(
      TidyDataset = (d$TidyDatasets
                     %>% filter(tidy_dataset == current_tidy_dataset)
      ),
      Digitization = (d$Digitizations
                      %>% filter(digitization == current_digitization)
      ),
      Source = (d$Originals
                %>% filter(digitization == current_digitization)
                %>% semi_join(x = d$Sources, by = "source")
      ),
      Originals = (d$Originals
                   %>% filter(digitization == current_digitization)
                   %>% mutate(original = basename(path_original_data))
                   %>% relocate(original, .before = source)
      ),
      # deleted reference to tables.csv, move info from tables.csv to tidydataset.csv
      Columns = (d$Schema
                 %>% filter(tidy_dataset == current_tidy_dataset)
                 %>% left_join(d$Columns, by = "column")
      )
    )
  }

  finalize_tracking_tables(metadata, for_lbom)
}


add_resource_path = function(resource_table) {
  if (is.null(resource_table)) return(resource_table)

  ## drop old path columns
  path_cols = grepl("^path_", names(resource_table))
  resource_table = resource_table[, !path_cols, drop = FALSE]

  ## construct consistent paths
  id = resource_table[[1L]]
  path = file.path(
      "pipelines"
    , resource_table$source
    , gsub("_", "-", sprintf("%ss", names(resource_table)[1L]))
    , sprintf("%s.%s", id, resource_table[["extension"]])
  )

  ## if in an iidda project, prepend the remote url component
  ## TODO: do not assume main branch
  if (in_proj()) {
    path = file.path(remote_iidda_git(), "blob", "main", path)
  }

  ## create new resource path column
  resource_table$resource_path = path
  resource_table
}

add_data_path = function(dataset_table) {
  path = file.path("derived-data", dataset_table[[1L]])
  if (in_proj()) {
    path = file.path(remote_iidda_git(), "blob", "main", path)
  }
  dataset_table$path_tidy_data = path
  dataset_table
}

bind_rows_lst = function(lst) do.call(rbind, lst)
bind_rows_lst_df = function(lst, df) do.call(rbind, c(lst, list(df)))
bind_rows_robust = function(x, y) {
  if (is.data.frame(x) | is.null(x)) x = list(x)
  if (is.data.frame(y) | is.null(y)) y = list(y)
  do.call(rbind, c(x, y))
}

get_all_dataset_metadata = function(dataset) {
  ## recursion to find dependencies of dependencies
  prerequisite_dataset_ids = (dataset
    |> read_prerequisite_paths("^derived-data/[a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\\.csv$")
    |> basename()
    |> tools::file_path_sans_ext()
  )
  prerequisite_metadata = list()
  for (id in prerequisite_dataset_ids) prerequisite_metadata[[id]] = Recall(id)
  prerequisite_metadata$PrepScripts = (prerequisite_metadata
    |> lapply(getElement, "PrepScripts")
    |> Reduce(f = bind_rows_robust)
    |> unique()
  )
  prerequisite_metadata$AccessScripts = (prerequisite_metadata
    |> lapply(getElement, "AccessScripts")
    |> Reduce(f = bind_rows_robust)
    |> unique()
  )
  prerequisite_metadata$Scans = (prerequisite_metadata
    |> lapply(getElement, "Scans")
    |> Reduce(f = bind_rows_robust)
    |> unique()
  )
  prerequisite_metadata$Digitizations = (prerequisite_metadata
    |> lapply(getElement, "Digitizations")
    |> Reduce(f = bind_rows_robust)
    |> unique()
  )

  ## find direct dependencies
  PrepScripts = (dataset
    |> read_resource_metadata("^pipelines/[a-zA-Z0-9_-]+/prep-scripts/[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+$")
    |> assert_tracking_type("PrepScripts")
    |> bind_rows_robust(prerequisite_metadata$PrepScripts)
  )
  AccessScripts = (dataset
    |> read_resource_metadata("^pipelines/[a-zA-Z0-9_-]+/access-scripts/[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+$")
    |> assert_tracking_type("AccessScripts")
    |> bind_rows_robust(prerequisite_metadata$AccessScripts)
  )
  Scans = (dataset
    |> read_resource_metadata("^pipelines/[a-zA-Z0-9_-]+/scans/[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+$")
    |> assert_tracking_type("Scans")
    |> bind_rows_robust(prerequisite_metadata$Scans)
  )
  Digitizations = (dataset
    |> read_resource_metadata("^pipelines/[a-zA-Z0-9_-]+/digitizations/[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+$")
    |> assert_tracking_type("Digitizations")
    |> bind_rows_robust(prerequisite_metadata$Digitizations)
  )
  metadata = nlist(PrepScripts, AccessScripts, Scans, Digitizations) |> lapply(add_resource_path)
  sources = (metadata
    |> lapply(getElement, "source")
    |> unlist(use.names = FALSE)
    |> unique()
  )
  metadata$Columns = read_column_metadata(dataset, "^metadata/columns/[a-zA-Z0-9_-]+\\.json$")
  metadata$Columns$tidy_dataset = dataset
  metadata$Columns = relocate(metadata$Columns, tidy_dataset, .before = title)
  metadata$TidyDataset = read_global_metadata(dataset, "tidy-datasets") |> add_data_path()
  metadata$Source = read_global_metadata(sources, "sources")

  return(metadata)
}

#' Get Dataset Metadata
#'
#' Get an object with metadata information about a particular dataset from
#' tracking tables.
#'
#' @param dataset Dataset identifier.
#' @export
get_dataset_metadata = function(dataset) {
  metadata = get_all_dataset_metadata(dataset)
  metadata$TidyDataset = add_data_path(metadata$TidyDataset)
  metadata$Originals = rename(metadata$Scans, original = scan)
  metadata |> finalize_tracking_tables(FALSE)
}

filter_new_format = function(table_list, current_tidy_dataset, current_digitization) {
  if (FALSE & in_proj()) {
    add_if = function(list, path, name) {
      if (file.exists(path)) list[[name]] = read.csv(path
        , check.names = FALSE
        , colClasses = "character"
      )
      return(list)
    }
    git = remote_iidda_git()
    table_list = (table_list
      |> add_if("tracking/Sources.csv", "Sources")
      |> add_if("tracking/TidyDatasets.csv", "TidyDatasets")
      |> add_if("tracking/Columns.csv", "Columns")
      |> add_if("tracking/Organizations.csv", "Organizations")
      |> add_if(
          file.path("dataset-dependencies", current_tidy_dataset, sprintf("%s.PrepScripts.csv", tidy_dataset))
        , "PrepDependencies"
      )
      |> add_if(
          file.path("dataset-dependencies", current_tidy_dataset, sprintf("%s.Digitizations.csv", current_tidy_dataset))
        , "DigitizationDependencies"
      )
      |> add_if(
          file.path("dataset-dependencies", current_tidy_dataset, sprintf("%s.Scans.csv", current_tidy_dataset))
        , "ScanDependencies"
      )
      |> add_if(
          file.path("dataset-dependencies", current_tidy_dataset, sprintf("%s.DerivedData", current_tidy_dataset))
        , "DerivedData"
      )
      |> add_if(
          file.path("dataset-dependencies", current_tidy_dataset, sprintf("%s.Columns.csv", current_tidy_dataset))
        , "Schema"
      )
    )
    table_list$PrepScripts$path_prep_script = file.path(git, "blob", "main"
      , "pipelines"
      , table_list$PrepScripts$source
      , "prep-scripts"
      , sprintf("%s.%s",table_list$PrepScripts$prep_script, table_list$PrepScripts$extension)
    )
    table_list$Digitizations$path_digitized_data = file.path(git, "blob", "main"
      , "pipelines"
      , table_list$Digitizations$source
      , "digitizations"
      , sprintf("%s.%s", table_list$Digitizations$digitization, table_list$Digitizations$extension)
    )
    table_list$Scans$path_original_data = file.path(git, "blob", "main"
      , "pipelines"
      , table_list$Scans$source
      , "scans"
      , sprintf("%s.%s", table_list$Scans$scan, table_list$Scans$extension)
    )
  }
  lookup_tidy_dataset = data.frame(tidy_dataset = current_tidy_dataset)
  lookup_digitization = data.frame(digitization = current_digitization)
  dependency_re = "^([A-Z]{1}[a-z]*)Dependencies$"
  dep_files = grep(dependency_re, names(table_list), value = TRUE)
  dep_id = sub(dependency_re, "\\1", dep_files)
  source_file_re = sprintf("^%s.*(?<!Dependencies)$", dep_id)
  source_files = vapply(
    source_file_re,
    grep,
    character(1L),
    names(table_list),
    value = TRUE, perl = TRUE, USE.NAMES = FALSE
  )

  filtered_tidy_datasets = semi_join(
    table_list$TidyDatasets,
    lookup_tidy_dataset,
    by = "tidy_dataset"
  )
  filtered_dependencies = mapply(
    semi_join,
    table_list[dep_files],
    MoreArgs = list(
      y = lookup_tidy_dataset,
      by = "tidy_dataset"
    ),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  dependency_keys = (filtered_dependencies
    %>% lapply(names)
    %>% lapply(getElement, 1L) ## require key to be in first column!!
  )
  # this mapply filter needs to be explicit about the 'by' argument
  filtered_source_files = mapply(
    semi_join,
    table_list[source_files],
    filtered_dependencies,
    dependency_keys  # by keys
  )
  filtered_digitizations = semi_join(
    table_list$Digitizations,
    lookup_digitization,
    "digitization"
  )
  filtered_source_info = semi_join(
    table_list$Sources,
    filtered_digitizations,
    "source"
  )
  filtered_columns = table_list$Columns
  if (!is.null(table_list$Schema)) {
    filtered_columns = (table_list$Schema
       %>% filter(tidy_dataset == current_tidy_dataset)
       %>% left_join(table_list$Columns, by = "name")
    )
  }
  list(
    TidyDataset = filtered_tidy_datasets,
    Digitization = filtered_source_files$Digitizations,
    Source = filtered_source_info,
    Originals = rename(filtered_source_files$Scans, original = scan),  ## IsOriginalOf
    Columns = filtered_columns,
    PrepScript = filtered_source_files$PrepScripts,
    AccessScripts = filtered_source_files$AccessScripts
  )
}
finalize_tracking_tables = function(metadata, for_lbom) {
  metadata$Columns = (metadata$Columns
    %>% split(metadata$Columns$tidy_dataset)
    %>% lapply(remove_rownames)
    %>% lapply(column_to_rownames, var = "name")
  )

  if (nrow(metadata$Originals) > 0L) {
    metadata$Originals = split(metadata$Originals, metadata$Originals$original)
  } else {
    metadata$Originals = list(metadata$Originals)
  }
  if (for_lbom) metadata = process_lbom(metadata)
  metadata
}

process_lbom = function(metadata) {
  abs_path = metadata$Digitization$resource_path
  if (!is.null(abs_path)) {
    if (length(abs_path) == 1L & is.character(abs_path)) {
      metadata$lbom_info$relative_path = file.path(".", strip_blob_github(abs_path))
    }
  }
  metadata$lbom_info$data_category = switch(
    metadata$TidyDataset$type,
    Mortality = "mortality",
    ACM = "all-cause-mortality",
    Births = "births",
    Plague = "plague",
    Population = "population"
  )
  metadata
}



collect_tracking_metadata = function(...) {
  all_paths = Sys.glob(file.path(..., "**", "tracking", "*.csv"))
  all_data_frames = lapply(all_paths, iidda::read_data_frame)
  bound_frames = tapply(all_data_frames, basename(all_paths), dplyr::bind_rows, simplify = FALSE)
  lapply(bound_frames, \(x) unique)
}




#' Which Tracking Tables have a Particular Column
#'
#' @param metadata Output of \code{\link{read_tracking_tables}}.
#' @param col_nm Name of a column.
#' @export
tracking_tables_with_column = function(metadata, col_nm) {
  (metadata
   %>% lapply(names)
   %>% lapply(`==`, col_nm)
   %>% lapply(any)
   %>% unlist
   %>% which
  )
}

#' Tracking Table Keys
#'
#' @export
tracking_table_keys = list(
  organization = list(
    primary = c('Organizations'),
    foreign = c('Contacts', 'Sources')
  ),
  source = list(
    primary = c('Sources'),
    foreign = c('Originals')
  ),
  digitization = list(
    primary = c('Digitizations'),
    foreign = c('Originals', 'TidyDatasets')
  ),
  tidy_dataset = list(
    primary = c('TidyDatasets'),
    foreign = c('Schema')
  ),
  column = list(
    primary = c('Columns'),
    foreign = c('Schema')
  )
)


#' Melt Tracking Table Keys (Deprecated)
#'
#' To be used in conjunction with \code{\link{tracking_table_keys}}.
#'
#' @param keys Character vector of
#' @export
melt_tracking_table_keys = function(keys) {
  (keys
   %>% lapply(as.data.frame)
   %>% bind_rows(.id = 'key')
  )
}

#' Write Local Data Dictionaries
#'
#' @param metadata Output of \code{\link{read_tracking_tables}}.
#' @param path Path to a new JSON file.
#' @importFrom tibble rownames_to_column
#' @importFrom jsonlite write_json
#' @export
write_local_data_dictionaries = function(metadata, path) {
  dictionary_data_frames = (metadata$Columns
   %>% lapply(rownames_to_column, var = "name")
   %>% lapply(select, -tidy_dataset)
   # %>% lapply(toJSON, pretty = TRUE)
   # %>% setNames(names(metadata$Columns))
  )
  if (length(dictionary_data_frames) != 1L) {
    stop("cannot create data dictionary from more than one Columns data frame")
  }
  write_json(dictionary_data_frames[[1]], path, pretty = TRUE, auto_unbox = TRUE)
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
add_metadata = function(
    table,
    table_metadata,
    column_metadata
  ) {
  table = as.data.frame(table)
  table_metadata = as.list(table_metadata)
  check_tidy_data_cols(table, column_metadata)
  attr(table, 'title') = table_metadata$Title
  attr(table, 'description') = table_metadata$Description
  for (column in rownames(column_metadata)) {
    attr(table[[column]], 'label') = column_metadata[column, "title"]
    attr(table[[column]], 'description') = column_metadata[column, "description"]
  }
  table
}

make_related_identifier = function(
    metadata,
    relation_type = c("IsCompiledBy", "IsDerivedFrom", "References")
  ) {

  get_original_data_paths = function(metadata) {
    path_vec = unlist_char_list(
      lapply(metadata$Originals, `[[`, "resource_path")
    )
    path_vec[!is_empty(path_vec)]
  }

  # From Table 9 in https://schema.datacite.org/meta/kernel-4.3/doc/DataCite-MetadataKernel_v4.3.pdf
  # IsCompiledBy -- for prep scripts and access scripts -- Scripts
  # IsDerivedFrom -- for digitizations -- Digitizations
  # References -- for scans -- Originals
  relation_type = match.arg(relation_type)
  path_vec = switch(relation_type
    , IsCompiledBy = c(
      metadata$PrepScript$resource_path,
      metadata$AccessScripts$path_access_script
    )
    , IsDerivedFrom = metadata$Digitization$resource_path
    , References = get_original_data_paths(metadata)
  )
  if (length(path_vec) == 0L) return(list())
  listify_relation = function(x) list(
    relatedIdentifier = x,
    relatedIdentifierType = "URL",
    relationType = relation_type
  )
  lapply(unname(path_vec), listify_relation)
}


make_contributors = function(metadata) {
  enterers = metadata$Digitization$data_enterer |> unique()
  enterers = enterers[nchar(enterers) != 0L]
  orgs = metadata$Source$organization |> unique()
  orgs = orgs[nchar(orgs) != 0L]
    c(
      lapply(orgs, function(name) {
        list(
          # wish there was a better type than "Other", but this contributor
          # is intended to provide the organization from whom we obtained
          # the original source documents
          contributorType = "Other",
          name = name, ## TODO: this will just put the ackronym. not great.
          nameType = "Organizational"
        )
      }),
    lapply(enterers, function(name) {
      list(
        contributorType = "Other",
        name = name,
        nameType = "Personal"
      )
    })
  )
}


#' Make DataCite JSON Metadata
#'
#' @param metadata Output of get_tracking_metadata
#' @param file Path to metadata file
#' @importFrom jsonlite write_json
#' @export
make_data_cite_tidy_data = function(metadata, file) {
  name_util = function(vec, name) {
    lapply(vec, as.list) |> lapply(setNames, name)
  }

  # schema:
  # https://github.com/datacite/schema/blob/master/source/json/kernel-4.3/datacite_4.3_schema.json

  # From Table 9 in https://schema.datacite.org/meta/kernel-4.3/doc/DataCite-MetadataKernel_v4.3.pdf
  # IsCompiledBy -- for prep scripts and access scripts
  # IsDerivedFrom -- for digitizations
  # References -- for scans

  # TODO: remove much of the hard-coding below
  # https://github.com/canmod/iidda-tools/issues/7
  data_cite = list(
    # TODO: move this identifier down to alternateIdentifiers
    # https://github.com/canmod/iidda-tools/issues/8
    identifiers = list(list(
      identifier = metadata$TidyDataset$tidy_dataset,
      identifierType = 'iidda-id'
    )),
    creators = list(
      list(
        # TODO: iidda@mcmaster.ca should be the contact
        # bouncing now -- send a message to sys admin
        # https://github.com/canmod/iidda-tools/issues/9
        name = "McMaster University Theo-Bio Lab",
        nameType = "Organizational"
      )
    ),
    titles = list(
      list(
        title = metadata$TidyDataset$title,
        lang = "en"
      )
    ),
    publisher = metadata$TidyDataset$publisher,
    publicationYear = metadata$TidyDataset$publicationYear,
    subjects = list(),
    contributors = make_contributors(metadata),
    language = 'en',
    types = list(
      resourceTypeGeneral = "Dataset",
      resourceType = lookup(metadata$TidyDataset$type, resource_type_dict)[[1L]]
    ),
    # TODO: move main identifier here once we get DOI's going
    # https://github.com/canmod/iidda-tools/issues/8
    alternateIdentifiers = list(),
    relatedIdentifiers = c(
      list(),
      make_related_identifier(metadata, "IsCompiledBy"),
      make_related_identifier(metadata, "IsDerivedFrom"),
      make_related_identifier(metadata, "References")
    ),
    sizes = list(metadata$size),
    formats = list("csv"),
    version = metadata$TidyDataset$current_version,
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
        description = metadata$TidyDataset$description
      ),
      list(
        descriptionType = "Methods",
        lang = "en",
        description = paste(
          "This data set is a part of a systematic effort to make it more",
          "convenient to access publicly available historical information",
          "on infectious diseases. We are systematically and opportunistically",
          "contacting data stewards (particularly across Canada, but also",
          "internationally) to access disparate source documents.",
          "Some of these documents are digital but many others are on paper,",
          "which we scan and make publicly available. We",
          "manually enter the information provided by these scans",
          "into Excel spreadsheets, which we also make publicly",
          "available. The layout of these spreadsheets are identical to the",
          "originals, making it as easy as possible to compare the",
          "reproductions with the sources. A reproducible automated process",
          "was used to convert a digitized version of the source data into",
          "tidy CSV files. These tidy data contain all of the information in",
          "the original source documents, but are more convenient for",
          "analysis and discovery. All columns in the tidy datasets are",
          "drawn from a common data dictionary, making it easier to combine",
          "data that we obtained from different primary sources.",
          sep = " "
        )
      )
    ),
    dates = list(list(
      date = iso_8601_dateranges(
        metadata$TidyDataset$period_start_date,
        metadata$TidyDataset$period_end_date
      ),
      dateType = "Collected",
      dateInformation = paste(
        "Date ranges refer to the start and end dates of the historical",
        "period described by these data.",
        sep = " "
      )
    )),
    fundingReferences = list(
      ## TODO: add other funders on a per-dataset basis. should be done
      ## by creating a Funders.csv tracking table and linking it to the
      ## TidyDatasets.csv table.
      list(
        funderName = "Natural Sciences and Engineering Research Council of Canada",
        awardURI = "https://www.nserc-crsng.gc.ca/NSERC-CRSNG/FundingDecisions-DecisionsFinancement/2021/EIDM-MMIE_eng.asp",
        awardTitle = "CANMOD: Canadian Network for Modelling Infectious Disease",
        funderIdentifier = "https://ror.org/01h531d29",
        funderIdentifierType = "ROR",
        SchemeURI = "https://ror.org/"
      )
    ),
    geoLocations = name_util(
        metadata$geo
      , "geoLocationPlace"
    ),
    schemaVersion = "http://datacite.org/schema/kernel-4"
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}

# inheritParams make_data_cite_tidy_data
make_data_cite_digitization = function(metadata, file) {
  data_cite = list(
    identifier = list()
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}

# inheritParams make_data_cite_tidy_data
make_data_cite_scans = function(metadata, file) {
  data_cite = list(
    identifier = list()
  )
  write_json(data_cite, file, pretty = TRUE, auto_unbox = TRUE)
}

#' Get CANMOD Digitization Metadata
#'
#' Superseded by functionality in `iidda.api`.
#'
#' @param tracking_list output of \code{read_tracking_tables}
#' @export
get_canmod_digitization_metadata = function(tracking_list) {
  d = tracking_list
  (d$Sources
    %>% filter(type == "CDI", breakdown == "province",
               grepl('(weekly|4-weekly|quarterly|monthly)', frequency))
    %>% left_join(d$Originals, by = "source", suffix = c("_source", "_orig"))
    %>% left_join(d$Digitizations, by = "digitization", suffix = c("", "_digit"))
    %>% left_join(d$TidyDatasets, by = "digitization", suffix = c("", "_tidy"))
    %>% filter(!is_empty(path_tidy_data))
  )
}

#' IIDDA Data Dictionary
#'
#' Get the global data dictionary for IIDDA
#'
#' This function requires an internet connection.
#'
#' @importFrom jsonlite read_json fromJSON toJSON
#' @export
iidda_data_dictionary = function() {
  # data dictionary location
  global_data_dictionary_url = file.path(
    "https://raw.githubusercontent.com", # api
    "canmod", # github user/org
    "iidda", # github repo
    "main", # github branch
    "global-metadata", # folder
    "data-dictionary.json" # file
  )

  (global_data_dictionary_url
    %>% read_json
    %>% toJSON
    %>% fromJSON
  )
  # try(
  #   read_json(global_data_dictionary_url),
  #   silent = TRUE
  # )

  # (ops$metadata(response_type = 'data_dictionary')
  #  %>% lapply(toJSON)
  #  %>% lapply(fromJSON)
  #  %>% rbind_pages
  #  %>% unique
  #  %>% remove_rownames
  # )
  # api_template = "%{api_url}s/datasets/%{dataset_id}s?response_type=raw_csv"

}

#' Convert Harmonized Metadata
#'
#' Get metadata for a harmonized data source, given metadata for the
#' corresponding tidy data source metadata and initial harmonized data
#' source metadata.
#'
#' @param tidy_metadata Metadata from \code{\link{read_tracking_tables}} for
#' a tidy data source.
#' @param harmonized_metadata Initial metadata from
#' \code{\link{read_tracking_tables}} for a harmonized data source.
#' @param tidy_source IIDDA data source ID for a data source that is being
#' harmonized.
#' @param harmonized_dataset_id ID of dataset being harmonized.
#' @param tidy_source_metadata_path Output of \code{\link{convert_metadata_path}}.
#'
#' @export
convert_harmonized_metadata = function(tidy_metadata, harmonized_metadata, tidy_source, harmonized_dataset_id, tidy_source_metadata_path) {
  prep_scripts = tidy_metadata$PrepScripts$prep_script[tidy_metadata$PrepScripts$source == tidy_source] |> unique()

  get_ids = function(tracking_table, column_name, lookup_column_name, id_vector) {
    tracking_column = as.character(tracking_table[[column_name]])
    lookup_column = as.character(tracking_table[[lookup_column_name]])
    unique(tracking_column[lookup_column %in% id_vector])
  }
  dataset_ids = get_ids(tidy_metadata$PrepDependencies, "tidy_dataset", "prep_script", prep_scripts)
  digitization_ids = get_ids(tidy_metadata$DigitizationDependencies, "digitization", "tidy_dataset", dataset_ids)
  prep_ids = get_ids(tidy_metadata$PrepDependencies, "prep_script", "tidy_dataset", dataset_ids)
  access_ids = get_ids(tidy_metadata$AccessDependencies, "access_script", "tidy_dataset", dataset_ids)

  get_tidy_metadata = function(tidy_dataset) {
    digitization = get_ids(tidy_metadata$DigitizationDependencies, "digitization", "tidy_dataset", tidy_dataset)
    iidda::get_tracking_metadata(tidy_dataset, digitization, tidy_source_metadata_path, original_format = FALSE)
  }
  tidy_metadata_list = lapply(dataset_ids, get_tidy_metadata)
  columns = harmonized_metadata$Columns[harmonized_metadata$Columns$column %in% harmonized_metadata$Schema$column[harmonized_metadata$Schema$tidy_dataset == harmonized_dataset_id], , drop = FALSE]
  columns$tidy_dataset = harmonized_dataset_id
  rownames(columns) = columns$column
  originals = lapply(tidy_metadata_list, getElement, "Originals") |> unlist(FALSE)
  digitizations = lapply(tidy_metadata_list, getElement, "Digitization")
  TidyDataset = harmonized_metadata$TidyDatasets[harmonized_metadata$TidyDatasets$tidy_dataset == harmonized_dataset_id, , drop = FALSE]
  harmonized_prep_script = get_ids(harmonized_metadata$PrepDependencies, 'prep_script', 'tidy_dataset', TidyDataset$tidy_dataset)
  allprepscripts = rbind(harmonized_metadata$PrepScripts[harmonized_metadata$PrepScripts$prep_script %in% c(prep_ids, harmonized_prep_script), , drop = FALSE],
                         tidy_metadata$PrepScripts[tidy_metadata$PrepScripts$prep_script %in% c(prep_ids, harmonized_prep_script), , drop = FALSE])
  allaccessscripts = rbind(harmonized_metadata$AccessScripts[harmonized_metadata$AccessScripts$access_script %in% access_ids, , drop = FALSE],
                           tidy_metadata$AccessScripts[tidy_metadata$AccessScripts$prep_script %in% access_ids, , drop = FALSE])
  list(
    TidyDataset = TidyDataset,
    Source = harmonized_metadata$Sources[harmonized_metadata$Sources$source == harmonized_source, , drop = FALSE],
    Digitization = do.call(rbind, digitizations)[!duplicated(do.call(rbind, digitizations)), ],
    PrepScript = unique(allprepscripts),
    AccessScript = unique(allaccessscripts),
    Originals = originals[!duplicated(originals)],
    Columns = setNames(list(columns), harmonized_dataset_id),
    dataset_ids = dataset_ids
  )
}

#' Convert Metadata Path
#'
#' Convert a metadata path to one corresponding to tidy data being harmonized.
#'
#' @param metadata_path Path to a collection of tracking tables.
#' @param harmonized_source IIDDA data source ID for a harmonized source.
#' @param tidy_source IIDDA data source ID for a data source that is being
#' harmonized.
#'
#' @export
convert_metadata_path = function(metadata_path, harmonized_source, tidy_source) {
  gsub(harmonized_source, tidy_source, metadata_path, fixed = TRUE)
}
