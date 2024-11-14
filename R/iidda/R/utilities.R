#' Is Empty
#'
#' Return \code{\link{TRUE}} if a string is empty. Emptiness means that
#' any one of the following is true: \code{NA}, \code{NaN},
#' \code{nchar(as.character(x)) == 0L},
#' \code{tolower(as.character(x)) == "na"}
#'
#' @param x object to test
#'
#' @export
is_empty = function(x) {
  is.na(x) | is.nan(x) | is.null(x) | (nchar(as.character(x)) == 0L) | (tolower(as.character(x)) == 'na')
}

#' Empty is Blank
#'
#' Force empty strings to be blank. See \code{\link{is_empty}}.
#'
#' @inheritParams is_empty
#' @export
empty_is_blank = function(x) {
   ifelse(is_empty(x), '', as.character(x))
}

#' Open a Path on Mac OS or Windows
#'
#' @inheritParams strip_blob_github
#' @param command Command-line function to use to open the file (not
#' applicable on Windows systems.
#' @param args Additional options to pass to \code{command} (ignored on
#' Windows systems).
#'
#' @export
open_locally = function(urls, command = 'open', args = character()) {
  paths = strip_blob_github(urls)
  sys_name = Sys.info()["sysname"]
  if (sys_name == "Windows") {
    for (path in paths) shell.exec(path)
  } else if (sys_name == "Darwin") {
    system2(command = c(command, paths, args), stdout = FALSE)
  } else {
    stop("Currently only works on Windows and MacOS.")
  }
}

#' @describeIn open_locally Open IIDDA pipeline resources locally.
#' @param id Resource ID.
#' @param type Type of resource.
#' @export
open_resources_locally = function(id, type = c("scans", "digitizations", "prep-scripts", "access-scripts")) {
  type = match.arg(type)
  glob = sprintf("pipelines/*/%s/%s.*", type, id)
  paths = Sys.glob(glob)
  is_metadata = grepl("^[a-zA-Z0-9_-]+\\.[a-zA-Z0-9_-]+\\.json", basename(paths))
  paths = paths[!is_metadata]
  if (length(paths) > 0L) {
    open_locally(paths)
  } else {
    message("cannot find ", glob)
  }
}

#' @describeIn open_locally Open all pipeline resources regardless of
#' resource type.
#' @export
open_all_resources_locally = function(id) {
  open_resources_locally(id, "scans")
  open_resources_locally(id, "digitizations")
  open_resources_locally(id, "prep-scripts")
  open_resources_locally(id, "access-scripts")
}

#' @describeIn open_locally Open scans locally.
#' @export
open_scans_locally = function(id) open_resources_locally(id, "scans")

#' @describeIn open_locally Open digitizations locally.
#' @export
open_digitizations_locally = function(id) open_resources_locally(id, "digitizations")

open_dataset_locally = function(id) {
  open_locally(file.path("derived-data", id, sprintf("%s.csv", id)))
}

open_provenance_locally = function(data, row_number) {
  open_scans_locally(data$scan_id[row_number])
  open_digitizations_locally(data$digitization_id[row_number])
  id = data$original_dataset_id[row_number]
  d = file.path("derived-data", id, sprintf("%s.csv", id)) |> read_data_frame()
  all_in = function(cols) all(cols %in% names(data)) & all(cols %in% names(d))
  if (all_in(c("period_start_date", "period_end_date"))) {
    d = filter(d
      , period_start_date == data$period_start_date[row_number]
      , period_end_date == data$period_end_date[row_number]
    )
  }
  if (all_in("historical_disease")) {
    d = filter(d
      , historical_disease == data$historical_disease[row_number]
    )
  }
  if (all_in("historical_disease_subclass")) {
    d = filter(d
      , historical_disease_subclass == data$historical_disease_subclass[row_number]
    )
  }
  if (all_in("historical_disease_family")) {
    d = filter(d
      , historical_disease_family == data$historical_disease_family[row_number]
    )
  }
  d
}
