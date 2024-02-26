#' Compare Two Excel Files
#'
#' Report on the differences between two xlsx files.
#'
#' @param path_one Path to an Excel file.
#' @param path_two Path to an Excel file.
#' @param ... Additional arguments to pass to \code{\link{xlsx_cells}}.
#' @importFrom tidyxl xlsx_cells
#' @importFrom dplyr filter inner_join anti_join
#'
#' @returns Either `TRUE` if the two files are identical, or a list with the
#' following items.
#' * `all_equal` : Result of applying \code{\link{all.equal}} to the
#' data frames representing each Excel file.
#' * `in_both_but_different` : Data frame containing cells that are in both
#' Excel files but with different values.
#' * `in_one_only` : Data frame containing cells that are in the first
#' Excel file but not the second.
#' * `in_two_only` : Data frame containing cells that are in the second
#' Excel file but not the first.
#'
#' @export
xlsx_diff = function(path_one, path_two, ...) {
  one = (path_one
    |> xlsx_cells(...)
    |> filter(!is_blank)
    |> collapse_xlsx_value_columns()
    |> select(address, sheet, value)
  )
  two = (path_two
    |> xlsx_cells(...)
    |> filter(!is_blank)
    |> collapse_xlsx_value_columns()
    |> select(address, sheet, value)
  )
  all_equal = all.equal(one, two)
  if (isTRUE(all_equal)) return(TRUE)
  by = c("sheet", "address")
  in_both_but_different = (one
    |> dplyr::inner_join(two, by = by, suffix = c("_one", "_two"))
    |> filter(value_one != value_two)
  )
  in_two_only = (two
    |> dplyr::anti_join(one, by = by)
    |> distinct()
  )
  in_one_only = (one
    |> dplyr::anti_join(two, by = by)
    |> distinct()
  )

  nlist(all_equal, in_both_but_different, in_one_only, in_two_only)
}

#' Copy old git File Version
#'
#' Create a temporary file containing a copy of a file under git version
#' control for a particular revision of that file.
#'
#' @param file Path to file.
#' @param version_hash Git version hash.
#'
#' @returns Temporary file path containing the copy.
#'
#' @importFrom tools file_ext
#' @export
cp_git_version = function(file, version_hash) {
  temp = sprintf("%s.%s", tempfile(), tools::file_ext(file))
  ("git show %s:%s > %s"
    |> sprintf(version_hash, file, temp)
    |> system()
  )
  temp
}

#' Excel to CSV
#'
#' Convert an Excel file to a CSV file.
#'
#' @param xlsx_path Path to an Excel file.
#' @param csv_path Path to a new CSV file.
#' @importFrom tidyxl xlsx_cells
#' @importFrom readr write_csv
#' @export
xlsx_to_csv = function(xlsx_path, csv_path) {
  (xlsx_path
   |> xlsx_cells()
   |> filter(!is_blank)
   |> write_csv(csv_path)
  )
}
