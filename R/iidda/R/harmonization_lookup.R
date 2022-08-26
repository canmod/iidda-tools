#' Harmonization Lookup Tables
#'
#' List of lookup tables for harmonizing historical inconsistencies
#' in naming.
#'
#' For example, NFLD and Newfoundland can both be represented using
#' the iso-3166-2 standard as CA-NL. These tables can be joined to
#' data in IIDDA to produce standardized variables that harmonize
#' historical inconsistencies.
#'
#' @format A list of data frames, one for each column with historical
#' naming inconsistencies:
#' \describe{
#'   \item{location}{
#'     \describe{
#'       \item{location}{Unique names of locations found in IIDDA}
#'       \item{iso_3166}{National jurisdiction codes}
#'       \item{iso_3166_2}{Sub-national jurisdiction codes}
#'     }
#'   }
#'   \item{sex}{
#'     \describe{
#'       \item{sex}{Unique names of sexes found in IIDDA}
#'       \item{iso_5218}{Numeric sex codes}
#'     }
#'   }
#' }
"harmonization_lookup_tables"

#' Standards
#'
#' List of lists of lists that exploits tab completion to make it convenient
#' to get vectors of all synonyms associated with a particular standard code.
#' This mechanism is useful when searching for data in IIDDA.
#'
#' @format List of lists of character vectors containing the original historical
#' names:
#' \describe{
#'   \item{location}{
#'     \describe{
#'       \item{iso_3166}{
#'         Historical national names associated with each iso-3166 code.
#'       }
#'       \item{iso_3166_inclusive}{
#'         Historical national and sub-national names associated with
#'         each iso-3166 code.
#'       }
#'       \item{iso_3166_2}{
#'         Historical sub-national names associated with each iso-3166-2 code.
#'       }
#'     }
#'   }
#'   \item{sex}{
#'     \describe{
#'       \item{iso_5218}{
#'         Historical names refering to sexes associated with
#'         each iso-5218 code.
#'       }
#'     }
#'   }
#' }
"standards"
