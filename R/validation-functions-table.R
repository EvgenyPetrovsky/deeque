#------------------------------------------------------------------------------#
#         Library of data validation functions that are used in checks         #
#------------------------------------------------------------------------------#

# Metadata checks

#' Function that calculates number of rows in dataframe and applies UDF (user
#' defined function) to it
#'
#' @export
#' @param data dataframe
#' @param udf user-defined funcion that takes number and returns logical value
tab_hasRowCount <- function(data, udf) {
  n <- nrow(data)
  r <- udf(n)
  r
}

#' Function that calculates number of columns in dataframe and applies UDF (user
#' defined function) to it
#'
#' @export
#' @param data dataframe
#' @param udf user-defined funcion that takes number and returns logical value
tab_hasColumnCount <- function(data, udf) {
  n <- ncol(data)
  r <- udf(n)
  r
}

#' Function that checks whether column presents in the dataframe
#'
#' @export
#' @param data dataframe
#' @param column column name
tab_hasColumn <- function(data, column) {
  r <- column[1] %in% colnames(data)
  r
}

#' Function that checks whether columns presents in the dataframe
#'
#' Function returns TRUE if all columns present in the dataframe
#'
#' @export
#' @param data dataframe
#' @param columns vector of column names
tab_hasColumns <- function(data, columns) {
  r <- all(columns %in% colnames(data))
  r
}

#' Function that checks whether combination of columns contains unique value
#'
#' @export
#' @param data dataframe
#' @param columns vector of column names that must contain unique value
tab_hasUniqueKey <- function(data, columns) {
  stop_if_miss_columns(data, columns)
  if (nrow(data) == 0) {
    stop("Data frame has 0 rows, uniqueness can't be checked")
  }
  check_data <- subset(data, select = columns)
  n0 <- nrow(check_data)
  n1 <- nrow(unique(check_data))
  r <- n0 == n1
  r
}
