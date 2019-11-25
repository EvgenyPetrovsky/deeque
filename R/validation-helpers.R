#' Greater than function
#'
#' Function returns another function that checks if given value is greater than
#' to \code{x}
#'
#' @export
#' @param x - value to compare with.
udf_gt <- function(x) {
    function(value) {value > x}
}

#' Greater than or equal to function
#'
#' Function returns another function that checks if given value is greater than
#' or equal \code{x}
#'
#' @export
#' @param x - value to compare with.
udf_ge <- function(x) {
    function(value) {value >= x}
}

#' Less than function
#'
#' Function returns another function that checks if given value is less than
#' \code{x}
#'
#' @export
#' @param x - value to compare with.
udf_lt <- function(x) {
    function(value) {value < x}
}

#' Less than or equal to function
#'
#' Function returns another function that checks if given value is less than
#' or equal to \code{x}
#'
#' @export
#' @param x - value to compare with.
udf_le <- function(x) {
    function(value) {value <= x}
}

#' Between function
#'
#' Function returns another function that checks if given value is in between
#' \code{x1} and \code{x2}
#'
#' @export
#' @param x1 - left border
#' @param x2 - right border
#' @param include - inclusion type (0 / none, 1 / left, 2 / right, 3 / both) 
#'   can be specified with digit, words, first letter of words
udf_between <- function(
    x1, x2,
    include = "both"
) {
    if (include %in% c("none", "n", 0)) {
        function(value) {x1 < value & value < x2}
    } else if (include %in% c("right", "l", 1)) {
        function(value) {x1 <= value & value < x2}
    } else if (include %in% c("right", "r", 2)) {
        function(value) {x1 < value & value <= x2}
    } else if (include %in% c("both", "b", 3)) {
        function(value) {x1 <= value & value <= x2}
    } else {
        function(value) {FALSE}
    }
}

#' Equal to function
#'
#' Function returns another function that checks if given value
#' is equal to \code{x}
#'
#' @export
#' @param x - value to compare with.
udf_eq <- function(x) {
    function(value) identical(value, x)
}
