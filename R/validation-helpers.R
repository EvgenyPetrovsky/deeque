#' Greater than function
#'
#' Function returns another function that checks if given value is greater than
#' to \code{x}
#'
#' @export
#' @param x - value to compare with.
#'
#' @examples
#' hasMean(data = data.frame(col1 = [0:10]), udf = udf_gt(0))
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
    function(value) {value < x}
}

udf_between <- function(
    x1, x2,
    include = c("both", "right", "left", "none", "b", "r", "l", "n", 3, 2, 1, 0)
) {
    if (include %in% c("none", "n", 0)) {
        function(value) {x1 < value & value < x2}
    } else if (include %in% c("right", "r", 1)) {
        function(value) {x1 < value & value <= x2}
    } else if (include %in% c("right", "r", 1)) {
        function(value) {x1 <= value & value < x2}
    } else if (include %in% c("both", "b", 1)) {
        function(value) {x1 <= value & value <= x2}
    } else {
        function(value) {FALSE}
    }
}
