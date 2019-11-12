#' Create new group of checks
#'
#' @export
new_group <- function() {
    list()
}

#' Add check into group
#'
#' @export
#'
#' @param group group of checks where new check needs to be added
#' @param new_check new check defined by \code{new_check} function
add_check <- function(group, new_check) {
    c(group, list(new_check))

}

#' Create new check
#'
#' Function takes parameters and returns data structure that represents check
#' and can de included into group and then executed
#'
#' @export
#'
#' @param description check description expressed in understandable way.
#' it should give good idea of a purpose of the check. Description should define
#' condition that data must meet. Like "Country must be always filled".
#' Good practice is to a) start with name of subject of check b) give clear and
#' concise criteria c) specify when it is applicable (if not always)
#' @param severity check severity, can be chosen from `severity` list attached
#' to package
#' @param function_name validation function name - text or function (without parameters)
#' @param ... other parameters that will be passed to validation function like 
#'   \code{column_name}, \code{udf}, etc.
new_check <- function(description, severity, function_name, ...) {
    if (typeof(function_name) == "closure") {
        function_name = as.character(substitute(function_name))
    }
    list(
        description = description,
        severity = severity,
        function_name = function_name,
        parameters = list(...)
    )
}

#' Execute check one check on data
#'
#' Function executes check on given data and creates data structure (list) with
#' check summary
#'
#' @export
#'
#' @param data dataset to be checked
#' @param check check to apply - use \code{new_check} function to create a check
apply_check <- function(data, check) {
    fun <- check$function_name
    par <- check$parameters
    par$data = data
    res <- do.call(fun, par)
    list(
        description = check$description,
        severity = check$severity,
        function_name = check$function_name,
        check_column = check$parameters$column,
        scope_size = length(res),
        valid_ratio = ratio(res),
        check_success = ratio(res) == 1
    )
}

#' Translate severity label into rank
#'
#' Function takes vector of severity labels and translates them into number
#' according to their position within \code{severity} list. Severities in list
#' start from lowest to highest. So first mentioned severity has rank = 1 and
#' every next severity has rank incremented by one
#'
#' @export
#'
#' @param severity severity value (or vector of values) from \code{severity}
#'   list
severity_rank <- function(severity) {
    init <- if (length(severity) == 0) {
        return(numeric())
    }
    # get severities from data
    labels  <- names(deeque::severity)
    # get severity ranks
    ranks   <- 1:length(labels)
    # initialize vector of ranks (initially 0 values)
    init <- replicate(n = length(severity), expr = 0)
    # sum replace severity names with ranks
    # if severity name matches to label then add its rank to 0-val vector
    Reduce(
        f = function(z, x) {z + x * (severity == labels[x])},
        x = ranks, init = init
    )
}

