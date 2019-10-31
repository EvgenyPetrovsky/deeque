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
#'
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
run_check <- function(data, check) {
    fun <- check$function_name
    par <- check$parameters
    par$data = data
    res <- do.call(fun, par)
    list(
        description = description,
        severity = check$severity,
        function_name = check$function_name,
        check_column = check$parameters$column,
        scope_size = length(res),
        valid_ratio = ratio(res),
        check_success = ratio(res) == 1
    )
}

severity_rank <- function(severity) {
    # get severities from data
    labels  <- names(deeque::severity)
    # get severity ranks
    ranks   <- 1:length(labels)
    # sum replace severity names with ranks
    Reduce(
        f = function(z, x) {z + x * (severity == labels[x])},
        x = ranks, init = rep(0, length(ranks))
    )
}

#' Function that creates severity check function.
#'
#' Function takes only severity level parameter and returns function that
#' accepts data check result and checks whether defined severity level was
#' reached. If severity level was reached function returns TRUE.
#'
#' @export
#'
#' @param severity - severity text label (INFO, WARNING, ERROR)
severity_at_threshold <- function(severity) {
    function(check_results) {
        check_sev_ranks <- severity_rank(check_results$severity)
        input_sev_rank  <- severity_rank(severity)
        filter_condition <- check_sev_ranks >= input_sev_rank & check_success == FALSE
        violations_found <- nrow(check_results[filter_condition]) > 0
        violations_found
    }
}

#' Function that creates severity check function.
#'
#' @export
#'
#' @param severity - severity text label (INFO, WARNING, ERROR)
severity_under_threshold <- function(severity) {
    function(check_results) {
        check_sev_ranks <- severity_rank(check_results$severity)
        input_sev_rank  <- severity_rank(severity)
        filter_condition <- check_sev_ranks >= input_sev_rank & check_success == FALSE
        violations_found <- nrow(check_results[filter_condition]) > 0
        violations_found
    }
}

#' Function that creates severity check function.
#'
#' @export
#'
#' @param severity - severity text label (INFO, WARNING, ERROR)
severity_above_threshold <- function(severity) {
    function(check_results) {
        check_sev_ranks <- severity_rank(check_results$severity)
        input_sev_rank  <- severity_rank(severity)
        filter_condition <- check_sev_ranks >= input_sev_rank & check_success == FALSE
        violations_found <- nrow(check_results[filter_condition]) > 0
        violations_found
    }
}
