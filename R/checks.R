# create new group of checks
new_group <- function() {
    list()
}

# add check into group
add_check <- function(group, new_check) {
    c(group, list(new_check))

}

# create new check
new_check <- function(severity, function_name, ...) {
    if (typeof(function_name) == "closure") {
        function_name = as.character(substitute(function_name))
    }
    list(
        severity = severity,
        function_name = function_name,
        parameters = list(...)
    )
}

# execute check
run_check <- function(data, check) {
    fun <- check$function_name
    par <- check$parameters
    par$data = data
    res <- do.call(fun, par)
    list(
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
