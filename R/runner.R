# apply checks to dataset and return dataframe
run_checks <- function(data, checks) {
    result <- Map(f = function(check) {run_check(data, check)}, checks)
}

#' Analyze checks results and stop if data quality is insufficient
#'
#' Data quality reported in check resutls must be sufficient otherwise function
#' will raise error and stop program execution. Validation condition is given as
#' anonymous function into \code{condition}
#'
#' @export
#'
#' @param check_result check results that need to be analyzed
#' @param condition function that takes check_result and returns TRUE / FALSE
#'   where TRUE means success and proceeds execution and FALSE halts execution.
analyze_run <- function(check_result, condition) {
    if (!is.null(stop_udf) && stop_udf(check_result)) {
        stop(paste("data quality check results are not sufficient to proceed"))
    } else {
        check_result
    }
}

#' Apply all checks to dataset and if there are no errors
#'
#' This convinience function combines two: \code{run_checks} and
#' \code{analyze_run}. Check resuts are given to analyze function. If analyze
#' finds violations that exceed allowed threshold then process stops. Otherwise
#' function will return initial dataset and processing can be continued.
#'
#' @export
#'
#' @param data dataset that needs to be checked
#' @param checks group of checks that needs to be applied to dataset
#' @param condition user defined function that needs to take check results and
#'   return either TRUE or FALSE
run_checks_and_proceed <- function(data, checks, condition = NULL) {
    result <- run_checks(data, checks)
    analyze_check_result(result, stop_udf)
    return(data)
}
