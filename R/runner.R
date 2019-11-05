#' Apply checks to dataset and return dataframe
#' 
#' Apply group of checks to dataset. Function returns check results. In case when 
#' \code{stop_condition} is specified function stops execution when 
#'
#' @param data dataframe to check
#' @param checks group of checks created by new_group() %>% add_check() ...
#' @param condition function that takes check_result 
#'   and returns TRUE / FALSE where FALSE halts execution.
run_checks <- function(data, checks, condition = NULL) {
    check_result <- Map(f = function(check) {apply_check(data, check)}, checks)
    if (!is.null(condition)) {
        analyze_run(check_result, condition)
    } else {
        check_result
    }
}

#' Analyze checks results and stop if data quality is insufficient
#'
#' Data quality reported in check resutls must be sufficient otherwise function
#' will raise error and stop program execution. Validation condition is given as
#' anonymous function into \code{condition}
#'
#' @export
#'
#' @param check_result list of check application outcomes (\code{apply_check})
#' @param condition function that takes check_result and returns TRUE / FALSE
#'   where TRUE means success and proceeds execution and FALSE halts execution
analyze_run <- function(check_result, condition) {
    if (!is.null(condition) & condition(check_result) == FALSE) {
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
run_checks_and_proceed <- function(data, checks, condition) {
    result <- run_checks(data, checks)
    analyze_check_result(result, condition)
    return(data)
}
