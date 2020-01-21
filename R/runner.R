#' Apply checks to dataset and return dataframe
#'
#' Apply group of checks to dataset. Function returns check results. In case
#' when \code{stop_condition} is specified function stops execution when result
#' is FALSE
#'
#' @export
#'
#' @param data dataframe to check
#' @param checks group of checks
#' @param condition function that takes check_result
#'   and returns TRUE / FALSE where FALSE halts execution.
run_checks <- function(data, checks, condition = NULL) {
    check_result <- Map(f = function(check) apply_check(data, check), checks)
    res <- if (!is.null(condition)) {
        analyze_run(check_result, condition)
    } else {
        check_result
    }
    res
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
    }
    check_result
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
    analyze_run(result, condition)
    return(data)
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
        check_sev_ranks <- severity_rank(
            check_result_info(check_results, "severity"))
        input_sev_rank  <- severity_rank(severity)
        check_success   <- check_result_info(check_results, "check_success")
        all(check_sev_ranks <= input_sev_rank | check_success == TRUE)
    }
}

#' Create severity check function.
#'
#' Function will check and return TRUE when severity is under threshold for
#' checks with \code{check_sucess} status = \code{FALSE}
#'
#' @export
#'
#' @param severity - severity text label (INFO, WARNING, ERROR)
severity_under_threshold <- function(severity) {
    function(check_results) {
        check_sev_ranks <- severity_rank(
            check_result_info(check_results, "severity"))
        input_sev_rank  <- severity_rank(severity)
        check_success   <- check_result_info(check_results, "check_success")
        all(check_sev_ranks < input_sev_rank | check_success == TRUE)
    }
}

#' Function that creates severity check function.
#'
#' Function will check and return TRUE when severity is above threshold for
#' checks with \code{check_sucess} status = \code{FALSE}
#'
#' @export
#'
#' @param severity - severity text label (INFO, WARNING, ERROR)
severity_above_threshold <- function(severity) {
    function(check_results) {
        check_sev_ranks <- severity_rank(
            check_result_info(check_results, "severity"))
        input_sev_rank  <- severity_rank(severity)
        check_success   <- check_result_info(check_results, "check_success")
        all(check_sev_ranks > input_sev_rank | check_success == TRUE)
    }
}

#' Select specified attribute from check results as return vector of its values
#'
#' @param check_result list of check application outcomes (\code{apply_check})
#' @param attribute attribute name
check_result_info <- function(check_result, attribute) {
    if (is.null(check_result)) {
        c()
    } else if (is.data.frame(check_result)) {
        check_result[[attribute]]
    } else {
        res <- sapply(
            FUN = function(x) x[[attribute]],
            X = check_result, simplify = TRUE)
        res
    }
}
