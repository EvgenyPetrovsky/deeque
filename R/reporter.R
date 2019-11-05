#' Convert group of checks into dataframe
#'
#' @export
#'
#' @param group group of checks to be converted
convert_checks_to_df <- function(group) {
    group %>%
        Reduce(f = rbind, init = list()) %>%
        as.data.frame(stringsAsFactors = FALSE)
}

#' Convert check results into data frame
#'
#' @export
#'
#' @param check_results list of check results produced by \code{run_checks}
#' function
convert_run_results_to_df <- function(check_results) {
    df <- Reduce(f = rbind, x = check_results)
    rownames(df) <- NULL
    as.data.frame(df, stringsAsFactors = FALSE)
}
