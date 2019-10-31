#' Convert group of checks into dataframe
#'
#' @export
#'
#' @param group group of checks to be converted
check_definitions_to_data_frame <- function(group) {
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
check_results_to_data_frame <- function(check_results) {
    df <- Reduce(f = rbind, x = check_results)
    rownames(df) <- NULL
    as.data.frame(df, stringsAsFactors = FALSE)
}
