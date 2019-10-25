# apply checks to dataset and return dataframe
run_checks <- function(data, checks, stop_udf = NULL) {
    result <- Map(f = function(check) {run_check(data, check)}, checks)
    df <- Reduce(f = rbind, x = result)
    rownames(df) <- NULL
    as.data.frame(df, stringsAsFactors = FALSE)
}

# analyze checks results and stop if data quality is insufficient
analyze_check_result <- function(check_result, stop_udf = NULL) {
    if (!is.null(stop_udf) && stop_udf(check_result)) {
        stop(paste("data quality check results are not sufficient to proceed"))
    }
}

# apply all checks to dataset and if there are no errors
run_checks_and_proceed <- function(data, checks, stop_udf) {
    result <- run_checks(data, checks)
    analyze_check_result(result, stop_udf)
    return(dataset)
}
