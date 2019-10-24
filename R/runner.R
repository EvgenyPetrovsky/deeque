run_checks <- function(dataset, checks) {
    Map(f = function(check) {run_check(dataset, check)}, checks)
}

# apply all checks to dataset and if there are no errors
check_and_proceed <- function(dataset, checks) {
    return(dataset)
}
