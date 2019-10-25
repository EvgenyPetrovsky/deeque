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
        valid_ratio = ratio(res)
    )
}

checks_to_data_frame <- function(group) {
    group %>%
        Reduce(f = rbind, init = list()) %>%
        as.data.frame(stringsAsFactors = FALSE)
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
