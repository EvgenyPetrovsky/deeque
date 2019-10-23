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
    list(
        severity = severity,
        function_name = function_name,
        parameters = list(...)
    )
}

# execute check
run_check <- function(data, check) {
    fun_call <- call(c(check$function_name, check$parameters))
    res <- eval(fun_call)
    list(
        severity = check$severity,
        function_name = check$function_name,
        check_column = check$parameters$column_name,
        scope_size = length(res),
        score = ratio(res)
    )
}
