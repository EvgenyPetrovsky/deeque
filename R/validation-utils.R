#------------------------------------------------------------------------------#

#' Raise error in case when dataframe doesn't contaain columns
#'
#' @param data dataframe
#' @param columns vector of column names
stop_if_miss_columns <- function(data, columns) {
    if (length(columns) == 0) {
        stop("No columns specified")
    }
    data_columns <- colnames(data)
    miss_columns <- setdiff(columns, data_columns)
    if (length(miss_columns) > 0) {
        columns_text <- paste(paste("'", miss_columns, "'", sep = ""), collapse = ",")
        message_text <- if (length(miss_columns) > 1) {
            paste("Columns", columns_text, "are missing in data frame")
        } else {
            paste("Column", columns_text, "is missing in data frame")
        }
        stop(message_text)
    }
    return(NULL)
}

#' Function that just stops execution and generates error message
#'
#' @param name - name of the function which is not implemented
#' @param not_implemented - logical value TRUE / FALSE
stop_if_not_implemented <- function(name, not_implemented) {
    if (not_implemented) {
        stop(paste("Function", name, "is not yet implemented"))
    }
}

#------------------------------------------------------------------------------#

#' Ratio of success based on vector of logical values
#' 
#' @param - vector of logical values 
ratio <- function(logical_vector) {
    if (!is.logical(logical_vector)) {
        stop("Values must be logical (TRUE or FALSE)")
    }
    mean(logical_vector)
}
