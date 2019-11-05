#------------------------------------------------------------------------------#
#         Library of data validation functions that are used in checks         #
#------------------------------------------------------------------------------#

# Value checks

#' No missing values in a column
#'
#' @export
#' @param data dataframe
#' @param column column name
col_isComplete <- function(data, column) {
  stop_if_miss_columns(data, column)
  !is.na(data[[column]])
}

#' Assess ratio of missing value with user-defined function
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param udf user-defined function that takes ratio and returns logical value
col_hasCompleteness <- function(data, column, udf) {
    stop_if_miss_columns(data, column)
    stat <- mean(!is.na(data[[column]]))
    r <- udf(stat)
    r
}

#' No duplicates in a column
#'
#' @export
#' @param data dataframe
#' @param column column name
col_isUnique <- function(data, column) {
    stop_if_miss_columns()
    # index of 1st duplicated element (0 - no duplicates)
    idx <- anyDuplicated(data[[column]])
    r <- idx == 0
    r
}

#' Assess uniqueness of column values with user-defined function
#'
#' Uniqueness is defined by formula |{v ∈ V | cv = 1}| / |V| where V is total set of values that column takes, v - exact value, cv - values count in column
#' In other words - number of values that occur only 1 time divided by total number of possible values. For example uniqueness of [1, 2, 2, 3] is 2/3
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param udf user-defined function that takes ratio and returns logical value
col_hasUniqueness <- function(data, column, udf) {
    # Uniqueness: |{v ∈ V | cv = 1}| / |V|
    vals <- data[[column]]
    dup_ids <- duplicated(vals)
    V <- unique(vals)
    d <- unique(vals[dup_ids])
    v <- V[!V %in% d]
    uniqueness <- length(v) / length(V)
    r <- udf(uniqueness)
    r
}


#' Assess distinctness of column values with user-defined function
#'
#' Distinctness is defined by formula |V| / N where V is is total set of values that column takes, N is number of values in column.
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param udf user-defined function that takes ratio and returns logical value
col_hasDistinctness <- function(data, column, udf) {
    # Distinctness: |V| / N
    vals <- data[[column]]
    V <- unqiue(vals)
    distinctness <- length(V) / length(vals)
    r <- udf(distinctness)
    r
}

#' Value belongs to list of values
#' @export
#' @param data dataframe
#' @param column column name
#' @param lov list of values given as vector
col_isInLOV <- function(data, column, lov) {
    stop_if_miss_columns(data, column)
    r <- data[[column]] %in% lov
    r
}

#' Value belongs to list of values
#'
#' Compatibility function that calls isInLOV
#' 
#' @export
#' @param data dataframe
#' @param column column name
#' @param lov list of values given as vector
col_isInRange <- function(data, column, range) {isInLOV(data, column, range)}

#' The largest fraction of values has the same type
#'
#'
col_hasConsistentType <- function(data, column) {
    stop_if_not_implemented("hasConsistentType", not_implemented = TRUE)
    vals <- data[[column]]
    check_vals <- if (is.factor(vals)) {
        levels(vals)
    } else if (
        is.numeric.Date(vals) | is.numeric(vals) | 
        is.integer(vals) | is.logical(vals)
        ) {
        character()
    } else if (is.character(vals)) {
        character()
    } 
}

#' validation whether all values in a numeric column are non-negative
#'
#'
col_isNonNegative <- function(data, column) {
    stop_if_miss_columns(data, column)
    r <- data[[column]] >= 0
    r
}

#' validation whether values in the 1s column are less than in the 2nd column
#'
#'
col_isLessThan <- function(data, column, ref_column, value) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] < data[[ref_column]]
    r
}

#' validation whether values in the 1s column are not less than in the 2nd column
#'
#'
col_isNotLessThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] >= data[[ref_column]]
    r
}

#' validation whether values in the 1s column are greater than in the 2nd column
#'
#'
col_isGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] > data[[ref_column]]
    r
}

#' validation whether values in the 1s column are not greater than in the 2nd column
#'
#'
col_isNotGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] <= data[[ref_column]]
    r
}

#' validation whether column has values that satisfy predicate and uder defined function
#'
#'
col_hasValue <- function(data, column, predicate) {
    stop_if_miss_columns(data, column)
    r <- predicate(data[[column]])
    r

}

#' validation whether all values from list present in column
#'
#'
col_hasAllValues <- function(data, column, lov) {
    unique_vals <- unique(data[[column]])
    missing_vals <- setdiff(lov, unique_vals)
    r <- length(missing_vals) == 0
    r
}

#' validation whether all values from list present in column
#'
#'
col_hasAnyValue <- function(data, column, lov) {
    unique_vals <- unique(data[[column]])
    present_vals <- unique_vals %in% lov
    r <- sum(present_vals) > 0
    r
}

#' validation whether all rows matching 1st predicate also match 2nd predicate
#'
#'
col_satisfies <- function(data, predicate) {
    r <- predicate(data)
    r
}

#' validation whether all rows matching 1st predicate also match 2nd predicate
#'
#'
col_satisfiesIf <- function(data, predicate_1, predicate_2) {
    r1 <- predicate_1(data)
    r2 <- predicate_2(data)
    r1[r1 == FALSE] <- NA
    r <- r1 & r2
    r
}

#' user-defined validation of the predictability of a column
#'
#'
col_hasPredictability <- function(data, column, ref_columns, udf) {
    stop_if_not_implemented("hasPredictability", not_implemented = TRUE)
}

#' custom validation of the maximum fraction of values of the same data type
#'
#'
col_hasTypeConsistency <- function(data, column, udf) {
    stop_if_not_implemented("hasTypeConsistency", not_implemented = TRUE)
}

#' custom validation of the number of distinct non-null values in a column
#'
#'
col_hasCountDistinct <- function(data, column, udf) {
    stop_if_miss_columns(data, column)
    vals <- data[[column]]
    V <- unique(vals[!is.na(vals)])
    r <- udf(V)
    r
}

#'
#'
col_hasApproxCountDistinct <- function(data, column, udf) {
    stop_if_not_implemented("hasApproxCountDistinct", not_implemented = TRUE)
}

#' custom validation of a column’s minimum value
#'
#'
col_hasMin <- function(data, column, udf) {
    min_val <- min(data[[column]], na.rm = T)
    udf(min_val)
}

#' custom validation of a column’s maximum value
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param udf user-defined function to apply to maximum value found
col_hasMax <- function(data, column, udf) {
    max_val <- max(data[[column]], na.rm = T)
    udf(max_val)
}

#' custom validation of a column’s mean value
#'
#'
col_hasMean <- function(data, column, udf) {
    mean_val <- mean(data[[column]], na.rm = T)
    udf(mean_val)
}

#' custom validation of a column’s standard deviation
#'
#'
col_hasStandardDeviation <- function(data, column, udf) {
    stdev <- sd(data[[column]])
    r <- udf(stdev)
    r
}

#' custom validation of a particular quantile of a column (approx.)
#'
#'
col_hasApproxQuantile <- function(data, column, quantile, udf ) {
    stop_if_not_implemented("hasApproxQuantil", not_implemented = TRUE)
}

#' custom validation of a column’s entropy
#'
#'
col_hasEntropy <- function(data, column, udf) {
    vals <- data[[column]]
    # dataset size
    N <- length(vals)
    # unique values as vector names and their counts as vector values
    NV <- table(vals)
    func <- function(x) {
        cv <- length(NV[as.character()])
        size <- N
        return(cv/size * log(cv/size))
    }
    entropy <- sum(sapply(FUN = func, X = names(NV)))
    r <- udf(entropy)
    r
}

#
col_hasMutualInformation <- function(data, column, ref_column, udf ) {
    vals1 <- data[[column]]
    vals2 <- data[[ref_column]]
    # dataset size
    N <- length(vals1)
    # unique values as vector names and their counts as vector values
    NV1 <- table(vals1)
    NV2 <- table(vals2)
    pairs <- list()
    #func <- function(x){
    #    cv <- length(NV[as.character()])
    #    size <- N
    #    return(cv/size * log(cv/size))
    #}


    r <- udf(mutual_info)
}

# custom validation of a column pair’s mutual information
col_hasHistogramValue <- function(data, column, udf) {
    stop_if_not_implemented("hasHistogramValue", not_implemented = TRUE)
}

# custom validation of a column pair’s correlation
col_hasCorrelation <- function(data, column, ref_column, udf) {
    vals <- data[[column]]
    ref_vals <- data[[ref_column]]
    c <- corr(vals, ref_vals)
    r <- udf(c)
    r
}

# time
col_hasNoAnomalies <- function(data, column, metric, detector) {
    stop_if_not_implemented("hasNoAnomalies", not_implemented = TRUE)
}
