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
    stop_if_miss_columns(data, column)
    vals <- data[[column]]
    V <- unqiue(vals)
    distinctness <- length(V) / length(vals)
    r <- udf(distinctness)
    r
}

#' Value is contained in a list of predefined values
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param lov list of values given as vector
col_isInLOV <- function(data, column, lov) {
    stop_if_miss_columns(data, column)
    r <- data[[column]] %in% lov
    r
}

#' Value is contained in a list of predefined values
#'
#' Compatibility function that calls isInLOV
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param alloved_values list of values given as vector
col_isContainedIn <- function(data, column, alloved_values) {
    isInLOV(data, column, alloved_values)
}

#' The largest fraction of values has the same type
#'
#' Function returns TRUE automativally for all types except character and
#' factor. Latter factors are checked for consistency.
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param udf user-defined function that takes largest fraction value and
#'   returns logical value
col_hasConsistentType <- function(data, column, udf) {
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
#' @export
#' @param data dataframe
#' @param column column name
col_isNonNegative <- function(data, column) {
    stop_if_miss_columns(data, column)
    r <- data[[column]] >= 0
    r
}

#' validation whether values in the 1s column are less than in the 2nd column
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param ref_column reference column name
col_isLessThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] < data[[ref_column]]
    r
}

#' validation whether values in the 1s column are not less than in the 2nd column
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param ref_column reference column name
col_isNotLessThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] >= data[[ref_column]]
    r
}

#' validation whether values in the 1s column are greater than in the 2nd column
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param ref_column reference column name
col_isGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] > data[[ref_column]]
    r
}

#' validation whether values in the 1s column are not greater than in the 2nd
#' column
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param ref_column reference column name
col_isNotGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] <= data[[ref_column]]
    r
}

#' validation whether column has values that satisfy predicate and uder defined
#' function
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param predicate function that takes column value and returns TRUE or FALSE
col_hasValue <- function(data, column, predicate) {
    stop_if_miss_columns(data, column)
    r <- predicate(data[[column]])
    r

}

#' All values from list present in column
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param lov list of values given as vector
col_hasAllValues <- function(data, column, lov) {
    unique_vals <- unique(data[[column]])
    missing_vals <- setdiff(lov, unique_vals)
    r <- length(missing_vals) == 0
    r
}

#' Any of values from list present in column
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param lov list of values given as vector
col_hasAnyValue <- function(data, column, lov) {
    unique_vals <- unique(data[[column]])
    present_vals <- unique_vals %in% lov
    r <- sum(present_vals) > 0
    r
}

#' Data rows match the predicate
#'
#' @export
#' @param data dataframe
#' @param predicate check function that takes \code{data} as a parameter and
#'   returns TRUE if records satisy predicate of FALSE if not
col_satisfies <- function(data, predicate) {
    r <- predicate(data)
    r
}

#' validation whether all rows matching 1st predicate also match 2nd predicate
#'
#' @export
#' @param data dataframe
#' @param predicate_1 filter function that takes \code{data} as a parameter and
#'   returns TRUE / FALSE. Records marked with TRUE will be checked by second
#'   predicate
#' @param predicate_2 check function that takes \code{data} as a parameter and
#'   returns TRUE if records satisy predicate of FALSE if not
col_satisfiesIf <- function(data, predicate_1, predicate_2) {
    r1 <- predicate_1(data)
    r2 <- predicate_2(data)
    r1[r1 == FALSE] <- NA
    r <- r1 & r2
    r
}

#' user-defined validation of the predictability of a column
#'
#' @param data dataframe
#' @param column column name (response)
#' @param ref_columns columns used as predictors
#' @param udf function that takes number and returns logical value
col_hasPredictability <- function(data, column, ref_columns, udf) {
    stop_if_not_implemented("hasPredictability", not_implemented = TRUE)
}

#' custom validation of the maximum fraction of values of the same data type
#'
#' @param data dataframe
#' @param column column name
#' @param udf function that takes number and returns logical value
col_hasTypeConsistency <- function(data, column, udf) {
    stop_if_not_implemented("hasTypeConsistency", not_implemented = TRUE)
}

#' Has number of distinct non-null values
#'
#' @export
#' @param data dataframe
#' @param column column name
#' @param udf function that takes number and returns logical value
col_hasCountDistinct <- function(data, column, udf) {
    stop_if_miss_columns(data, column)
    vals <- data[[column]]
    V <- unique(vals[!is.na(vals)])
    r <- udf(V)
    r
}

#' Has minimum value
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param udf user-defined function to apply to minimum value found
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

#' Has mean value
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param udf user-defined function to apply to mean value found
col_hasMean <- function(data, column, udf) {
    mean_val <- mean(data[[column]], na.rm = T)
    udf(mean_val)
}

#' Has mean value
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param udf user-defined function to apply to standard deviation found
col_hasStandardDeviation <- function(data, column, udf) {
    stdev <- sd(data[[column]])
    r <- udf(stdev)
    r
}

#' custom validation of a particular quantile of a column
#'
#' Function is applicable only to numeric columns. It calculates value of
#' quantile of column values for a given probability. user defined function does
#' validation of quantile. Quantile is a value that corresponds to probability
#' of taking random value from sample that is less than quantile value
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param probability probability for calculation of quantile
#' @param udf user-defined function to apply to quantile found
col_hasQuantile <- function(data, column, probability, udf) {
    stop_if_miss_columns(data, column)
    stop_if_not_implemented("hasApproxQuantil", not_implemented = TRUE)
    if (!tab_hasColumnOfType(data, column, col_type$numeric)) {
        warning(paste("column", column, "has type incompatible with numeric"))
        return(FALSE)
    }
    vals <- data[[column]]
    if (probability < 0 | probability > 1) {
        stop(paste("probability has value", probability, "but it must be in range 0..1"))
    }
    qv <- quantile(vals, probability)
    r <- udf(qv)
    r
}

#' custom validation of a column’s entropy
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param udf user-defined function to apply to quantile found
col_hasEntropy <- function(data, column, udf) {
    vals <- data[[column]]
    # dataset size
    N <- length(vals)
    # unique values as vector names and their counts as vector values
    NV <- table(vals)
    func <- function(x) {
        cv <- length(NV[x])
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

#' Has histogram value
#'
#' Function to build histogram object and examine it with user-defined function
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name to check
#' @param udf user-defined function to apply to histogram value found
col_hasHistogramValue <- function(data, column, udf) {
    stop_if_miss_columns(data, column)
    vals <- data[[column]]
    hist_val <- hist(vals)
    r <- udf(hist_val)
    r
}

#' Has correlation with reference column
#'
#' @export
#'
#' @param data dataframe to check
#' @param column column name
#' @param ref_column reference column name
#' @param udf user-defined function to apply to correlation value found
col_hasCorrelation <- function(data, column, ref_column, udf) {
    stop_if_miss_columns(data, c(column, ref_column))
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
