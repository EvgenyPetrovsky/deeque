#------------------------------------------------------------------------------#
#         Library of data validation functions that are used in checks         #
#------------------------------------------------------------------------------#

# Metadata checks

#' Function that calculates number of rows in dataframe and applies UDF (user 
#' defined function) to it
#'
#' @export
#' @param data dataframe
#' @param udf user-defined funcion that takes number and returns logical value
hasRowCount <- function(data, udf) {
  n <- nrow(data)
  r <- udf(n)
  r
}

#' Function that calculates number of columns in dataframe and applies UDF (user 
#' defined function) to it
#'
#' @export
#' @param data dataframe
#' @param udf user-defined funcion that takes number and returns logical value
hasColumnCount <- function(data, udf) {
  n <- ncol(data)
  r <- udf(n)
  r
}

#'
#'
hasColumn <- function(data, column) {
  r <- column[1] %in% colnames(data)
  r
}

#'
#'
hasColumns <- function(data, columns) {
  result = (columns %in% colnames(data))
  r <- min(result)
  r
}

#'
#'
hasColumnOfType <- function(
  data, column,
  type = c("Date", "numeric", "integer", "logical", "character", "factor")
) {
  type <- match.arg(type)
  stop_if_miss_columns(data, column)
  r <- with(list(col = data[[column]]), {
    if (type == "Date" & is.numeric.Date(col)) {TRUE}
    else if (type == "numeric" & is.numeric(col)) {TRUE}
    else if (type == "integer" & is.integer(col)) {TRUE}
    else if (type == "logical" & is.logical(col)) {TRUE}
    else if (type == "character" & is.character(col)) {TRUE}
    else if (type == "factor" & is.factor(col)) {TRUE}
    else {FALSE}
  })
  r
}

#'
#'
hasUniqueKey <- function(data, columns){
  col_diff <- setdiff(columns, colnames(data))
  col_text <- paste(paste("'", col_diff, "'", sep = ""), collapse = ", ")
  if (length() > 0) {
    stop(paste("columns", col_text, "are not present in data frame"))
  }
  n0 <- nrow(data)
  n1 <- nrow(subset(data, select = columns))
  r <- if (n0 == n1) {TRUE} else {FALSE}
  r
}

#------------------------------------------------------------------------------#
# Value checks

#' check that there are no missing values in a column
#'
#'
isComplete <- function(data, column) {
  stop_if_miss_columns(data, column)
  !is.na(data[[column]])
}

#' custom validation of the fraction of missing values in a column
#'
#'
hasCompleteness <- function(data, column, udf) {
    stop_if_miss_columns(data, column)
    stat <- mean(!is.na(data[[column]]))
    r <- udf(stat)
    r
}

#' check that there are no duplicates in a column
#'
#'
isUnique <- function(data, column) {
    stop_if_miss_columns()
    # index of 1st duplicated element (0 - no duplicates)
    idx <- anyDuplicated(data[[column]])
    r <- idx == 0
    r
}

#' custom validation of the unique value ratio in a column
#'
#'
hasUniqueness <- function(data, column, udf) {
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


#'
#'
hasDistinctness <- function(data, column, udf) {
    # Distinctness: |V| / N
    vals <- data[[column]]
    V <- unqiue(vals)
    distinctness <- length(V) / length(vals)
    r <- udf(distinctness)
    r
}

#' validation of the fraction of values that are in a valid range
#'
#'
isInLOV <- function(data, column, lov) {
    stop_if_miss_columns(data, column)
    r <- data[[column]] %in% lov
    r
}'
#'
#'
isInRange <- function(data, column, range) {isInLOV(data, column, range)}

#' validation of the largest fraction of values that have the same type
#'
#'
hasConsistentType <- function(data, column) {
    stop_if_not_implemented("hasConsistentType", not_implemented = TRUE)
}

#' validation whether all values in a numeric column are non-negative
#'
#'
isNonNegative <- function(data, column) {
    stop_if_miss_columns(data, column)
    r <- data[[column]] >= 0
    r
}

#' validation whether values in the 1s column are less than in the 2nd column
#'
#'
isLessThan <- function(data, column, ref_column, value) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] < data[[ref_column]]
    r
}

#' validation whether values in the 1s column are not less than in the 2nd column
#'
#'
isNotLessThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] >= data[[ref_column]]
    r
}

#' validation whether values in the 1s column are greater than in the 2nd column
#'
#'
isGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] > data[[ref_column]]
    r
}

#' validation whether values in the 1s column are not greater than in the 2nd column
#'
#'
isNotGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    r <- data[[column]] <= data[[ref_column]]
    r
}

#' validation whether column has values that satisfy predicate and uder defined function
#'
#'
hasValue <- function(data, column, predicate) {
    stop_if_miss_columns(data, column)
    r <- predicate(data[[column]])
    r

}

#' validation whether all values from list present in column
#'
#'
hasAllValues <- function(data, column, lov) {
    unique_vals <- unique(data[[column]])
    missing_vals <- setdiff(lov, unique_vals)
    r <- length(missing_vals) == 0
    r
}

#' validation whether all values from list present in column
#'
#'
hasAnyValue <- function(data, column, lov) {
    unique_vals <- unique(data[[column]])
    present_vals <- unique_vals %in% lov
    r <- sum(present_vals) > 0
    r
}

#' validation whether all rows matching 1st predicate also match 2nd predicate
#'
#'
satisfies <- function(data, predicate) {
    r <- predicate(data)
    r
}

#' validation whether all rows matching 1st predicate also match 2nd predicate
#'
#'
satisfiesIf <- function(data, predicate_1, predicate_2) {
    r1 <- predicate_1(data)
    r2 <- predicate_2(data)
    r1[r1 == FALSE] <- NA
    r <- r1 & r2
    r
}

#' user-defined validation of the predictability of a column
#'
#'
hasPredictability <- function( data, column, ref_columns, udf) {
    stop_if_not_implemented("hasPredictability", not_implemented = TRUE)
}

# statistics (can be used to verify dimension consistency)

#' custom validation of the number of records
#'
#'
hasSize <- function(data, udf) {
    size <- nrow(data)
    r <- udf(size)
    r
}

#' custom validation of the maximum fraction of values of the same data type
#'
#'
hasTypeConsistency <- function(data, column, udf) {
    stop_if_not_implemented("hasTypeConsistency", not_implemented = TRUE)
}

#' custom validation of the number of distinct non-null values in a column
#'
#'
hasCountDistinct <- function(data, column, udf) {
    stop_if_miss_columns(data, column)
    vals <- data[[column]]
    V <- unique(vals[!is.na(vals)])
    r <- udf(V)
    r
}'
#'
#'
hasApproxCountDistinct <- function(data, column, udf) {
    stop_if_not_implemented("hasApproxCountDistinct", not_implemented = TRUE)
}

#' custom validation of a column’s minimum value
#'
#'
hasMin <- function(data, column, udf) {
    min_val <- min(data[[column]], na.rm = T)
    udf(min_val)
}

#' custom validation of a column’s maximum value
#'
#'
hasMax <- function(data, column, udf) {
    max_val <- max(data[[column]], na.rm = T)
    udf(max_val)
}

#' custom validation of a column’s mean value
#'
#'
hasMean <- function(data, column, udf) {
    mean_val <- mean(data[[column]], na.rm = T)
    udf(mean_val)
}

#' custom validation of a column’s standard deviation
#'
#'
hasStandardDeviation <- function(data, column, udf) {
    stdev <- sd(data[[column]])
    r <- udf(stdev)
    r
}

#' custom validation of a particular quantile of a column (approx.)
#'
#'
hasApproxQuantile <- function(data, column, quantile, udf ) {
    stop_if_not_implemented("hasApproxQuantil", not_implemented = TRUE)
}

#' custom validation of a column’s entropy
#'
#'
hasEntropy <- function(data, column, udf) {
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
hasMutualInformation <- function(data, column, ref_column, udf ) {
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
hasHistogramValue <- function(data, column, udf) {
    stop_if_not_implemented("hasHistogramValue", not_implemented = TRUE)
}

# custom validation of a column pair’s correlation
hasCorrelation <- function(data, column, ref_column, udf) {
    vals <- data[[column]]
    ref_vals <- data[[ref_column]]
    c <- corr(vals, ref_vals)
    r <- udf(c)
    r
}

# time
hasNoAnomalies <- function(data, column, metric, detector) {
    stop_if_not_implemented("hasNoAnomalies", not_implemented = TRUE)
}
