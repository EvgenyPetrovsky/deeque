#------------------------------------------------------------------------------#
#         Library of data validation functions that are used in checks         #
#------------------------------------------------------------------------------#
# Service functions
stop_if_miss_columns <- function(data, columns) {
    if (length(columns) == 0) {
        stop("no columns specified")
    }
    data_columns <- colnames(data)
    miss_columns <- setdiff(columns, data_columns)
    if (length(miss_columns) > 0) {
        columns_text <- paste(paste("'", miss_columns, "'", sep = ""), collapse = ",")
        message_text <- if (length(miss_columns) > 1) {
            paste("columns", columns_text, "are missing in data frame")
        } else {
            paste("column", columns_text, "is missing in data frame")
        }
        stop(message_text)
    }
    return(NULL)
}

stop_if_not_implemented <- function(name, not_implemented) {
    if (not_implemented) {
        stop(paste("function", name, "is not yet implemented"))
    }
}

ratio <- function(logical_vector) {
    mean(logical_vector)
}
#------------------------------------------------------------------------------#
# Metadata checks

hasRowCount <- function(data, udf) {
  n <- nrow(data)
  udf(n)
}

hasColumnCount <- function(data, udf) {
  n <- ncol(data)
  udf(n)
}

hasColumn <- function(data, column) {
  column[1] %in% colnames(data)
}

hasColumns <- function(data, columns) {
  result = (columns %in% colnames(data))
  min(result)
}

hasColumnOfType <- function(
  data, column,
  type = c("Date", "numeric", "integer", "logical", "character", "factor")
) {
  type <- match.arg(type)
  stop_if_miss_columns(data, column)
  with(list(col = data[["column"]]), {
    if (type == "Date" && is.numeric.Date(col)) {TRUE}
    else if (type == "numeric" && is.numeric(col)) {TRUE}
    else if (type == "integer" && is.integer(col)) {TRUE}
    else if (type == "logical" && is.logical(col)) {TRUE}
    else if (type == "character" && is.character(col)) {TRUE}
    else if (type == "factor" && is.factor(col)) {TRUE}
    else {FALSE}
  })
}

hasUniqueKey <- function(data, columns){
  col_diff <- setdiff(columns, colnames(data))
  col_text <- paste(paste("'", col_diff, "'", sep = ""), collapse = ", ")
  if (length() > 0) {
    stop(paste("columns", col_text, "are not present in data frame"))
  }
  n0 <- nrow(data)
  n1 <- nrow(subset(data, select = columns))
  if (n0 == n1) {TRUE} else {FALSE}
}

#------------------------------------------------------------------------------#
# Value checks

# check that there are no missing values in a column
isComplete <- function(data, column) {
  stop_if_miss_columns(data, column)
  !is.na(data[[column]])
}

# custom validation of the fraction of missing values in a column
hasCompleteness <- function(data, column, udf) {
  udf(isComplete(data, column))
}

# check that there are no duplicates in a column
isUnique <- function(data, column) {
    stop_if_miss_columns()
    # index of 1st duplicated element (0 - no duplicates)
    idx <- anyDuplicated(data[[column]])
    idx == 0
}

# custom validation of the unique value ratio in a column
hasUniqueness <- function(data, column, udf) {
    ds <- duplicated(data[[column]])
    uniqueness <- mean(ds == FALSE)
    udf(uniqueness)
    #stop_if_not_implemented("hasUniqueness", not_implemented = TRUE)
}
hasDistinctness <- function(data, column, udf) {
    stop_if_not_implemented("hasDistinctness", not_implemented = TRUE)
}

# validation of the fraction of values that are in a valid range
isInLOV <- function(data, column, lov) {
    stop_if_miss_columns(data, column)
    data[[column]] %in% lov
}
isInRange <- function(data, column, range) {isInLOV(data, column, range)}

# validation of the largest fraction of values that have the same type
hasConsistentType <- function(data, column) {
    stop_if_not_implemented("hasConsistentType", not_implemented = TRUE)
}

# validation whether all values in a numeric column are non-negative
isNonNegative <- function(data, column) {
    stop_if_miss_columns(data, column)
    data[[column]] >= 0
}

# validation whether values in the 1s column are less than in the 2nd column
isLessThan <- function(data, column, ref_column, value) {
    stop_if_miss_columns(data, c(column, ref_column))
    data[[column]] < data[[ref_column]]
}

# validation whether values in the 1s column are not less than in the 2nd column
isNotLessThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    data[[column]] >= data[[ref_column]]
}

# validation whether values in the 1s column are greater than in the 2nd column
isGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    data[[column]] > data[[ref_column]]
}

# validation whether values in the 1s column are not greater than in the 2nd column
isNotGreaterThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    data[[column]] <= data[[ref_column]]
}

# validation whether column has values that satisfy predicate and uder defined function
hasValue <- function(data, column, predicate, udf = NULL) {
    stop_if_miss_columns(data, column)
    val <- predicate(data[[column]])
    if (!is.null(udf)) {
        udf(ratio(val))
    } else {
        ratio(val)
    }
}

# validation whether all rows matching 1st predicate also match 2nd predicate
satisfies <- function(data, predicate, udf = NULL) {
    stop_if_not_implemented("satisfies", not_implemented = TRUE)
    
}

# validation whether all rows matching 1st predicate also match 2nd predicate
satisfiesIf <- function(data, predicate_1, predicate_2) {
    stop_if_not_implemented("satisfiesIf", not_implemented = TRUE)
}

# user-defined validation of the predictability of a column
hasPredictability <- function( data, column, ref_columns, udf) {
    stop_if_not_implemented("hasPredictability", not_implemented = TRUE)
}

# statistics (can be used to verify dimension consistency)

# custom validation of the number of records
hasSize <- function(date, udf) {
    stop_if_not_implemented("hasSize", not_implemented = TRUE)
}

# custom validation of the maximum fraction of values of the same data type
hasTypeConsistency <- function(data, column, udf) {
    stop_if_not_implemented("hasTypeConsistency", not_implemented = TRUE)
}

# custom validation of the number of distinct non-null values in a column
hasCountDistinct <- function(data, column) {

    stop_if_not_implemented("hasCountDistinct", not_implemented = TRUE)
}
hasApproxCountDistinct <- function(data, column, udf) {
    stop_if_not_implemented("hasApproxCountDistinct", not_implemented = TRUE)
}

# custom validation of a column’s minimum value
hasMin <- function(data, column, udf) {
    min_val <- min(data[[column]], na.rm = T)
    udf(min_val)
}

# custom validation of a column’s maximum value
hasMax <- function(data, column, udf) {
    max_val <- max(data[[column]], na.rm = T)
    udf(max_val)
}

# custom validation of a column’s mean value
hasMean <- function(data, column, udf) {
    mean_val <- mean(data[[column]], na.rm = T)
    udf(mean_val)
}
hasStandardDeviation <- function(data, column, udf) {
    stop_if_not_implemented("hasStandardDeviation", not_implemented = TRUE)
}
hasApproxQuantile <- function(data, column, quantile, udf ) {
    stop_if_not_implemented("hasApproxQuantil", not_implemented = TRUE)
}
hasEntropy <- function(data, column, udf) {
    stop_if_not_implemented("hasEntropy", not_implemented = TRUE)
}
hasMutualInformation <- function(data, column, ref_column, udf ) {
    stop_if_not_implemented("hasMutualInformation", not_implemented = TRUE)
}
hasHistogramValue <- function(data, column, udf) {
    stop_if_not_implemented("hasHistogramValue", not_implemented = TRUE)
}
hasCorrelation <- function(data, column, ref_column, udf) {
    stop_if_not_implemented("hasCorrelation", not_implemented = TRUE)
}

# time
hasNoAnomalies <- function(data, column, metric, detector) {
    stop_if_not_implemented("hasNoAnomalies", not_implemented = TRUE)
}
