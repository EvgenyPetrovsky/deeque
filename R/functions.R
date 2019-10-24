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

# completeness
isComplete <- function(data, column) {
  stop_if_miss_columns(data, column)
  !is.na(data[[column]])
}

hasCompleteness <- function(data, column, udf) {
  udf(isComplete(data, column))
}

# consistency
isUnique <- function(data, column) {
    stop_if_miss_columns()
    # index of 1st duplicated element (0 - no duplicates)
    idx <- anyDuplicated(data[[column]])
    idx == 0
}
hasUniqueness <- function(data, column, udf) {
    stop_if_not_implemented("hasUniqueness", not_implemented = TRUE)
}
hasDistinctness <- function(data, column, udf) {
    stop_if_not_implemented("hasDistinctness", not_implemented = TRUE)
}
isInLOV <- function(data, column, lov) {
    stop_if_miss_columns(data, column)
    data[[column]] %in% lov
}
hasConsistentType <- function(data, column) {
    stop("function hasConsistentType is not yet implemented")
}
isNonNegative <- function(data, column) {
    stop("function isNonNegative is not yet implemented")
}
isLessThan <- function(data, column, ref_column) {
    stop_if_miss_columns(data, c(column, ref_column))
    column <- ref_column
    stop("function isLessThan is not yet implemented")
}
satisfies <- function(data, predicate) {
    stop("function satisfies is not yet implemented")
}

# validation whether all rows matching 1st predicate also match 2nd predicate
satisfiesIf <- function(data, predicate_1, predicate_2) {
    stop("function satisfiesIf is not yet implemented")
}

# user-defined validation of the predictability of a column
hasPredictability <- function( data, column, ref_columns, udf) {
    stop("function hasPredictability is not yet implemented")
}

# statistics (can be used to verify dimension consistency)

# custom validation of the number of records
hasSize <- function(date, udf) {
    stop("fucntion hasSize is not yet supported")
}

# custom validation of the maximum fraction of values of the same data type
hasTypeConsistency <- function(data, column, udf) {
    stop("fucntion hasTypeConsistency is not yet supported")
}

# custom validation of the number of distinct non-null values in a column
hasCountDistinct <- function(data, column) {

    stop("fucntion hasCountDistinct is not yet supported")
}
hasApproxCountDistinct <- function(data, column, udf) {
    stop("fucntion hasApproxCountDistinct is not yet supported")
}
hasMin <- function(data, column, udf) {
    min_val <- min(data[[column]], na.rm = T)
    udf(min_val)
}
hasMax <- function(data, column, udf) {
    max_val <- max(data[[column]], na.rm = T)
    udf(max_val)
}
hasMean <- function(data, column, udf) {
    mean_val <- mean(data[[column]], na.rm = T)
    udf(mean_val)
}
hasStandardDeviation <- function(data, column, udf) {
    stop("fucntion hasStandardDeviation is not yet supported")
}
hasApproxQuantile <- function(data, column, quantile, udf ) {
    stop("fucntion hasApproxQuantil is not yet supported")
}
hasEntropy <- function(data, column, udf) {
    stop("fucntion hasEntropy is not yet supported")
}
hasMutualInformation <- function(data, column, ref_column, udf ) {
    stop("fucntion hasMutualInformation is not yet supported")
}
hasHistogramValue <- function(data, column, udf) {
    stop("fucntion hasHistogramValue is not yet supported")
}
hasCorrelation <- function(data, column, ref_column, udf) {
    stop("fucntion hasCorrelation is not yet supported")
}

# time
hasNoAnomalies <- function(data, column, metric, detector) {
    stop("fucntion hasNoAnomalies is not yet supported")
}
