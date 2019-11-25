df <- data.frame

# col_isComplete
test_that("col_isComplete returns true only when all values are non-NA", {
  expect_equal(col_isComplete(df(a = NA), "a"), FALSE)
  expect_equal(col_isComplete(df(a = 1), "a"), TRUE)
  expect_equal(col_isComplete(df(a = c(1, NA)), "a"), c(TRUE, FALSE))
  expect_equal(col_isComplete(df(a = numeric()), "a"), logical())
})

# col_hasCompleteness
test_that("col_hasCompleteness returns proper result", {
  data <- df(a = 1:2, b = c(1, NA), c = c(NA, NA))
  expect_equal(col_hasCompleteness(data, "a", udf_ge(1.00)), TRUE)
  expect_equal(col_hasCompleteness(data, "b", udf_ge(0.66)), FALSE)
  expect_equal(col_hasCompleteness(data, "b", udf_le(0.66)), TRUE)
  expect_equal(col_hasCompleteness(data, "b", udf_ge(0.50)), TRUE)
  expect_equal(col_hasCompleteness(data, "c", udf_ge(0.50)), FALSE)
  expect_equal(col_hasCompleteness(data, "c", udf_ge(0.00)), TRUE)
})

# col_isUnique
test_that("col_isUnique returns proper result", {
  data <- df(a = 1:3, b = c(1, 2, NA), c = c(1, NA, NA), d = c(1, 1, 3))
  expect_equal(col_isUnique(data, "a"), TRUE)
  expect_equal(col_isUnique(data, "b"), TRUE)
  expect_equal(col_isUnique(data, "c"), FALSE)
  expect_equal(col_isUnique(data, "d"), FALSE)
})

# col_hasUniqueness
test_that("col_hasUniqueness returns proper result", {
  data <- df(a = 1:3, b = c(1, 2, NA), c = c(1, NA, NA), d = c(1, 1, 3))
  expect_equal(col_hasUniqueness(data, "a", udf_between(.9, 1)), TRUE)
  expect_equal(col_hasUniqueness(data, "b", udf_between(.9, 1)), TRUE)
  expect_equal(col_hasUniqueness(data, "c", udf_between(.9, 1)), FALSE)
  expect_equal(col_hasUniqueness(data, "d", function(x) x == .5), TRUE)
  expect_equal(col_hasUniqueness(data, "d", udf_between(.9, 1)), FALSE)
})

# col_hasDistinctness
test_that("col_hasDistinctness returns proper result", {
  data <- df(a = 1:3, b = c(1, 2, NA), c = c(1, NA, NA), d = c(1, 1, 3))
  expect_equal(col_hasDistinctness(data, "a", udf_eq(1)), TRUE)
  expect_equal(col_hasDistinctness(data, "b", udf_eq(1)), TRUE)
  expect_equal(col_hasDistinctness(data, "c", udf_eq(1)), FALSE)
  expect_equal(col_hasDistinctness(data, "d", udf_eq(2/3)), TRUE)
  expect_equal(col_hasDistinctness(data, "d", udf_eq(0.6666)), FALSE)
  expect_equal(col_hasDistinctness(data, "d", udf_between(.9, 1)), FALSE)
})

# col_isInLOV
test_that("col_isInLOV returns proper result", {
  data <- df(a = 1:3, b = c(1, 2, NA), c = c(1, NA, NA), d = c(1, 1, 3))
  expect_equal(col_isInLOV(data, "a", c(1, 3)), c(T,F,T))
  expect_equal(col_isInLOV(data, "a", unique(data$a)), c(T,T,T))
  expect_equal(col_isInLOV(data, "a", 1:10), c(T,T,T))
  expect_equal(col_isInLOV(data, "b", unique(data$b)), c(T,T,T))
})

# col_isContainedIn
#test_that("", {
#  expect_equal(,)
#})

# col_hasConsistentType
#test_that("", {
#  expect_equal(,)
#})

# col_isNonNegative
#test_that("", {
#  expect_equal(,)
#})

# col_isLessThan
#test_that("", {
#  expect_equal(,)
#})

# col_isNotLessThan
#test_that("", {
#  expect_equal(,)
#})

# col_isGreaterThan
#test_that("", {
#  expect_equal(,)
#})

# col_isNotGreaterThan
#test_that("", {
#  expect_equal(,)
#})

# col_hasValue
#test_that("", {
#  expect_equal(,)
#})

# col_hasAllValues
#test_that("", {
#  expect_equal(,)
#})

# col_hasAnyValue
#test_that("", {
#  expect_equal(,)
#})

# col_satisfies
#test_that("", {
#  expect_equal(,)
#})

# col_satisfiesIf
#test_that("", {
#  expect_equal(,)
#})

# col_hasPredictability
#test_that("", {
#  expect_equal(,)
#})

# col_hasTypeConsistency
#test_that("", {
#  expect_equal(,)
#})

# col_hasCountDistinct
#test_that("", {
#  expect_equal(,)
#})

# col_hasMin
#test_that("", {
#  expect_equal(,)
#})

# col_hasMax
#test_that("", {
#  expect_equal(,)
#})

# col_hasMean
#test_that("", {
#  expect_equal(,)
#})

# col_hasStandardDeviation
#test_that("", {
#  expect_equal(,)
#})

# col_hasQuantile
#test_that("", {
#  expect_equal(,)
#})

# col_hasEntropy
#test_that("", {
#  expect_equal(,)
#})

# col_hasMutualInformation
#test_that("", {
#  expect_equal(,)
#})

# col_hasHistogramValue
#test_that("", {
#  expect_equal(,)
#})

# col_hasCorrelation
#test_that("", {
#  expect_equal(,)
#})

# col_hasNoAnomalies
#test_that("", {
#  expect_equal(,)
#})

