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
test_that_lov_thing <- function() {
  data <- df(a = 1:3, b = c(1, 2, NA), c = c(1, NA, NA), d = c(1, 1, 3))
  expect_equal(col_isInLOV(data, "a", c(1, 3)), c(T,F,T))
  expect_equal(col_isInLOV(data, "a", unique(data$a)), c(T,T,T))
  expect_equal(col_isInLOV(data, "a", 1:10), c(T,T,T))
  expect_equal(col_isInLOV(data, "b", unique(data$b)), c(T,T,T))
}
test_that("col_isInLOV returns proper result", {
  test_that_lov_thing()
})

# col_isContainedIn
test_that("col_isContainedIn returns proper result (similar to col_isInLOV)", {
  test_that_lov_thing()
})

# col_hasConsistentType
#test_that("", {
#  expect_equal(,)
#})

# col_isNonNegative
test_that("col_isNonNegative returns proper result", {
  data <- df(
    a = c(-1, 0, 1, NA)
  )
  expect_equal(col_isNonNegative(data, "a"), c(F, T, T, NA))
})

# col_isLessThan
test_that("col_isLessThan returns proper result", {
  data <- df(
    a = c(0, 0, 0, 0),
    b = c(-1, 0, 1, NA)
  )
  expect_equal(col_isLessThan(data, "a", "b"), c(F, F, T, NA))
})

# col_isNotLessThan
test_that("col_isNotLessThan returns proper result", {
  data <- df(
    a = c(0, 0, 0, 0),
    b = c(-1, 0, 1, NA)
  )
  expect_equal(col_isNotLessThan(data, "a", "b"), c(T, T, F, NA))
})

# col_isGreaterThan
test_that("col_isGreaterThan returns proper result", {
  data <- df(
    a = c(0, 0, 0, 0),
    b = c(-1, 0, 1, NA)
  )
  expect_equal(col_isGreaterThan(data, "a", "b"), c(T, F, F, NA))
})

# col_isNotGreaterThan
test_that("col_isGreatercol_isNotGreaterThanThan returns proper result", {
  data <- df(
    a = c(0, 0, 0, 0),
    b = c(-1, 0, 1, NA)
  )
  expect_equal(col_isNotGreaterThan(data, "a", "b"), c(F, T, T, NA))
})

# col_hasValue
test_that("col_hasValue returns proper result", {
  data <- df(
    a = c("A", "AA", "AAA", "AAAA", "AAAAA"),
    b = 1:5,
    stringsAsFactors = FALSE
  )
  f_a <- function(x) nchar(x) < 3
  f_b <- function(x) x < 3
  expect_equal(col_hasValue(data, "a", f_a), c(T, T, F, F, F))
  expect_equal(col_hasValue(data, "b", f_b), c(T, T, F, F, F))
})

# col_hasAllValues
test_that("col_hasAllValues returns proper result", {
  data <- df(
    a = 1:5,
    b = LETTERS[1:5]
  )
  expect_equal(col_hasAllValues(data, "a", 1:3), TRUE)
  expect_equal(col_hasAllValues(data, "a", 1:5), TRUE)
  expect_equal(col_hasAllValues(data, "a", 1:6), FALSE)
  expect_equal(col_hasAllValues(data, "a", 6), FALSE)
  expect_equal(col_hasAllValues(data, "b", LETTERS[1:3]), TRUE)
  expect_equal(col_hasAllValues(data, "b", LETTERS[1:5]), TRUE)
  expect_equal(col_hasAllValues(data, "b", LETTERS[1:6]), FALSE)
  expect_equal(col_hasAllValues(data, "b", LETTERS[6]), FALSE)
})

# col_hasAnyValue
test_that("col_hasAnyValue returns proper result", {
  data <- df(
    a = 1:5,
    b = LETTERS[1:5]
  )
  expect_equal(col_hasAnyValue(data, "a", 1:3), TRUE)
  expect_equal(col_hasAnyValue(data, "a", 1:5), TRUE)
  expect_equal(col_hasAnyValue(data, "a", 1:6), TRUE)
  expect_equal(col_hasAnyValue(data, "a", 6), FALSE)
  expect_equal(col_hasAnyValue(data, "b", LETTERS[1:3]), TRUE)
  expect_equal(col_hasAnyValue(data, "b", LETTERS[1:5]), TRUE)
  expect_equal(col_hasAnyValue(data, "b", LETTERS[1:6]), TRUE)
  expect_equal(col_hasAnyValue(data, "b", LETTERS[6]), FALSE)
})

# col_satisfies
test_that("col_satisfies returns proper result", {
  data <- df(
    a = c("A", "AA", "AAA", "AAAA", "AAAAA"),
    b = 1:5,
    stringsAsFactors = FALSE
  )
  f_a <- function(x) nchar(x$a) < 3
  f_b <- function(x) x$b < 3
  expect_equal(col_satisfies(data, f_a), c(T, T, F, F, F))
  expect_equal(col_satisfies(data, f_b), c(T, T, F, F, F))
})

# col_satisfiesIf
test_that("col_satisfiesIf returns proper result", {
  Y <- "Y"; N <- "N"
  data <- df(
    a = -2:2,
    c = c(-9, -5, +6, +9, +12),
    d = c(-1, +3, -6, +9, +13),
    e = c(N, N, Y, Y, Y),
    f = c(NA, NA, -1.01, +0.25, NA),
    stringsAsFactors = FALSE
  )
  expect_equal(
    col_satisfiesIf(data,
      predicate    = function(x) with(x, a <= 1),
      if_predicate = function(x) with(x, a >= 0)),
    c(NA, NA, T, T, F))
  expect_equal(
    col_satisfiesIf(data,
      predicate    = function(x) with(x, c %% 3 == 0),
      if_predicate = function(x) with(x, c > 0)),
    c(NA, NA, T, T, T))
  expect_equal(
    col_satisfiesIf(data,
      predicate    = function(x) with(x, d > 0),
      if_predicate = function(x) with(x, d %% 3 == 0)),
    c(NA, T, F, T, NA))
  expect_equal(
    col_satisfiesIf(data,
      predicate    = function(x) with(x, !is.na(f)),
      if_predicate = function(x) with(x, e == Y)),
    c(NA, NA, T, T, F))
})

# col_hasPredictability
#test_that("", {
#  expect_equal(,)
#})

#col_hasType
test_that("col_hasType recognize types properly", {

  typeOf <- function(val, is) {
    dat <- data.frame("a" = val, stringsAsFactors = F)
    col_hasType(dat, "a", is)
  }

  typs <- list(
    "numeric" = 1.1,
    "integer" = 1L,
    "logical" = TRUE,
    "character" = "Hello",
    "factor" = factor("A")
  )
  # matrix where row is actual type and column is hypothetical type
  mc <- c(
    T,F,F,F,F,
    T,T,F,F,F,
    F,F,T,F,F,
    F,F,F,T,F,
    F,F,F,F,T
  )
  mx <- matrix(
    data = mc, nrow = length(typs), length(typs), byrow = T, 
    dimnames = replicate(2, list(names(typs))))

  for (typ in names(typs)) {
    val <- typs[[typ]]
    for (idx in names(typs)) {
      expect_equal(typeOf(val, is = idx), mx[typ, idx])
    }
  }
})

test_that("col_hasType fails when requested to check unknown type", {
  data <- df(a = 1:3)
  expect_error(col_hasType(data, "a", "dummy"))
})

# col_hasTypeConsistency
#test_that("", {
#  expect_equal(,)
#})

# col_hasCountDistinct
test_that("col_hasCountDistinct returns proper result", {
  data <- df(
    a = 1:26,
    b = LETTERS,
    c = sample.int(n=10, size=26, replace=T),
    stringsAsFactors = F
  )
  expect_equal(col_hasCountDistinct(data, "a", udf_eq(26)), TRUE)
  expect_equal(col_hasCountDistinct(data, "b", udf_eq(26)), TRUE)
  expect_equal(col_hasCountDistinct(data, "c", udf_le(10)), TRUE)
})

# col_hasMin
test_that("col_hasMin returns proper result", {
  data <- df(
    a = 1:26,
    b = LETTERS,
    c = sample.int(n=10, size=26, replace=T),
    stringsAsFactors = F
  )
  expect_equal(col_hasMin(data, "a", udf_gt(1)), FALSE)
  expect_equal(col_hasMin(data, "b", udf_eq("A")), TRUE)
  expect_equal(col_hasMin(data, "c", udf_gt(0)), TRUE)
})

# col_hasMax
test_that("col_hasMax returns proper result", {
  data <- df(
    a = 1:26,
    b = LETTERS,
    c = sample.int(n=10, size=26, replace=T),
    stringsAsFactors = F
  )
  expect_equal(col_hasMax(data, "a", udf_lt(26)), FALSE)
  expect_equal(col_hasMax(data, "b", udf_eq("Z")), TRUE)
  expect_equal(col_hasMax(data, "c", udf_le(10)), TRUE)
})

# col_hasMean
test_that("col_hasMean returns proper result", {
  data <- df(
    a = 1:26,
    b = replicate(26, 0),
    c = sample.int(n=10, size=26, replace=T),
    d = c(NA, replicate(25, 5)),
    stringsAsFactors = F
  )
  expect_equal(col_hasMean(data, "a", udf_eq(13.5)), TRUE)
  expect_equal(col_hasMean(data, "b", udf_eq(0)), TRUE)
  expect_equal(col_hasMean(data, "c", udf_between(0,10)), TRUE)
  expect_equal(col_hasMean(data, "d", udf_eq(5)), TRUE)
})

# col_hasStandardDeviation
test_that("col_hasStandardDeviation returns proper result", {
  
  data <- df(
    a = 1:26,
    b = replicate(26, 0),
    c = sample.int(n=10, size=26, replace=T),
    d = c(NA, replicate(25, 5)),
    stringsAsFactors = F
  )
  expect_equal(col_hasStandardDeviation(data, "a", udf_eq(sd(data$a))), TRUE)
  expect_equal(col_hasStandardDeviation(data, "b", udf_eq(0)), TRUE)
  expect_equal(col_hasStandardDeviation(data, "c", udf_between(0,10)), TRUE)
  expect_equal(col_hasStandardDeviation(data, "d", udf_eq(0)), TRUE)
})

# col_hasQuantile
test_that("col_hasQuantile returns proper result", {
  data <- df(
    a = seq(from=-1.5, to=+1.5, by=1),
    b = c(NA, 1, NA, 3),
    c = sample.int(n=10, size=4)
  )

  expect_equal(
    col_hasQuantile(data, "a", probability = 0.5, udf_eq(0)), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "a", probability = 0, udf_eq(-1.5)), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "a", probability = 1, udf_eq(1.5)), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "b", probability = 0.5, udf_eq(2)),
    TRUE)
  expect_equal(
    col_hasQuantile(data, "b", probability = 0, udf_eq(1)), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "b", probability = 1, udf_eq(3)), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "c", probability = .5, udf_eq(quantile(data$c, .5, names = F))), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "c", probability = 0, udf_eq(min(data$c))), 
    TRUE)
  expect_equal(
    col_hasQuantile(data, "c", probability = 1, udf_eq(max(data$c))), 
    TRUE)
  expect_error(
    col_hasQuantile(data, "c", probability = 1.1, udf_eq(NA)), 
    message = "must be in range 0..1")
  expect_error(
    col_hasQuantile(data, "c", probability = -.1, udf_eq(NA)), 
    message = "must be in range 0..1")
  
})

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
test_that("col_hasCorrelation returns proper result", {
  data <- df(
    a = 1:10,
    b = 10:1,
    c = sample.int(10, 10)
  )
  expect_equal(col_hasCorrelation(data, "a", "a", udf_eq(1)),TRUE)
  expect_equal(col_hasCorrelation(data, "a", "b", udf_eq(-1)),TRUE)
  expect_equal(col_hasCorrelation(data, "a", "c", udf_eq(cor(data$a, data$c))),TRUE)
})

# col_hasNoAnomalies
#test_that("", {
#  expect_equal(,)
#})

