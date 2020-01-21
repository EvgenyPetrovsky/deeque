test_that("severity rank is equal to label position in the list", {
  for (i in seq_len(length(severity))) {
    slabel <- severity[[i]]
    srank  <- i
    expect_equal(severity_rank(slabel), srank)
  }
})

test_that("severity_rank returns as many values as receives", {
  for (i in c(0, 1, 10, 100, 1000)) {
    v <- replicate(n = i, expr = severity[[1]])
    r <- severity_rank(v)
    expect_equal(length(r), i)
  }
})

test_that("New group of checks is an empty list of length 0", {
  g <- new_group()
  expect_equal(length(g), 0)
  expect_equal(g, list())
})

test_that("Function new check returns list object", {
  c <- new_check(
    description = "New check for testing",
    severity = severity[[1]],
    function_name = identity
    )
  expect_equal(is.list(c), TRUE)
})

test_that("function new check fails if no description provided", {
  expect_error(
    new_check(severity = "INFO", function_name = col_isComplete, column = "a"),
    message = "description.+ missing"
  )
})

test_that("Function new_check fails if no severity defined", {
  expect_error(
    new_check(
      description = "test",
      function_name = col_isComplete, column = "a"),
    message = "severity.+ missing"
  )
})

test_that("Function new_check fails if severity is invalid", {
  expect_error(
    new_check(
      description = "test", severity = "JOKE",
      function_name = col_isComplete, column = "a"),
    message = "severity.+valid.deequee::severity"
  )
})

test_that("Function new_check fails if function is not defined", {
  NULL
})

test_that("Function new check creates object with required elements", {
  c <- new_check("Test", "INFO", "column_1", deeque::col_isComplete)
  attr <- c("description", "severity", "function_name", "parameters")
  expect_equal(all(attr %in% names(c)), TRUE)
})

test_that("Function add_check increases number of elements in group by 1", {
  g0 <- new_group()
  c  <- new_check("Test", "INFO", "column_1", deeque::col_isComplete)
  g1 <- add_check(g0, c)
  expect_equal(length(g0), 0)
  expect_equal(length(g1), 1)
})

test_that("only check can be included into group", {
  NULL
})

test_that("new_checks_for_columns for N (number) columns returns N checks", {
  columns <- c("A", "B", "C")
  checks  <- new_checks_for_columns(
    columns = columns,
    description = "New check for testing",
    severity = severity[[1]],
    function_name = identity
    )
  expect_equal(length(checks), 3)
})

test_that("add list of n checks into group of x returns group of length x+n", {
  g0 <- new_group()
  columns <- c("A", "B", "C")
  checks  <- new_checks_for_columns(
    columns = columns,
    description = "New check for testing",
    severity = severity[[1]],
    function_name = identity
  )
  g3 <- add_checks(g0, checks)
  g6 <- add_checks(g3, checks)
  expect_equal(length(g3), 0 + 3)
  expect_equal(length(g6), 0 + 3 + 3)
})
