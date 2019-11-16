test_that("severity rank is equal to label position in the list", {
  for (i in 1:length(severity)) {
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

#test_that("function new check fails if there is no description provided", {})

#test_that("Function new_check fails if there is no severity defined or severity is invalid", {})

#test_that("Function new_check fails if function is not defined") ???

#test_that("Function new check returns object with elements 'description', 'severity', 'function_name', 'parameters'", {})

#test_that("Function add_ccheck increases number of elements in group by 1")

#test_that("only check can be included into group")

test_that("add checks for columns returns list of length equal to number of column names", {
  group   <- new_group
  columns <- c("A", "B", "C")
  checks  <- new_checks_for_columns(
    columns = columns,
    description = "New check for testing", 
    severity = severity[[1]], 
    function_name = identity
    )
  expect_equal(length(checks), 3)
  expect_equal(length(add_check(group, checks)), 3)

})