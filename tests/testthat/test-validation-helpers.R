# udf_gt
test_that("'Greater than' generates another function", {
  expect_equal(typeof(udf_gt(3)), "closure")
})

test_that("'Greater than' generates function that takes one parameter", {
  x <- 3
  f <- udf_gt(x)
  expect_equal(length(formals(f)), 1)
})

test_that("'Greater than' generates function that returns boolean result", {
  x <- 3
  f <- udf_gt(x)
  expect_equal(typeof(f(x)), "logical")
})

test_that("'Greater than' generates function that works as expected", {
  x <- 3
  f <- udf_gt(x)
  expect_equal(f(x - 1), FALSE)
  expect_equal(f(x), FALSE)
  expect_equal(f(x + 1), TRUE)
})

# udf_ge
test_that("'Greater than or Equal to' generates function", {
  expect_equal(typeof(udf_ge(3)), "closure")
})

test_that("'Greater than or Equal to' generates function that takes one parameter", {
  x <- 3
  f <- udf_ge(x)
  expect_equal(length(formals(f)), 1)
})

test_that("'Greater than or Equal to' generates function that returns boolean result", {
  x <- 3
  f <- udf_ge(x)
  expect_equal(typeof(f(x)), "logical")
})

test_that("'Greater than or Equal to' generates function that works as expected", {
  x <- 3
  f <- udf_ge(x)
  expect_equal(f(x - 1), FALSE)
  expect_equal(f(x), TRUE)
  expect_equal(f(x + 1), TRUE)
})

# udf_lt
test_that("'Less than' generates function", {
  expect_equal(typeof(udf_lt(3)), "closure")
})

test_that("'Less than' generates function that takes one parameter", {
  x <- 3
  f <- udf_lt(x)
  expect_equal(length(formals(f)), 1)
})

test_that("'Less than' generates function that returns boolean result", {
  x <- 3
  f <- udf_lt(x)
  expect_equal(typeof(f(x)), "logical")
})

test_that("'Less than' generates function that works as expected", {
  x <- 3
  f <- udf_lt(x)
  expect_equal(f(x - 1), TRUE)
  expect_equal(f(x), FALSE)
  expect_equal(f(x + 1), FALSE)
})

# udf_le
test_that("'Less than or Equal to' generates function", {
  expect_equal(typeof(udf_le(3)), "closure")
})

test_that("'Less than or Equal to' generates function that takes one parameter", {
  x <- 3
  f <- udf_le(x)
  expect_equal(length(formals(f)), 1)
})

test_that("'Less than or Equal to' generates function that returns boolean result", {
  x <- 3
  f <- udf_le(x)
  expect_equal(typeof(f(x)), "logical")
})

test_that("'Less than or Equal to' generates function that works as expected", {
  x <- 3
  f <- udf_le(x)
  expect_equal(f(x - 1), TRUE)
  expect_equal(f(x), TRUE)
  expect_equal(f(x + 1), FALSE)
})

# udf_between
test_that("'Between' generates function", {
  x1 <- 3
  x2 <- 6
  expect_equal(typeof(udf_between(x1, x2, "b")), "closure")
})

test_that("'Between' generates function that takes one parameter", {
  x1 <- 3
  x2 <- 6
  f1 <- udf_between(x1, x2)
  f2 <- udf_between(x1, x2, "b")
  expect_equal(length(formals(f1)), 1)
  expect_equal(length(formals(f2)), 1)
})

test_that("'Between' generates function that returns boolean result", {
  x1 <- 3
  x2 <- 6
  f1 <- udf_between(x1, x2)
  f2 <- udf_between(x1, x2, "b")
  expect_equal(typeof(f1(x1)), "logical")
  expect_equal(typeof(f2(x1)), "logical")
})

test_that("'Between' generates function that works as expected", {
  x1 <- 3
  x2 <- 6

  fn <- udf_between(x1, x2, "n")
  expect_equal(fn(x1 - 1), FALSE)
  expect_equal(fn(x1), FALSE)
  expect_equal(fn(x1 + 1), TRUE)
  expect_equal(fn(x2 - 1), TRUE)
  expect_equal(fn(x2), FALSE)
  expect_equal(fn(x2 + 1), FALSE)

  fl <- udf_between(x1, x2, "l")
  expect_equal(fl(x1 - 1), FALSE)
  expect_equal(fl(x1), TRUE)
  expect_equal(fl(x1 + 1), TRUE)
  expect_equal(fl(x2 - 1), TRUE)
  expect_equal(fl(x2), FALSE)
  expect_equal(fl(x2 + 1), FALSE)

  fr <- udf_between(x1, x2, "r")
  expect_equal(fr(x1 - 1), FALSE)
  expect_equal(fr(x1), FALSE)
  expect_equal(fr(x1 + 1), TRUE)
  expect_equal(fr(x2 - 1), TRUE)
  expect_equal(fr(x2), TRUE)
  expect_equal(fr(x2 + 1), FALSE)

  fb <- udf_between(x1, x2, "b")
  expect_equal(fb(x1 - 1), FALSE)
  expect_equal(fb(x1), TRUE)
  expect_equal(fb(x1 + 1), TRUE)
  expect_equal(fb(x2 - 1), TRUE)
  expect_equal(fb(x2), TRUE)
  expect_equal(fb(x2 + 1), FALSE)
})

# udf_eq
test_that("'Equal to' generates function", {
  expect_equal(typeof(udf_eq(3)), "closure")
})

test_that("'Equal to' generates function that takes one parameter", {
  x <- 3
  f <- udf_eq(x)
  expect_equal(length(formals(f)), 1)
})

test_that("'Equal to' generates function that returns boolean result", {
  x <- 3
  f <- udf_eq(x)
  expect_equal(typeof(f(x)), "logical")
})

test_that("'Equal to' generates function that works as expected", {
  x <- 3
  f <- udf_eq(x)
  expect_equal(udf_eq(1)(1), TRUE)
  expect_equal(udf_eq(1)(.9999), FALSE)
  expect_equal(udf_eq(1 / 3)(1 / 3), TRUE)
  expect_equal(udf_eq(1 / 3)("Hello world"), FALSE)
  expect_equal(udf_eq("X")("X"), TRUE)
  expect_equal(udf_eq(TRUE)(TRUE), TRUE)
})
