# tab_hasRowCount
test_that("Empty dataframe has 0 rows", {
  tab <- data.frame()
  expect_equal(tab_hasRowCount(tab, udf = function(x) x == 0), TRUE)
  expect_equal(tab_hasRowCount(tab, udf = function(x) x != 0), FALSE)
})
test_that("udf functoin works correct for number of rows check", {
  for (i in sample.int(1000, 5)) {
    tab <- data.frame(col = replicate(i, 42))
    expect_equal(tab_hasRowCount(tab, udf = function(x) x == i), TRUE)
    expect_equal(tab_hasRowCount(tab, udf = function(x) x != i), FALSE)
    expect_equal(tab_hasRowCount(tab, udf = function(x) x > i), FALSE)
    expect_equal(tab_hasRowCount(tab, udf = function(x) x < i), FALSE)
    expect_equal(tab_hasRowCount(tab, udf = function(x) x >= i), TRUE)
    expect_equal(tab_hasRowCount(tab, udf = function(x) x <= i), TRUE)
  }
})

# tab_hasColumnCount
test_that("empty dataframe has 0 columns", {
  tab <- data.frame()
  expect_equal(tab_hasColumnCount(tab, udf = function(x) x == 0), TRUE)
  expect_equal(tab_hasColumnCount(tab, udf = function(x) x != 0), FALSE)
})

test_that("udf functoin works correct for number of columns check", {
  for (i in sample.int(100, 5)) {
    tab <- data.frame(Map(f = function(x) 1, 1:i))
    expect_equal(tab_hasColumnCount(tab, udf = function(x) x == i), TRUE)
    expect_equal(tab_hasColumnCount(tab, udf = function(x) x != i), FALSE)
    expect_equal(tab_hasColumnCount(tab, udf = function(x) x > i), FALSE)
    expect_equal(tab_hasColumnCount(tab, udf = function(x) x < i), FALSE)
    expect_equal(tab_hasColumnCount(tab, udf = function(x) x >= i), TRUE)
    expect_equal(tab_hasColumnCount(tab, udf = function(x) x <= i), TRUE)
  }
})

# tab_hasColumn
test_that("tab_hasColumn check for empty dataframe always returns FALSE", {
  tab <- data.frame()
  expect_equal(tab_hasColumn(tab, "A"), FALSE)
  expect_equal(tab_hasColumn(tab, "B"), FALSE)
})

test_that("tab_hasColumn check returns TRUE when column exists", {
  tab <- data.frame(a = 1, b = 1)
  expect_equal(tab_hasColumn(tab, "a"), TRUE)
  expect_equal(tab_hasColumn(tab, "b"), TRUE)
})

test_that("tab_hasColumn check returns FALSE when column exists", {
  tab <- data.frame(a = 1, b = 1)
  expect_equal(tab_hasColumn(tab, "A"), FALSE)
  expect_equal(tab_hasColumn(tab, "B"), FALSE)
})

# tab_hasColumns
test_that("tab_hasColumns check for empty dataframe always returns FALSE", {
  tab <- data.frame()
  expect_equal(tab_hasColumns(tab, "A"), FALSE)
  expect_equal(tab_hasColumns(tab, "B"), FALSE)
  expect_equal(tab_hasColumns(tab, c("A", "B")), FALSE)
})

test_that("tab_hasColumns check returns TRUE when all columns exist", {
  tab <- data.frame(a = 1, b = 1, c = 1)
  expect_equal(tab_hasColumns(tab, "a"), TRUE)
  expect_equal(tab_hasColumns(tab, c("a", "a", "a")), TRUE)
  expect_equal(tab_hasColumns(tab, "b"), TRUE)
  expect_equal(tab_hasColumns(tab, c("a", "b")), TRUE)
  expect_equal(tab_hasColumns(tab, c("a", "b", "c")), TRUE)
})

test_that("tab_hasColumns check returns FALSE when one column does not exist", {
  tab <- data.frame(a = 1, b = 1, c = 1)
  expect_equal(tab_hasColumns(tab, "A"), FALSE)
  expect_equal(tab_hasColumns(tab, c("A", "A", "A")), FALSE)
  expect_equal(tab_hasColumns(tab, "B"), FALSE)
  expect_equal(tab_hasColumns(tab, c("a", "B")), FALSE)
  expect_equal(tab_hasColumns(tab, c("a", "b", "c", "A")), FALSE)
})

# tab_hasUniqueKey
test_that("tab_hasUniqueKey stops when dataframe has 0 columns", {
  tab <- data.frame()
  uk1 <- "uk_col_1"
  err_message <- paste0(uk1, ".*", "missing")
  expect_error(tab_hasUniqueKey(tab, uk1), err_message)
})

test_that("tab_hasUniqueKey stops when dataframe has 0 rows", {
  tab <- data.frame(a = numeric(), b = numeric(), c = numeric())
  uk1 <- colnames(tab)
  err_message <- "0 rows"
  expect_error(tab_hasUniqueKey(tab, uk1), err_message)
})

test_that("tab_hasUniqueKey stops when dataframe has no specified columns", {
  tab <- data.frame(a = 1, b = 1, c = 1)
  err_message <- paste0("Column", ".*", "missing")
  expect_error(tab_hasUniqueKey(tab, "A"), err_message)
  expect_error(tab_hasUniqueKey(tab, c("A", "A")), err_message)
  expect_error(tab_hasUniqueKey(tab, c("a", "A")), err_message)
})

test_that("tab_hasUniqueKey returns TRUE when column has unique values", {
  tab <- data.frame(a = c(1:26), b = LETTERS, c = letters)
  expect_equal(tab_hasUniqueKey(tab, c("a")), TRUE)
  expect_equal(tab_hasUniqueKey(tab, c("b")), TRUE)
  expect_equal(tab_hasUniqueKey(tab, c("c")), TRUE)
})

test_that("tab_hasUniqueKey returns FALSE when column has non-unique values", {
  tab <- data.frame(a = c(1, 1:10), b = FALSE, c = TRUE)
  expect_equal(tab_hasUniqueKey(tab, c("a")), FALSE)
  expect_equal(tab_hasUniqueKey(tab, c("b")), FALSE)
  expect_equal(tab_hasUniqueKey(tab, c("c")), FALSE)
})

test_that("tab_hasUniqueKey returns TRUE when combination of columns is unique", {
  tab <- data.frame(
    a = c(0, 1, 0, 1),
    b = c(0, 0, 1, 1),
    c = c(0, 0, 0, 0))
  expect_equal(tab_hasUniqueKey(tab, c("a")), FALSE)
  expect_equal(tab_hasUniqueKey(tab, c("b")), FALSE)
  expect_equal(tab_hasUniqueKey(tab, c("a", "b")), TRUE)
  expect_equal(tab_hasUniqueKey(tab, c("a", "b", "c")), TRUE)
  expect_equal(tab_hasUniqueKey(tab, c("a", "c")), FALSE)
})
