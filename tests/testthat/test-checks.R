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
