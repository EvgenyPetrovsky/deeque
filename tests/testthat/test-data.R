test_that("severity list has names that are equal to values", {
  for (i in names(severity)) {
    slabel <- severity[[i]]
    expect_equal(slabel, i)
  }
})

test_that("column type list has names that are equal to values", {
  for (i in names(col_type)) {
    ctlabel <- col_type[[i]]
    expect_equal(ctlabel, i)
  }
})
