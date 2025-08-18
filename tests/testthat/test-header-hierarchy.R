library(testthat)

test_that('header hierarchy, get_values_and_cellwidth gives correct widths 1', {
  test_columns = c('v1', 'v2\th1', 'v3\th1')
  test_width = c(1, 1, 1)
  ans = rtf.deluxe:::get_values_and_cellwidth(column_names=test_columns, base_width=test_width)
  expect_equal(ans[[2]]$width, c(1, 2))
})

test_that('header hierarchy, get_values_and_cellwidth gives correct widths 2', {
  test_columns = c('v1', 'v2', 'v3\th1', 'v4\th1')
  test_width = c(1, 1, 1, 1)
  ans = rtf.deluxe:::get_values_and_cellwidth(column_names=test_columns, base_width=test_width)
  expect_equal(ans[[2]]$width, c(1, 1, 2))
})
