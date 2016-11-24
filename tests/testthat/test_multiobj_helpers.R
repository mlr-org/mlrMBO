context("multi-objective helpers")

test_that("multi-objective helpers", {
  # we had a bug here
  m = matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
  e = getWorstExtremePoint(m, minimize = c(TRUE, TRUE))
  expect_equal(e, c(3, 4))

  e = getWorstExtremePoint(m, minimize = c(FALSE, TRUE))
  expect_equal(e, c(1, 4))

  # we had a bug here
  m = matrix(c(1, 2, 3, 4, -10, 2), 3, 2, byrow = TRUE)

  e = getWorstExtremePoint(m, minimize = c(TRUE, TRUE))
  expect_equal(e, c(3, 4))
  e = getWorstExtremePoint(m, minimize = c(FALSE, FALSE))
  expect_equal(e, c(-10, 2))
})
