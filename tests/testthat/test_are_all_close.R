testthat::test_that(
  "Return True when two vectors are close", {
    set.seed(100)
    x = runif(100, min = 1, max = 10)
    y = x + 1e-8
    x_y_close <- are_all_close(x, y)
    testthat::expect_true(
      are_all_close(x, y)
    )
  }
)

testthat::test_that(
  "Return False when relative error is above rel_tol", {
    set.seed(100)
    abs_tol = 1e-5
    rel_tol = 1e-9
    x = runif(100, min = 1, max = 10)
    y = x + 1e-6
    x_y_close <- are_all_close(x, y, abs_tol, rel_tol)
    testthat::expect_false(
      are_all_close(x, y, abs_tol, rel_tol)
    )
  }
)

testthat::test_that(
  "Return False when absolute error is above abs_tol", {
    set.seed(100)
    abs_tol = 1e-9
    rel_tol = 1e-3
    x = runif(100, min = 1, max = 10)
    y = x + 1e-7
    x_y_close <- are_all_close(x, y, abs_tol, rel_tol)
    testthat::expect_false(
      are_all_close(x, y, abs_tol, rel_tol)
    )
  }
)
