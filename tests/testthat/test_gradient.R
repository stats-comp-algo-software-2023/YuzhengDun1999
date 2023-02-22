approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  # Fill in
  for (i in 1:length(x)) {
    x_plus_dx = x
    x_plus_dx[i] = x_plus_dx[i] + dx
    numerical_grad[i] = (func(x_plus_dx) - func(x)) / dx
  }
  return(numerical_grad)
}

test_that(
  "test for the gradient of log likelihood in linear model", {
    n_obs = 32; n_pred = 4
    data = simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
    design = data$design; outcome = data$outcome
    beta = c(3, 1, 4, 1)
    analytical_grad <- lm_gradient(beta, design, outcome)
    numerical_grad <- approx_grad(function(beta) lm_log_likelihood(beta, design, outcome), beta)
    testthat::expect_true(
      are_all_close(analytical_grad, numerical_grad, abs_tol = 1e-3, rel_tol = 1e-3)
    )
  }
)
