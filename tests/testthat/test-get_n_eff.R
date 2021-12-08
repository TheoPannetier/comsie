context("test-get_n_eff")

test_comm <- c(-0.39, 0.25, 1.3)

test_that("use", {
  # Ordinary cases
  expect_equal(
    get_n_eff_cpp(test_comm, competition_sd = sqrt(0.5)),
    c(1.7214083, 1.995956, 1.389532), tolerance = 1e-6
  )
})

test_that("abuse", {
  expect_error(get_n_eff(Inf, Inf), "'z' contains forbidden values: Inf")
  expect_error(get_n_eff(-Inf, -Inf), "'z' contains forbidden values: -Inf")
  expect_error(
    get_n_eff(0, 0, competition_sd = -8),
    "'competition_sd' must be a positive numeric"
  )

  expect_error(get_n_eff("1.6", "1.6"), "'z' must be numeric")
  expect_error(get_n_eff(NA, NA), "'z' must be numeric")
  expect_error(get_n_eff(NaN, NaN), "'z' contains one or more NaNs")
  expect_error(get_n_eff(integer(), integer()), "'z' is empty")
})
