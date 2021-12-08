context("test-get_comp_coeff_comm")

is_on_ci <- (Sys.getenv("TRAVIS") != "" || Sys.getenv("APPVEYOR") != "")
nb_random_comms <- ifelse(is_on_ci, 20, 1)

test_that("regular_cases", {
  # Calibrated result
  expect_equal(
    get_comp_coeff_comm(
      trait_ind = 0,
      traits_comm = seq(-1, 0, by = 0.25),
      competition_sd = 0.2
    ),
    c(3.726653e-06, 8.838263e-04, 4.393693e-02, 4.578334e-01, 1),
    tolerance = 1e-07
  )

  # All individuals identical
  expect_equal(
    get_comp_coeff_comm(trait_ind = 0, traits_comm = rep(0, 3)),
    rep(1, 3)
  )
  # All individuals identical and infinite competition width
  expect_equal(
    get_comp_coeff_comm(
      trait_ind = 0,
      traits_comm = rep(0, 5),
      competition_sd = Inf
    ),
    rep(1, 5)
  )

  # All individuals identical and zero-width competition
  expect_equal(
    get_comp_coeff_comm(
      trait_ind = 0,
      traits_comm = rep(0, 5),
      competition_sd = 0
    ),
    rep(0, 5)
  )
})

test_that("random_comms", {
  for (i in nb_random_comms) {
    random_comm <- rnorm(5, 0, 0.1)
    random_sd <- runif(1, 0, 1)

    # Regular random case
    random_coeffs <- get_comp_coeff_comm(
      trait_ind = random_comm[1],
      traits_comm =  random_comm,
      competition_sd = random_sd
    )
    expect_silent(testarg_num(random_coeffs))
    expect_silent(testarg_prop(random_coeffs))

    # Zero-width competition width
    expect_equal(
      get_comp_coeff_comm(
        trait_ind = random_comm[1],
        traits_comm =  random_comm,
        competition_sd = 0
      ),
      rep(0, 5)
    )
    # Infinite competition width
    expect_equal(
      get_comp_coeff_comm(
        trait_ind = random_comm[1],
        traits_comm = random_comm,
        competition_sd = Inf
      ),
      rep(1, 5)
    )
  }
})

test_that("abuse", {
  # Testing that values in the arguments are of the correct format takes
  # too much on the running time. I  have left only the most important and trust
  # higher function calls to control the right format of parameters.

  expect_error(
    get_comp_coeff_comm(trait_ind = c(0, 0), traits_comm = c(0, 0)),
    "'trait_ind' must have length 1"
  )
})
