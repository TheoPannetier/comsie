context("test-get_fitness")

test_that("use", {
  expect_equal(get_fitness(rep(0.5, 5), carrying_cap_opt = 0), rep(0, 5))
  expect_equal(get_fitness(rep(0.5, 5), carrying_cap_opt = Inf), rep(exp(1), 5))
  expect_equal(
    get_fitness(rep(0.5, 5), carrying_cap_opt = 0, growth_rate = 0), rep(1, 5)
  )
})

test_that("abuse", {
  expect_error(
    object = get_fitness(Inf),
    regexp = "'traits_comm' contains forbidden values: Inf",
  )

  expect_error(
    object = get_fitness(-Inf),
    regexp = "'traits_comm' contains forbidden values: -Inf",
  )

  expect_error(
    object = get_fitness("doom"),
    regexp = "'traits_comm' must be numeric",
  )
  expect_error(
    object = get_fitness(NaN),
    regexp = "'traits_comm' contains one or more NaNs",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), growth_rate = TRUE),
    regexp = "'growth_rate' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), competition_sd = TRUE),
    regexp = "'competition_sd' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), trait_opt = "x"),
    regexp = "'trait_opt' must be numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carrying_cap_opt = -1),
    regexp = "'carrying_cap_opt' must be a positive numeric",
  )
  expect_error(
    object = get_fitness(rep(0.5, 3), carrying_cap_sd = "elmo"),
    regexp = "'carrying_cap_sd' must be numeric",
  )


})

is_on_ci <- (Sys.getenv("TRAVIS") != "" || Sys.getenv("APPVEYOR") != "")

test_that("fitness_functions", {
  if (!is_on_ci) {
    skip("Only test fitness functions on CI")
  }
  ##  Positive logistic function
  # Case 1. N/K close to 0
  expect_equal(
    get_fitness(
      0,
      fitness_func = fitness_func_logistic
    ),
    0.999
  )
  # Case 2. N/K = 1/2
  expect_equal(
    get_fitness(
      rep(0, 500),
      fitness_func = fitness_func_logistic
    ),
    rep(default_growth_rate() / 2, 500)
  )
  # Case 3. N/K = 1
  expect_equal(
    get_fitness(
      rep(0, 1000),
      fitness_func = fitness_func_logistic
    ),
    rep(0, 1000)
  )
  # Case 4. N/K < 0
  expect_equal(
    get_fitness(
      rep(0, 10000),
      fitness_func = fitness_func_logistic
    ),
    rep(0, 10000)
  )
  # Handling NaN situations
  expect_equal(
    get_fitness(
      rep(0.5, 5),
      carrying_cap_opt = 0,
      growth_rate = 0,
      fitness_func = fitness_func_logistic
      ),
    rep(0, 5)
  )

  ##  Ricker function
  # Case 1. N/K close to 0
  expect_equal(
    get_fitness(
      0,
      fitness_func = fitness_func_ricker
    ),
    exp(default_growth_rate() * 0.999)
  )
  # Case 2. N/K = 1/2
  expect_equal(
    get_fitness(
      rep(0, 500),
      fitness_func = fitness_func_ricker
    ),
    rep(exp(default_growth_rate() * 0.5), 500)
  )
  # Case 3. N/K = 1
  expect_equal(
    get_fitness(
      rep(0, 1000),
      fitness_func = fitness_func_ricker
    ),
    rep(1, 1000)
  )
  # Case 4. N/K < 0
  expect_equal(
    get_fitness(
      rep(0, 2000),
      fitness_func = fitness_func_ricker
    ),
    rep(exp(-default_growth_rate()), 2000)
  )

  ##  Pontarp function
  # Case 1. N/K close to 0
  expect_equal(
    get_fitness(
      0,
      fitness_func = fitness_func_pontarp
    ),
    default_growth_rate() * 0.999 + 1
  )
  # Case 2. N/K = 1/2
  expect_equal(
    get_fitness(
      rep(0, 500),
      fitness_func = fitness_func_pontarp
    ),
    rep(default_growth_rate() * 0.5 + 1, 500)
  )
  # Case 3. N/K = 1
  expect_equal(
    get_fitness(
      rep(0, 1000),
      fitness_func = fitness_func_pontarp
    ),
    rep(1, 1000)
  )
  # Case 4. N/K < 0
  expect_equal(
    get_fitness(
      rep(0, 2000),
      fitness_func = fitness_func_pontarp
    ),
    rep(max(1 - default_growth_rate(), 0), 2000)
  )
  # Handling NaN situations
  expect_equal(
    get_fitness(
      rep(0.5, 5),
      carrying_cap_opt = 0,
      growth_rate = 0,
      fitness_func = fitness_func_pontarp
    ),
    rep(1, 5)
  )

})
