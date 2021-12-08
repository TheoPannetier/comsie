context("test-run_simulation")

test_that("diverging_community", {
  # community starts with split values, expect speciation in the next step
  diverging_pop <- default_init_comm()
  diverging_pop$z[6:10] <- 0.1
  output <- run_simulation(
    init_comm = diverging_pop, nb_gens = 1, path_to_output = NULL
  )
  species <- output %>%
    dplyr::filter(t == 1) %>%
    dplyr::select(species) %>%
    unique %>%
    unlist()
  ancestral_species <- output %>%
    dplyr::filter(t == 1) %>%
    dplyr::select(ancestral_species) %>%
    unique() %>%
    unlist()
  expect_gt(length(species), 1)
  expect_gt(length(ancestral_species), 1)
})

test_that("output_format", {
  nb_gens <- 5
  output <- run_simulation(nb_gens = nb_gens, path_to_output = NULL)

  expect_equal(length(output), 4)
  # Main community columns
  expect_silent(
    output[, c("z", "species", "ancestral_species")] %>% test_comrad_comm()
  )
  # Format of columns missed by test_comrad_comm()
  expect_true(any(output$t >= 0))
  # Assert output corresponds to last generation
  expect_equal(unique(output$t), nb_gens)
})

test_that("extinction", {
  expect_output(
    # TPK
    output <- run_simulation(
      carrying_cap_opt = 0, nb_gens = 1, path_to_output = NULL),
    "\\nCommunity has gone extinct at generation 1 "
  )
})

test_that("parameter_abuse", {
  expect_error(
    run_simulation(init_comm = rep(0, 10), path_to_output = NULL, nb_gens = 20),
    "'init_comm' is not a tibble."
  )
  expect_error(
    run_simulation(path_to_output = 1, nb_gens = 20),
    "'path_to_output' must be either null or a character."
  )
  expect_error(
    run_simulation(nb_gens = 12.3, path_to_output = NULL),
    "'nb_gens' must be an integer"
  )
  expect_error(
    run_simulation(sampling_freq = 12.3, path_to_output = NULL, nb_gens = 20),
    "'sampling_freq' must be an integer"
  )
  expect_error(
    run_simulation(seed = 1.4, path_to_output = NULL, nb_gens = 20),
    "'seed' must be an integer"
  )
  expect_error(
    run_simulation(nb_gens = 1.4, path_to_output = NULL),
    "'nb_gens' must be an integer"
  )
  expect_error(
    run_simulation(nb_gens = 0, path_to_output = NULL),
    "'nb_gens' contains forbidden values: 0"
  )
  expect_error(
    run_simulation(nb_gens = Inf, path_to_output = NULL),
    "'nb_gens' contains forbidden values: Inf"
  )
  expect_error(
    run_simulation(prob_mutation = 15, path_to_output = NULL, nb_gens = 20),
    "'prob_mutation' must be a numeric between 0 and 1"
  )

})

is_on_ci <- (Sys.getenv("TRAVIS") != "" || Sys.getenv("APPVEYOR") != "")

testthat::test_that("Simulation is reproducible", {
  if (!is_on_ci) skip("Only run on CI")
  # Same seed generates same results
  sim_reps <- purrr::map(
    c(180, 180, 999), # seeds
    function(seed) {
      run_simulation(
        path_to_output = NULL,
        nb_gens = 500,
        seed = seed
      )
    })
  expect_equal(sim_reps[[1]], sim_reps[[2]])
  expect_failure(expect_equal(sim_reps[[1]], sim_reps[[3]]))
})
