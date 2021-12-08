context("test-rate_estim")

test_that("simultaneous speciation", {

  #                       |----2000----|
  #           |---2500----|
  #           |           |----2000----|
  # --1000----|
  #           |           |----2000----|
  #           |---2500----|
  #                       |----2000----|

  phylo <- ape::read.tree(
    text = "(((A:2000, B:2000):2500, (C:2000, D:2000):2500):1000);"
  )
  # Five placeholder replicates
  multi_phylo <- purrr::map(1:5, function(x) phylo)
  rates_tbl <- multi_phylo %>% estimate_dd_rates()
  exptd_tbl <- tibble::tibble(
    "N" = c(1, 1, 2, 2, 3, 3),
    "rate" = rep(c("speciation", "extinction"), 3),
    "value" = c(1e-03, NA, 2e-04, NA,  Inf, NA)
  )
  expect_equal(
    rates_tbl, exptd_tbl
  )
})
