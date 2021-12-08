context("test-find_trait_gaps")

test_that("use", {
  expect_equal(
    find_trait_gaps(rep(0.1, 10)), integer(0)
  )
  expect_equal(
    find_trait_gaps(seq(-0.5, 0.5, by = 0.05)), integer(0)
  )
  expect_equal(
    find_trait_gaps(seq(-0.5, 0.5, by = 0.1)), 1:10
  )
  expect_equal(
    find_trait_gaps(c(rep(-Inf, 3), rep(Inf, 3))), 3
  )
  # no split from rounding distances
  expect_equal(
    find_trait_gaps(c(0, 0.06)), integer(0)
  )
})

test_that("abuse", {

  expect_error(
    object =  find_trait_gaps(5:1),
    regexp =
      "'traits' must be sorted by ascending order before checking for gaps."
  )
  expect_error(
    object =  find_trait_gaps(NA),
    regexp = "'traits' must be numeric"
  )
  expect_error(
    object =  find_trait_gaps(numeric(0)),
    regexp = "'traits' is empty"
  )

})
