
testthat::test_that("single lineage cases", {
  #
  # sp1 >---------------------> sp1
  #
  comrad_tbl <- tibble::tibble(
    "t" = 0:1000,
    "z" = 0,
    "species" = "sp1",
    "ancestral_species" = as.character(NA)
  )
  testthat::expect_silent(
    phylo <- comrad_tbl %>% sim_to_phylo()
  )
  # Tests
  testthat::expect_equal(
    phylo, ape::read.tree(text = "(sp1:1000);")
  )
  testthat::expect_silent(
    phylo <- comrad_tbl %>% sim_to_phylo(with_extinct = FALSE)
  )
  testthat::expect_equal(
    phylo,
    ape::read.tree(text = "(sp1:1000);")
  )
  # No crown lineages
  testthat::expect_error(
    phylo <- comrad_tbl %>% sim_to_phylo(include_stem = FALSE),
    "can't get a crown tree: only one lineage in the community"
  )
})

testthat::test_that("two-lineages cases", {
  #                |-----------> sp1
  # sp1 >----------|
  #                |-----------> sp2
  comrad_tbl <- tibble::tibble(
    "t" = c(0:1000, 500:1000),
    "z" = 0,
    "species" = c(rep("sp1", 1001), rep("sp2", 501)),
    "ancestral_species" = c(rep(NA, 1001), rep("sp1", 501))
  )
  # Stem
  phylo <- comrad_tbl %>% sim_to_phylo()
  exptd_phylo <- ape::read.tree(text = "((sp1:500, sp2:500):500);")
  testthat::expect_equal(
    phylo,  exptd_phylo
  )
  # Crown
  phylo <- comrad_tbl %>% sim_to_phylo(include_stem = FALSE)
  exptd_phylo <- ape::read.tree(text = "(sp1:500,sp2:500);")
  testthat::expect_equal(
    phylo, exptd_phylo
  )

  # Anc. species replaced by daughter
  #                |----X sp1
  # sp1 >----------|
  #                |-----------> sp2
  #
  comrad_tbl <- tibble::tibble(
    "t" = c(0:749, 500:1000),
    "z" = 0,
    "species" = c(rep("sp1", 750), rep("sp2", 501)),
    "ancestral_species" = c(rep(NA, 750), rep("sp1", 501))
  )
  # Full, stem
  phylo <- comrad_tbl %>% sim_to_phylo()
  exptd_phylo <- ape::read.tree(text = "((sp1:250, sp2:500):500);")
  testthat::expect_equal(
    phylo, exptd_phylo
  )
  # Full, crown
  phylo <- comrad_tbl %>% sim_to_phylo(include_stem = FALSE)
  exptd_phylo <- ape::read.tree(text = "(sp1:250, sp2:500);")
  testthat::expect_equal(
    phylo, exptd_phylo
  )
  # Extant, stem
  phylo <- comrad_tbl %>%
    sim_to_phylo(include_stem = TRUE, with_extinct = FALSE)
  exptd_phylo <- ape::read.tree(text = "(sp2:1000);")
  testthat::expect_failure(
    # ape cuts the stem out
    testthat::expect_equal(
      phylo, exptd_phylo
    )
  )
  # Extant, crown
  testthat::expect_error(
    phylo <- comrad_tbl %>%
      sim_to_phylo(with_extinct = FALSE, include_stem = FALSE),
    "can't get a crown tree: only one living lineage in the community"
  )

  # Daughter goes extinct
  #                |-----------> sp1
  # sp1 >----------|
  #                |----X sp2
  comrad_tbl <- tibble::tibble(
    "t" = c(0:1000, 500:749),
    "z" = 0,
    "species" = c(rep("sp1", 1001), rep("sp2", 250)),
    "ancestral_species" = c(rep(NA, 1001), rep("sp1", 250))
  )
  # Full, stem
  phylo <- comrad_tbl %>% sim_to_phylo()
  exptd_phylo <- ape::read.tree(text = "((sp1:500, sp2:250):500);")
  testthat::expect_equal(
    phylo, exptd_phylo
  )
  # Full, crown
  phylo <- comrad_tbl %>% sim_to_phylo(include_stem = FALSE)
  exptd_phylo <- ape::read.tree(text = "(sp1:500, sp2:250);")
  testthat::expect_equal(
    phylo, exptd_phylo
  )
  # Extant, stem
  phylo <- comrad_tbl %>%
    sim_to_phylo(include_stem = TRUE, with_extinct = FALSE)
  exptd_phylo <- ape::read.tree(text = "(sp1:1000);")
  testthat::expect_failure(
    # ape cuts the stem out
    testthat::expect_equal(
      phylo, exptd_phylo
    )
  )
  # Extant, crown
  testthat::expect_error(
    phylo <- comrad_tbl %>%
      sim_to_phylo(with_extinct = FALSE, include_stem = FALSE),
    "can't get a crown tree: only one living lineage in the community"
  )
})

testthat::test_that("three lineages cases", {
  #                 |------ sp1
  #         |-------|
  #         |       |------ sp3
  # --------|
  #         |-------X sp2

  comrad_tbl <- tibble::tibble(
    "t" = c(0:1000, 250:499, 750:1000),
    "z" = 0,
    "species" = c(rep("sp1", 1001), rep("sp2", 250), rep("sp3", 251)),
    "ancestral_species" = c(rep(NA, 1001), rep("sp1", 250), rep("sp1", 251))
  )

  # Full, stem
  exptd_phylo <- ape::read.tree(
    text = "(((sp1:250, sp3:250):500, sp2:250):250);"
  )
  phylo <- comrad_tbl %>% sim_to_phylo()
  expect_equal(phylo, exptd_phylo)

  # Full, crown
  exptd_phylo <- ape::read.tree(
    text = "((sp1:250, sp3:250):500, sp2:250);"
  )
  phylo <- comrad_tbl %>% sim_to_phylo(include_stem = FALSE)
  expect_equal(phylo, exptd_phylo)

  # Extant, stem
  exptd_phylo <- ape::read.tree(
    text = "((sp1:250, sp3:250):500);"
  )
  phylo <- comrad_tbl %>% sim_to_phylo(with_extinct = FALSE)
  testthat::expect_failure(
    # ape cuts the stem out
    expect_equal(phylo, exptd_phylo)
  )
  # Extant, crown
  exptd_phylo <- ape::read.tree(
    text = "(sp1:250, sp3:250);"
  )
  phylo <- comrad_tbl %>%
    sim_to_phylo(with_extinct = FALSE, include_stem = FALSE)
  expect_equal(phylo, exptd_phylo)
})

testthat::test_that("trait distribution shouldn't matter", {
  if (Sys.getenv("TRAVIS") == "") {
    skip("only run on CI")
  } else {

    #                 |------> sp1
    #           |-----|sp1
    #           |     |------> sp3
    # sp1 >-----|sp1
    #           |     |------>  sp2
    #           |-----|sp2
    #                 |--x sp4
    #
    # with 50 inds/sp/gen & randomly distributed traits

    exptd_phylo <- ape::read.tree(
      text = "(((sp1:500,sp3:500):250,(sp2:500,sp4:250):250):250);"
    )
    gen_seq <- c(
      rep(1:1000, rep(50, 1000)),   # sp1
      rep(250:1000, rep(50, 751)),  # sp2
      rep(500:1000, rep(50, 501)),  # sp3
      rep(500:749, rep(50, 250))    # sp4
    )
    spp_seq <- c(
      rep("sp1", 50000),
      rep("sp2", 37550),
      rep("sp3", 25050),
      rep("sp4", 12500)
    )
    anc_spp_seq <- c(
      rep(as.character(NA), 50000),
      rep("sp1", 37550),
      rep("sp1", 25050),
      rep("sp2", 12500)
    )

    comrad_tbl <- tibble::tibble(
      "t" = as.numeric(gen_seq),
      "z" = as.numeric(NA), # values drawn below
      "species" = spp_seq,
      "ancestral_species" = anc_spp_seq
    )

    purrr::walk(
      1:100,
      function(x) {

        # Draw random values for traits
        comrad_tbl$z <- rnorm(length(gen_seq), 0, 1)

        testthat::expect_silent(
          phylo <- comrad_tbl %>% sim_to_phylo()
        )
        testthat::expect_equal(
          phylo, exptd_phylo
        )
      }
    )
  }
})
