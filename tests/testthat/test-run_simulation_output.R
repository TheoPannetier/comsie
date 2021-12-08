context("test-run_simulation_output")

temp_path_to_output <- paste0(tempfile("comrad_test_output"), ".csv")
run_simulation(path_to_output = temp_path_to_output, nb_gens = 5)
# Load results in the env
expect_silent(comrad_tbl <- read_comrad_tbl(temp_path_to_output))

test_that("standard_output_file", {
  # Check results format are comrad-standard
  expect_silent(
    comrad_tbl %>%
      dplyr::select("z", "species", "ancestral_species") %>%
      comrad::test_comrad_comm()
  )
})

test_that("test_phylo", {
  # Test phylogeny
  # not a legit phylogeny (1 tip), but the beam though
  # at least provides a check that a phylo object is produced correctly
  spp_tbl <- comrad_tbl %>% comrad::build_spp_tbl()
  expect_equal(
    spp_tbl,
    tibble::tibble(
      "species_name" = "#89ae8a",
      "ancestor_name" = as.character(NA),
      "time_birth" = 0,
      "time_death" = 5
    )
  )
  expect_equal(
    spp_tbl %>% comrad::write_newick_str(),
    "(#89ae8a:5);"
  )
})

# Since we have the data, why not test whether plots can be produced?
test_that("test_plots", {
  expect_true(
    plot_comm_trait_evolution(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_comm_trait_evolution(comrad_tbl, generation_range = c(0, 10)),
    "generation_range is out of the scope of generations in the comrad_tbl."
  )
  expect_true(
    plot_comm_traits(
      comrad_tbl, generation = 1
    ) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_comm_traits(comrad_tbl, generation = 100),
    "Generation 100 wasn't sampled."
  )
  expect_true(
    plot_comm_size(comrad_tbl, which_geom = "area") %>%
      ggplot2::is.ggplot()
  )
  expect_true(
    plot_comm_size(comrad_tbl, which_geom = "line") %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_comm_size(comrad_tbl, colouring = "poor_choice"),
    "'colouring' must be either 'auto' or 'species_names', see doc."
  )
  expect_error(
    plot_comm_size(comrad_tbl, which_geom = "poor_choice"),
    "'which_geom' must be either 'area' or 'line'."
  )
  expect_true(
    plot_comm_bubbles(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_comm_bubbles(comrad_tbl, generation_range = c(0, 10)),
    "generation_range is out of the scope of generations in the comrad_tbl."
  )
  expect_error(
    plot_comm_bubbles(comrad_tbl, xlim = 1:3),
    "custom 'xlim' must be a length-2 numeric vector."
  )
  expect_error(
    plot_comm_bubbles(comrad_tbl, ylim = 5:7),
    "custom 'ylim' must be a length-2 numeric vector."
  )
  expect_true(
    plot_comm_trait_evolution(comrad_tbl, hex_fill = "counts") %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_comm_trait_evolution(comrad_tbl, generation_range = c(0, 10)),
    "generation_range is out of the scope of generations in the comrad_tbl."
  )
  expect_error(
    plot_comm_trait_evolution(comrad_tbl, hex_fill = "poor_choice"),
    "'hex_fill' must be either 'counts' or 'species', see doc."
  )
  expect_error(
    plot_comm_trait_evolution(comrad_tbl, xlim = 1:3),
    "custom 'xlim' must be a length-2 numeric vector."
  )
  expect_error(
    plot_comm_trait_evolution(comrad_tbl, ylim = 5:7),
    "custom 'ylim' must be a length-2 numeric vector."
  )
  # Tailored test for the tiny species filtering option
  comrad_tbl_tiny <- comrad_tbl %>%
    dplyr::bind_rows(
      # Tiny dummy species with 1 ind appears at gen 5
      tibble::tibble(
        "t" = 5,
        "z" = 0,
        "species" = "dummy!",
        "ancestral_species" = "old_dummy!"
      )
    )
  # Check that dummy is excluded
  expect_equivalent(
    plot_comm_trait_evolution(
      comrad_tbl_tiny %>% exclude_ephemeral_spp(),
      hex_fill = "counts"
    ),
    plot_comm_trait_evolution(
      comrad_tbl,
      hex_fill = "counts"
    )
  )
  expect_true(
    plot_fitness_landscape(
      comrad_tbl,
      generation = 1,
      competition_sd = comrad::default_competition_sd(),
      carrying_cap_sd = comrad::default_carrying_cap_sd()
    ) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_fitness_landscape(
      comrad_tbl,
      generation = 100,
      competition_sd = comrad::default_competition_sd(),
      carrying_cap_sd = comrad::default_carrying_cap_sd()
    ),
    "Generation 100 wasn't sampled."
  )
  expect_true(
    plot_fitness_landscape_evol(
      comrad_tbl = comrad_tbl,
      carrying_cap_sd = comrad::default_carrying_cap_sd(),
      competition_sd = comrad::default_competition_sd()
    ) %>%
      class() == "trellis"
  )
  expect_error(
    plot_fitness_landscape_evol(
      comrad_tbl = comrad_tbl,
      generation_range = c(0, 10),
      carrying_cap_sd = comrad::default_carrying_cap_sd(),
      competition_sd = comrad::default_competition_sd()
    ),
    "generation_range is out of the scope of generations in the comrad_tbl."
  )
  expect_true(
    plot_trait_disparity(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )
  expect_true(
    plot_comm_traits_anim(comrad_tbl) %>%
      ggplot2::is.ggplot()
  )
  expect_error(
    plot_comm_traits_anim(comrad::default_init_comm() %>% dplyr::select(-t)),
    "'comrad_tbl' must contain a column 't' with generation times."
  )
  expect_error(
    plot_comm_traits_anim(comrad_tbl, generation_range = c(0, 10)),
    "generation_range is out of the scope of generations in the comrad_tbl."
  )
})

# Done with this tmp file -> thanks -> byyye
unlink(temp_path_to_output)

test_that("read_tbl abuse", {
  expect_error(
    read_comrad_tbl(1313),
    "'path_to_file' must be a character."
  )
  expect_error(
    read_comrad_tbl(character(0)),
    "'path_to_file' is empty"
  )
  expect_error(
    read_comrad_tbl("notacsv"),
    "'path_to_file' must be a .csv"
  )
})

test_that("phylogeny_hoaxids", {
  # Now with a proper phylogeny
  spp_tbl_hoaxids <- tibble::tibble(
    "species_name" = c(
      "Haggis_scoticus", "Dahu_senestris", "Dahu_dextris", "Thylarctos_plumetus"
    ),
    "ancestor_name" = c(
      as.character(NA), "Haggis_scoticus", "Dahu_senestris", "Haggis_scoticus"
    ),
    "time_birth" = c(0, 1, 2, 2),
    "time_death" = c(5, 5, 5, 5)
  )
  expect_silent(
    newick_str <- spp_tbl_hoaxids %>% comrad::write_newick_str()
  )
  # can't test plots without creating unwanted Rplots.pdf in /testthat
})
