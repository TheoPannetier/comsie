#' Run the simulation
#'
#' Run the competitive radiation simulation.
#'
#' @param path_to_output character, path to save the output file, which must be
#' a `.csv`. If `NULL`, the output is not saved and the final state of the
#' community is returned at the end of the simulation.
#' @param immigration_rate `NA` or a positive number, the rate at which
#' external immigrant populations enter the community.
#' @param nb_gens integer, the number of generations to run the
#' simulation for.
#' @param sampling_on_event logical. If `TRUE`, the community is sampled every
#' time a speciation or extinction happens, and `sampling_freq` is ignored and
#' must be set to `NA`.
#' @param sampling_freq numeric \code{> 0}, the frequency (in generations) at
#' which the community is written to output. See [comrad::set_sampling_freq()] for the
#' default option.
#' @param sampling_frac numeric (between 0 and 1), fraction of the community
#' (in terms of individuals) written to output at every sampled generation. A
#' truncation is operated.
#' @param seed integer \code{> 0}, the seed to set for the random number
#' generator. Defaults to an integer based on current day and time.
#' @inheritParams default_params_doc
#' @param hpc_job_id used to record a job ID in the metadata, only relevant for
#' simulations run on a high-performance cluster. Otherwise takes value
#' `"local"`.
#' @param brute_force_opt a string specifying which brute force option to use
#' to speed up the calculation of competition coefficients. Defaults to "none".
#' Other options are "omp", for multithreading with OpenMP, "simd" for single
#' instruction, multiple data (SIMD) via the C++ library
#' [`xsimd`](https://github.com/xtensor-stack/xsimd); and "simd_omp" for both.
#'
#' @return Returns a table with a row corresponding to each individual, and five
#' columns: `t` is the generation time, `z` the individual's trait value,
#' `species` the name of the species it belongs to, and `ancestral_species` the
#' previous species it descends from.
#' If `path_to_output = NULL`, the community at the last generation is returned.
#' If the path to a `.csv` file is supplied, each sampled generation is appended
#' to the file. In the `.csv`, the table is preceded by some lines of metadata,
#' which are automatically ignored if the file is read with [comrad::read_comrad_tbl()].
#'
#' @author Théo Pannetier
#' @export
#'
run_simulation <- function( # nolint, ignore high cyclomatic complexity
  path_to_output,
  nb_gens,
  immigration_rate = NA,
  mainland_nb_species = comsie::default_mainland_nb_species(),
  mainland_z_sd = comsie::default_mainland_z_sd(),
  growth_rate = comrad::default_growth_rate(),
  competition_sd = comrad::default_competition_sd(),
  carrying_cap_sd = comrad::default_carrying_cap_sd(),
  carrying_cap_opt = comrad::default_carrying_cap_opt(),
  trait_opt = comrad::default_trait_opt(),
  mutation_sd = comrad::default_mutation_sd(),
  trait_dist_sp = comrad::default_trait_dist_sp(),
  sampling_on_event = FALSE,
  sampling_freq = ifelse(
    sampling_on_event, NA, comrad::set_sampling_freq(nb_gens)
  ),
  sampling_frac = comrad::default_sampling_frac(),
  seed = comrad::default_seed(),
  hpc_job_id = NULL,
  brute_force_opt = "none"
) {
  if (!is.null(path_to_output)) {
    if (!is.character(path_to_output)) {
      stop("'path_to_output' must be either null or a character.")
    } else {
      if (!path_to_output %>% stringr::str_detect("\\.csv$")) {
        stop("'path_to_output' must be a .csv")
      }
    }
  }
  comrad::testarg_num(nb_gens)
  comrad::testarg_pos(nb_gens)
  comrad::testarg_not_this(nb_gens, c(0, Inf))
  comrad::testarg_int(nb_gens)
  if (!is.na(immigration_rate)) {
    comrad::testarg_num(immigration_rate)
    comrad::testarg_prop(immigration_rate)
  }
  comrad::testarg_log(sampling_on_event)
  comrad::testarg_num(mainland_z_sd)
  comrad::testarg_pos(mainland_z_sd)
  comrad::testarg_num(mainland_nb_species)
  comrad::testarg_pos(mainland_nb_species)
  comrad::testarg_not_this(mainland_nb_species, c(0, Inf))
  comrad::testarg_int(mainland_nb_species)
  if (sampling_on_event) {
    if (!is.na(sampling_freq)) {
      stop("If \"sampling_on_event\" is TRUE \"sampling_freq\" must be NA.")
    }
  } else {
    comrad::testarg_num(sampling_freq)
    comrad::testarg_int(sampling_freq)
  }
  comrad::testarg_num(seed)
  comrad::testarg_int(seed)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)
  comrad::testarg_num(trait_dist_sp)
  comrad::testarg_pos(trait_dist_sp)
  comrad::testarg_num(sampling_frac)
  comrad::testarg_prop(sampling_frac)
  comrad::testarg_char(brute_force_opt)

  is_on_peregrine <- Sys.getenv("HOSTNAME") == "peregrine.hpc.rug.nl"

  if (is_on_peregrine) {
    if (!is.null(hpc_job_id)) {
      comrad::testarg_num(hpc_job_id)
      comrad::testarg_int(hpc_job_id)
    }
  } else {
    hpc_job_id <- "local"
  }

  # Prepare metadata
  metadata_string <- paste0(
    "### Metadata ###",
    "\nimmigration_rate = ", immigration_rate,
    "\nmainland_nb_species = ", mainland_nb_species,
    "\nmainland_z_sd = ", mainland_z_sd,
    "\ncompetition_sd = ", competition_sd,
    "\ncarrying_cap_sd = ", carrying_cap_sd,
    "\ncarrying_cap_opt = ", carrying_cap_opt,
    "\ntrait_opt = ", trait_opt,
    "\ngrowth_rate = ", growth_rate,
    "\nmutation_sd = ", mutation_sd,
    "\ntrait_dist_sp = ", trait_dist_sp,
    "\n",
    "\nseed = ", seed,
    "\nHPC job ID = ", hpc_job_id,
    "\nsimulated under comsie ", as.character(utils::packageVersion("comsie")),
    "\n", R.version$version.string,
    "\n",
    "\n",
    "\nRunning for ", nb_gens, " generations",
    "\n"
  )
  if (is_on_peregrine) {
    cat(metadata_string)
  }


  set.seed(seed)
  # Draw mainland community
  z_range <- get_viable_z_range(
    pop_size = 10,
    trait_opt = trait_opt,
    carrying_cap_opt = carrying_cap_opt,
    carrying_cap_sd = carrying_cap_sd
  )
  mainland_comm <- create_mainland_comm(
    mainland_nb_species = mainland_nb_species,
    z_range = z_range,
    mainland_z_sd = mainland_z_sd
  )

  if (!is.null(path_to_output)) {
    cat(
      metadata_string,
      "\n### Mainland community ###",
      "\nspecies,mean_z,sd_z\n",
      file = path_to_output
    )
    readr::write_csv(
      mainland_comm,
      file = path_to_output,
      append = TRUE
    )
    cat(
      # Set up output table
      "\n\n### Simulation output ###",
      "\n",
      "\nt,z,species,ancestral_species,root_species\n",
      file = path_to_output,
      append = TRUE
    )
  }

  # Draw ten immigrants from mainland to seed island community
  init_comm <- sample_immigrant_from_mainland(mainland_comm) %>%
    # make 10 copies of that immigrant to make sure init pop survives
    dplyr::slice_sample(n = 10, replace = TRUE) %>%
    dplyr::mutate("t" = 0, .before = 1)

  # Set up data output table proper
  comsie_tbl <- init_comm
  if (!is.null(path_to_output)) {
    readr::write_csv(
      comsie_tbl,
      file = path_to_output,
      append = TRUE
    )
  }

  first_gen <- 0
  time_seq <- (first_gen + 1):(first_gen + nb_gens)
  next_immigration <- ifelse(
    is.na(immigration_rate),
    Inf,
    time_seq[1] + stats::rgeom(1, prob = immigration_rate)
  )

  # Go :)
  for (t in time_seq) {
    cat("\nt =", t)
    # Track changes in community composition
    species_before <- unlist(dplyr::distinct(comsie_tbl, species))

    # Replace current comm with next generation
    comsie_tbl <- dplyr::bind_cols(
      "t" = t,
      draw_comm_next_gen(
        island_comm = comsie_tbl[, c("z", "species", "ancestral_species", "root_species")], # not t
        mainland_comm = mainland_comm,
        growth_rate = growth_rate,
        competition_sd = competition_sd,
        trait_opt = trait_opt,
        carrying_cap_opt = carrying_cap_opt,
        carrying_cap_sd = carrying_cap_sd,
        mutation_sd = mutation_sd,
        trait_dist_sp = trait_dist_sp,
        brute_force_opt = brute_force_opt
      )
    )

    # Resolve immigration if applicable
    if (!is.na(immigration_rate) && t == next_immigration) {
      immigrant_pop <- sample_immigrant_from_mainland(mainland_comm) %>%
        dplyr::mutate("t" = t, .before = 1)
      cat("\nSpecies", unique(immigrant_pop$species), "immigrated on the island.")
      comsie_tbl <- dplyr::bind_rows(comsie_tbl, immigrant_pop)
      next_immigration <- t + (1 + stats::rgeom(1, prob = immigration_rate))
    }

    # Track changes in community composition
    species_after <- unlist(dplyr::distinct(comsie_tbl, species))

    # Resolve whole-community extinction
    if (nrow(comsie_tbl) < 1) {
      cat("\nCommunity has gone extinct at generation", t, "\n")
      if (is.null(path_to_output)) {
        return(comsie_tbl)
      } else {
        return()
      }
    }

    # Sample community to output
    if (sampling_on_event) {
      # Sample if any change in community composition
      sample_this_gen <- !setequal(species_before, species_after) || t == nb_gens
    } else {
      # Sample every sampling_freq generations
      sample_this_gen <- t %% sampling_freq == 0 || t == nb_gens
    }
    if (sample_this_gen) {
      if (!is.null(path_to_output)) {
        saved_seed <- .GlobalEnv$.Random.seed
        # Write only a sample of the output
        sampled_output <- comsie_tbl %>%
          dplyr::slice_sample(prop = sampling_frac)
        # Make sure all species are present in sample
        species_sampled <- unique(sampled_output$species)
        not_sampled <- setdiff(species_after, species_sampled)
        if (length(not_sampled) > 0) {
          for (sp in not_sampled) {
            sampled_output <- dplyr::bind_rows(
              sampled_output,
              comsie_tbl %>% dplyr::filter(species == sp)
            )
          }
        }
        readr::write_csv(
          sampled_output,
          file = path_to_output,
          append = TRUE
        )
        # Restore seed so sampling doesn't change course of simulation
        .GlobalEnv$.Random.seed <- saved_seed
      }
      cat("\nSampled generation", t, "/", time_seq[length(time_seq)])
    }
  }

  if (is.null(path_to_output)) {
    return(comsie_tbl)
  }
}
