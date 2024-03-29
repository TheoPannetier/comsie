% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_comsie_spp_tbl.R
\name{build_comsie_spp_tbl}
\alias{build_comsie_spp_tbl}
\title{Extracts phylogenetic information from a \code{comsie} simulation}
\usage{
build_comsie_spp_tbl(comsie_tbl)
}
\value{
a tibble with four columns and a row per species in the simulation
\itemize{
\item \code{species_name}, name of the species
\item \code{ancestor_name}, name of the ancestral species from which it branched. \code{NA}
for the initial species.
\item \code{time_birth}, the generation at which the species appeared.
\item \code{time_death}, the generation at which the last individual died. Last
generation if the species was still extant at the end of the simulation.
}
}
\description{
Reads the output of a \code{comsie} simulation and creates a table containing
the necessary information about every species to make a phylogeny.
}
\author{
Théo Pannetier
}
