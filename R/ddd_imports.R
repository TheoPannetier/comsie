#' Optimizer functions from package DDD
#'
#' See [DDD::optimizer()] documentation. I have frozen argument `optimmethod`
#' to `subplex`, and added an early exit if the first subplex cycle evaluates
#' to `-Inf`.
#'
#' @param optimpars Parameters of the optimization: relative tolerance in
#' function arguments, relative tolerance in function value, absolute tolerance
#' in function arguments, and maximum number of iterations
#' @param num_cycles Number of cycles of the optimization. When set to Inf, the
#' optimization will be repeated until the result is, within the tolerance,
#' equal to the starting values, with a maximum of 10 cycles.
#' @param fun Function to be optimized
#' @param trparsopt Initial guess of the parameters to be optimized
#' @param ... Any other arguments of the function to be optimimzed, or settings
#' of the optimization routine
#' @return \item{out}{ A list containing optimal function arguments
#' (\code{par}, the optimal function value (\code{fvalues}) and whether the
#' optimization converged (\code{conv})}.
#'
#' @export
ddd_optimizer <-  function(optimpars = c(1E-4, 1E-4, 1E-6, 1000),
                           num_cycles = 1,
                           fun,
                           trparsopt,
                           ...) {
  if (num_cycles == Inf) {
    max_cycles <- 10
  } else {
    max_cycles <- num_cycles
  }
  cy <- 1
  fvalue <- rep(-Inf, max_cycles)
  out <- NULL
  while (cy <= max_cycles) {
    if (num_cycles > 1) {
      cat(paste('Cycle ', cy, '\n', sep = ''))
    }
    minfun <- function(fun, trparsopt, ...) {
      return(-fun(trparsopt = trparsopt, ...))
    }
    outnew <- suppressWarnings(subplex::subplex(
      par = trparsopt,
      fn = minfun,
      control = list(
        abstol = optimpars[3],
        reltol = optimpars[1],
        maxit = optimpars[4]
      ),
      fun = fun,
      ...
    ))
    outnew <-  list(
      par = outnew$par,
      fvalues = -outnew$value,
      conv = outnew$convergence
    )

    if (any(is.infinite(outnew$fvalues))) {
      if (cy == 1) {
        cat('First cycle failed; returning results with conv 1.\n')
        outnew$conv <- 1
        return(outnew)
      } else {
        cat('The last cycle failed; second last cycle result is returned.\n')
        return(out)
      }
    }

    if (cy > 1 & (any(is.na(outnew$par)) | any(is.nan(outnew$par)) |
          is.na(outnew$fvalues) |is.nan(outnew$fvalues) | outnew$conv != 0)) {
      cat('The last cycle failed; second last cycle result is returned.\n')
      return(out)
    } else {
      out <- outnew
      trparsopt <- out$par
      fvalue[cy] <- out$fvalues
    }
    if (cy > 1) {
      if (abs(fvalue[cy] - fvalue[cy - 1]) < optimpars[3]) {
        if (cy < max_cycles) {
          cat('No more cycles needed.\n')
        }
        cy <- max_cycles
      } else if (cy == max_cycles) {
        cat('More cycles in optimization recommended.\n')
      }
    }
    cy <- cy + 1
  }
  return(out)
}

#' @name transform_pars
#' @title Transforming parameters from -Inf to Inf into parameters
#' from -1 to 1
#' @description Function to transform pars in a way that is more
#' useful for optimization: trpars <- sign(pars) * pars/(sign(pars) + pars);
#' @param pars Parameters to be transformed
#' @return Transformed parameters
#' @author Rampal S. Etienne
#' @export transform_pars
transform_pars <- function(pars)
{
  trpars1 <- sign(pars) * pars/(sign(pars) + pars);
  trpars1[which(pars == 0)] <- 0;
  trpars1[which(pars == -Inf)] <- -1;
  trpars1[which(pars == Inf)] <- 1;
  return(trpars1);
}

#' @name untransform_pars
#' @title Untransforming parameters from -1 to 1 into parameters
#' from -Inf to Inf.
#' @description Function to untransform pars after optimization:
#' pars <- sign(trpars) * trpars/(sign(trpars) - trpars);
#' @param trpars Parameters to be untransformed
#' @return Untransformed parameters
#' @author Rampal S. Etienne
#' @export untransform_pars
untransform_pars <- function(trpars)
{
  pars <- sign(trpars) * trpars/(sign(trpars) - trpars);
  pars[which(trpars == 0)] <- 0;
  pars[which(trpars == 1)] <- Inf;
  pars[which(trpars == -1)] <- -Inf;
  return(pars)
}
