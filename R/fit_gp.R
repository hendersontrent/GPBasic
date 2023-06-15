#' Fit a Gaussian process model
#'
#' @param X \code{matrix} or \code{vector} of numerical input values. Vectors will be coerced to matrices inside the function
#' @param y \code{vector} of output values
#' @param cov_fun \code{list} of lists containing the covariance functions to use and their respective hyperparameters. Each covariance function should have its own list, complete with values for "name" and the hyperparameters, e.g., \code{list(list(name = "se", sigma = 0.3, l = 0.8), list(name = "periodic", sigma = 0.3, p = 0.8, l = 0.5))}. Defaults to \code{list(list(name = "se", sigma = 0.3, l = 0.8))}
#' @param combine_kernels \code{character} denoting whether to combine covariance functions if multiple were specified. Can be one of \code{"no"} for no combining (only works for single function specification), \code{"add"} or \code{"multiply"}
#' @param optimise \code{Boolean} whether to optimise hyperparameters using maximum likelihood estimation. Defaults to \code{FALSE}
#' @param normalise \code{Boolean} whether to z-score the input data prior to fitting the GP. Defaults to \code{FALSE}
#' @return \code{GP} class object containing the posterior mean vector and posterior covariance matrix
#' @author Trent Henderson
#' @export
#'

fit_gp <- function(X, y, cov_fun = list(list(name = "se", sigma = 0.3, l = 0.8)),
                   combine_kernels = c("no", "add", "multiply"),
                   optimise = FALSE, normalise = FALSE){

  combine_kernels <- match.arg(combine_kernels)

  #----------------- Set up everything ----------------

  # Scale inputs if specified

  if(normalise){
    X <- scale(X, center = TRUE, scale = TRUE)
  }

  #----------------- Prior covariance function ----------------

  # Check covariance function arguments

  XX

  # Produce final prior covariance matrix

  XX

  #----------------- Fit the GP ----------------

  XX

  #----------------- Final returns ----------------

  gp <- list(mu, K)
  names(gp) <- c("mu", "K")
  class(gp) <- "GP"
  return(gp)
}
