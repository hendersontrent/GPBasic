#' Sample from GP posterior using the posterior mean vector and covariance matrix
#'
#' @importFrom MASS mvrnorm
#' @param gp \code{GP} object containing the posterior mean vector and covariance matrix
#' @param n \code{integer} denoting the number of samples to draw. Defaults to \code{5}
#' @param seed \code{integer} denoting a fix for R's pseudorandom number generator. Defaults to \code{123}
#' @return \code{matrix} of samples
#' @author Trent Henderson
#' @export
#'

sample_posterior <- function(gp, n = 5, seed = 123){
  set.seed(seed)
  stopifnot(class(gp) == "GP")
  samples <- MASS::mvrnorm(n = n, mu = gp$mu, Sigma = gp$K)
  return(samples)
}
