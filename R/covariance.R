#--------------------------------------------------------
#--------------------- Define kernels -------------------
#--------------------------------------------------------

#' Linear kernel function
#'
#' @param x1 \code{numeric} vector of values for first dataset
#' @param x2 \code{numeric} vector of values for second dataset
#' @param sigma \code{numeric} scalar denoting hyperparameter for average distance from the mean. Defaults to \code{0.3}
#' @param sigma_b \code{numeric} scalar denoting hyperparameter for certainty around C. Defaults to \code{0.8}
#' @param c \code{numeric} scalar denoting hyperparameter for offset. Defaults to \code{0}
#' @return \code{numeric} vector of covariance values
#' @author Trent Henderson
#' @export
#'

linear_cov <- function(x1, x2, sigma = 0.3, sigma_b = 0.8, c = 0) {
  stopifnot(sigma > 0)
  stopifnot(sigma_b > 0)
  return(sigma_b ^ 2 + sigma ^ 2 * (x1 - c) * (x2 - c))
}


#' Squared exponential kernel function (radial basis function)
#'
#' @param x1 \code{numeric} vector of values for first dataset
#' @param x2 \code{numeric} vector of values for second dataset
#' @param sigma \code{numeric} scalar denoting hyperparameter for average distance from the mean. Defaults to \code{0.3}
#' @param l \code{numeric} scalar denoting hyperparameter for reach of influence on neighbouring points. Defaults to \code{0.8}
#' @return \code{numeric} vector of covariance values
#' @author Trent Henderson
#' @export
#'

se_cov <- function(x1, x2, sigma = 0.3, l = 0.8) {
  stopifnot(sigma > 0)
  dist_squared <- sum((x1 - x2) ^ 2)
  return(sigma ^ 2 * exp(-dist_squared / (2 * l ^ 2)))
}

#' Periodic kernel function
#'
#' @param x1 \code{numeric} vector of values for first dataset
#' @param x2 \code{numeric} vector of values for second dataset
#' @param sigma \code{numeric} scalar denoting hyperparameter for average distance from the mean. Defaults to \code{0.3}
#' @param p \code{numeric} scalar denoting hyperparameter for distance between repetitions. Defaults to \code{0.8}
#' @param l \code{numeric} scalar denoting hyperparameter for reach of influence on neighbouring points. Defaults to \code{0.5}
#' @return \code{numeric} vector of covariance values
#' @author Trent Henderson
#' @export
#'

periodic_cov <- function(x1, x2, sigma = 0.3, p = 0.8, l = 0.5) {
  stopifnot(sigma > 0)
  dist_squared <- sum((sin(pi * abs(x1 - x2) / p)) ^ 2)
  return(sigma ^ 2 * exp(-2 * dist_squared / (l ^ 2)))
}

#' White noise kernel function
#'
#' @param x1 \code{numeric} vector of values for first dataset
#' @param x2 \code{numeric} vector of values for second dataset
#' @param sigma \code{numeric} scalar denoting hyperparameter for average distance from the mean. Defaults to \code{0.3}
#' @return \code{numeric} vector of covariance values
#' @author Trent Henderson
#' @export
#'

noise_cov <- function(x1, x2, sigma = 0.3) {
  stopifnot(sigma > 0)
  if (x1 == x2) {
    return(sigma ^ 2)
  } else {
    return(0)
  }
}

#' Matern kernel function
#'
#' @param x1 \code{numeric} vector of values for first dataset
#' @param x2 \code{numeric} vector of values for second dataset
#' @param sigma \code{numeric} scalar denoting hyperparameter for average distance from the mean. Defaults to \code{0.3}
#' @param l \code{numeric} scalar denoting hyperparameter for reach of influence on neighbouring points. Defaults to \code{0.8}
#' @param nu \code{numeric} scalar denoting hyperparameter for smoothness. Defaults to \code{1.5}
#' @return \code{numeric} vector of covariance values
#' @author Trent Henderson
#' @export
#'

matern_cov <- function(x1, x2, sigma = 0.3, l = 0.8, nu = 1.5) {
  stopifnot(sigma > 0)
  dist <- sqrt(sum((x1 - x2) ^ 2))
  term1 <- ((2 ^ (nu - 1)) / gamma(nu)) * ((sqrt(2 * nu) * dist) / l) ^ nu
  term2 <- besselK(sqrt(2 * nu) * dist / l, nu)
  return(sigma ^ 2 * term1 * term2)
}
