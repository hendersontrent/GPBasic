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

  if(length(cov_fun) >= 2 && combine_kernels %ni% c("add", "multiply")){
    stop("If multiple covariance functions are listed, combine_kernels must be either 'add' or 'multiply'.")
  }

  if(length(cov_fun) == 1 && combine_kernels != "no"){
    message("A single covariance function was detected. Changing to no kernel combination.")
    combine_kernels <- "no"
  }

  # Check covariance function names and hyperparameter specifications

  for(i in 1:length(cov_fun)){
    if(cov_fun[[i]]$name == "linear"){
      if(names(cov_fun[[i]])[!names(cov_fun[[i]]) == 'name'] != c("sigma", "sigma_b", "c")){
        stop("linear covariance function requires hyperparameters 'sigma', 'sigma_b' and 'c'.")
      }
    } else if(cov_fun[[i]]$name == "se"){
      if(names(cov_fun[[i]])[!names(cov_fun[[i]]) == 'name'] != c("sigma", "l")){
        stop("se covariance function requires hyperparameters 'sigma' and 'l'.")
      }
    } else if(cov_fun[[i]]$name == "periodic"){
      if(names(cov_fun[[i]])[!names(cov_fun[[i]]) == 'name'] != c("sigma", "p", "l")){
        stop("periodic covariance function requires hyperparameters 'sigma', 'p', and 'l'.")
      }
    } else if(cov_fun[[i]]$name == "matern"){
      if(names(cov_fun[[i]])[!names(cov_fun[[i]]) == 'name'] != c("sigma", "l", "nu")){
        stop("matern covariance function requires hyperparameters 'sigma', 'l', and 'nu'.")
      }
    } else if(cov_fun[[i]]$name == "noise"){
      if(names(cov_fun[[i]])[!names(cov_fun[[i]]) == 'name'] != c("sigma")){
        stop("noise covariance function requires hyperparameter 'sigma'.")
      }
    } else{
      stop("An invalid covariance function was found. Covariance functions must be one of 'linear', 'se', 'periodic', 'matern' or 'noise'.")
    }
  }

  # Calculate covariance matrix

  sigmas <- vector(mode = "list", length = length(cov_fun))

  for(i in 1:length(cov_fun)){
    sigmas[[i]] <- calc_cov(X = X, covariance = cov_fun[[i]])
  }

  if(combine_kernels == "no"){
    Sigma <- sigmas[[1]]
  } else if(combine_kernels == "add"){
    Sigma <- Reduce(`+`, sigmas)
  } else{
    Sigma <- Reduce(`%*%`, sigmas)
  }

  #----------------- Fit the GP ----------------

  XX

  #----------------- Final returns ----------------

  gp <- list(mu, K)
  names(gp) <- c("mu", "K")
  class(gp) <- "GP"
  return(gp)
}
