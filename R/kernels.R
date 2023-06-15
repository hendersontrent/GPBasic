#' Helper function for kernel names and their hyperparameter names
#'
#' @return \code{data.frame} of kernel function names and corresponding hyperparameter names to assist with using the \code{fit_gp} function
#' @author Trent Henderson
#' @export
#'

kernels <- function(){

  kerns <- data.frame(name = c("linear", "se", "periodic", "matern", "noise"),
                      full_name = c("linear", "squared exponential (radial basis function)", "periodic", "Matern", "White noise"),
                      hyperparameters = c("sigma, sigma_b, c", "sigma, l", "sigma, p, l", "sigma, l, nu", "sigma"))

  return(kerns)
}
