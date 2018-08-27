#' Describe function
#'
#' Quickly calculate poission dispersion statistic from a GLM object
#' @param glm_fit.  An object of the type (glm,..,family = "poisson")
#' @keywords dispersion, pearson, residuals
#' @export
#' @examples
#' poisson_dispersion_statistic(glm_fit)


poisson_dispersion_statistic <- function(glm_fit){
  pearson_chi2 <- sum(residuals(glm_fit, type = "pearson")**2)
  return(round(pearson_chi2/glm_fit$df.residual,2)) ##Dispersion statistic
}