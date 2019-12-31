#' @return Returns an object of class \code{"SSResults"} containing the
#'     estimation and inference results. The \code{print} function can be used
#'     to print a summary of the results. The object is a list with at least the
#'     following components: \describe{
#'
#' \item{beta}{Point estimate of the effect of interest \eqn{\beta}{beta}}
#'
#' \item{se, p}{A vector of standard errors and a vector of p-values of the null
#' \eqn{H_{0}\colon \beta = \beta_{0}}{H_0 : beta = beta0} for the inference
#' methods in \code{method}, with \eqn{\beta_{0}}{beta0} specified by the
#' argument \code{beta0}. For the method \code{"akm0"}, the standard error
#' corresponds to the effective standard error (length of the confidence
#' interval divided by \code{2*stats::qnorm(1-alpha/2)})}
#'
#' \item{ci.l, ci.r}{Upper and lower endpoints of the confidence interval for
#' the effect of interest \eqn{\beta}{beta}, for each of the methods in
#' \code{method}}
#'
#' }
