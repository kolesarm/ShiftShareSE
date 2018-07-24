#' @section Note: \code{subset} is evaluated in the same way as variables in
#'     \code{formula}, that is first in \code{data} and then in the environment
#'     of \code{formula}.

#' @param formula object of class \code{"formula"} (or one that can be coerced
#'     to that class) of the form \code{outcome ~ controls}
#' @param data optional data frame, list or environment (or object coercible by
#'     \code{as.data.frame} to a data frame) containing the outcome and running
#'     variables in the model. If not found in \code{data}, the variables are
#'     taken from \code{environment(formula)}, typically the environment from
#'     which the function is called.
#' @param subset optional vector specifying a subset of observations to be used
#'     in the fitting process.
#' @param weights an optional vector of weights to be used in the fitting
#'     process. Should be \code{NULL} or a numeric vector. If non-\code{NULL},
#'     weighted least squares is used with weights \code{weights} (that is,
#'     minimizing \code{sum(weights*residuals^2)}); otherwise ordinary least
#'     squares is used.
