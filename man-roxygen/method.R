#' @param method Vector specifying which inference methods to use. The vector
#'     elements have to be one or more of the following strings:
#'
#' \describe{
#'
#' \item{\code{"homosk"}}{Assume i.i.d. homoskedastic errors}
#'
#' \item{\code{"ehw"}}{Eicker-Huber-White standard errors}
#'
#' \item{\code{"region_cluster"}}{Standard errors clustered at regional level}
#'
#' \item{\code{"akm"}}{Adão-Kolesár-Morales}
#'
#' \item{\code{"akm0"}}{Adão-Kolesár-Morales with null imposed. Note the
#' reported standard error for this method corresponds to the normalized
#' standard error, given by the length of the confidence interval divided by
#' \eqn{2z_{1-\alpha/2}}{2z_{1-alpha/2}}}
#'
#' \item{\code{"all"}}{All of the methods above}}
#' @param beta0 null that is tested (only affects reported p-values)
#' @param region_cvar A vector with length \code{N} of cluster variables, for
#'     method \code{"cluster_region"}. If the vector \code{1:N} is used,
#'     clustering is effectively equivalent to \code{ehw}
#' @param sector_cvar A vector with length \code{S} of cluster variables, if
#'     sectors are to be clustered, for methods \code{"akm"} and \code{"akm0"}.
#'     If the vector \code{1:S} is used, this is equivalent to not clustering.
#' @param alpha Determines confidence level of reported confidence intervals,
#'     which will have coverage \code{1-alpha}.
