#' @param X Shift-share vector with length \code{N} of sectoral shocks,
#'     aggregated to regional level using the share matrix \code{W}. That is,
#'     each element of \code{X} corresponds to a region.
#' @param W A matrix of sector shares, so that \code{W[i, s]} corresponds to
#'     share of sector \code{s} in region \code{i}. The ordering of the regions
#'     must coincide with that in the other inputs, such as \code{X}. The
#'     ordering of the sectors in the columns of \code{W} is irrelevant but the
#'     identity of the sectors in must coincide with those used to construct
#'     \code{X}.
