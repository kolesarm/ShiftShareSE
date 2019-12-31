#' Inference in an IV regression with a shift-share instrument
#'
#' Computes confidence intervals and p-values in an instrumental variables
#' regression in which the instrument has a shift-share structure, as in Bartik
#' (1991). Several different inference methods can computed, as specified by
#' \code{method}.
#' @template formulaiv
#' @template shocks
#' @template method
#' @template value
#' @references{
#'
#' \cite{Bartik, Timothy J., Who Benefits from State and Local Economic
#' Development Policies?, Kalamazoo, MI: W.E. Upjohn Institute for Employment
#' Research, 1991.}
#'
#' \cite{Adão, Rodrigo, Kolesár, Michal, and Morales, Eduardo,
#' "Shift-Share Designs: Theory and Inference", Quarterly Journal of Economics
#' 2019, 134 (4), 1949-2010.}
#'
#' }
#' @examples
#' ## Use ADH data from Autor, Dorn, and Hanson (2013)
#' ivreg_ss(d_sh_empl ~ 1 | shock, X=IV, data=ADH$reg, W=ADH$W,
#'          method=c("ehw", "akm", "akm0"))
#' @export
ivreg_ss <- function(formula, X, data, W, subset, weights, method, beta0=0,
                     alpha=0.05, region_cvar=NULL, sector_cvar=NULL) {

    ## construct model frame
    cl <- mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "X", "data", "subset", "weights", "region_cvar"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    formula <- Formula::as.Formula(formula)
    stopifnot(length(formula)[1] == 1L, length(formula)[2] == 2)
    mf$formula <- formula

    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())

    y1 <- stats::model.response(mf, "numeric")
    w <- as.vector(stats::model.weights(mf))
    rc <- mf$"(region_cvar)"

    if (!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")

    mt <- stats::terms(formula, data = data)
    mtZ <- stats::terms(formula, data = data, rhs = 1)
    Z <- if (stats::is.empty.model(mtZ)) NULL
         else stats::model.matrix(mtZ, mf, contrasts=NULL)

    mty2 <- stats::delete.response(stats::terms(formula, data = data, rhs = 2))
    attr(mty2, "intercept") <- 0
    y2 <- drop(stats::model.matrix(mty2, mf, contrasts=NULL))

    ret <- ivreg_ss.fit(y1, y2, mf$"(X)", W, Z, w, method, beta0, alpha, rc,
                        sector_cvar)
    ret$call <- cl
    ret$terms <- mt
    ret
}



#' Inference in an IV regression with a shift-share instrument
#'
#' Basic computing engine to calculate confidence intervals and p-values in an
#' instrumental variables regression with a shift-share instrument, using
#' different inference methods, as specified by \code{method}.
#' @param y1 Outcome variable. A vector of length \code{N}, with each row
#'     corresponding to a region.
#' @param y2 Endogenous variable, vector of length \code{N}, with each row
#'     corresponding to a region.
#' @param Z Matrix of regional controls, matrix with \code{N} rows corresponding
#'     to regions.
#' @template shocks
#' @template method
#' @template value
#' @param w vector of weights (length \code{N}) to be used in the fitting
#'     process. If not \code{NULL}, weighted least squares is used with weights
#'     \code{w}, i.e., \code{sum(w * residuals^2)} is minimized.
#' @export
ivreg_ss.fit <- function(y1, y2, X, W, Z, w=NULL, method=c("akm", "akm0"),
                         beta0=0, alpha=0.05, region_cvar=NULL,
                         sector_cvar=NULL) {

    mm <- cbind(X, Z)

    if (is.null(w)) {
        ddX <- stats::lm.fit(y=X, x=Z)$residuals # \ddot{X}
        ddY1 <- stats::lm.fit(y=y1, x=Z)$residuals # \ddot{Y}_{1}
        ddY2 <- stats::lm.fit(y=y2, x=Z)$residuals # \ddot{Y}_{2}
        hX <- stats::lm.fit(y=ddX, x=W)$coefficients #  \hat{\Xs}
        wgt <- 1
        r1 <- stats::lm.fit(mm, y1)
        r2 <- stats::lm.fit(mm, y2)
    } else {
        ddX <- stats::lm.wfit(y=X, x=Z, w=w)$residuals
        ddY1 <- stats::lm.wfit(y=y1, x=Z, w=w)$residuals
        ddY2 <- stats::lm.wfit(y=y2, x=Z, w=w)$residuals
        hX <- stats::lm.wfit(y=ddX, x=W, w=w)$coefficients
        wgt <- w
        r1 <- stats::lm.wfit(mm, y1, w)
        r2 <- stats::lm.wfit(mm, y2, w)
    }

    betahat <- unname(r1$coefficients[1]/r2$coefficients[1])
    resid <- r1$residuals-r2$residuals*betahat

    se.h <- se.r <- se.s <- se.akm <- se.akm0 <- NA

    if (qr(W)$rank < ncol(W))
        stop("Share matrix is collinear")

    RX <- sum(wgt * ddY2*ddX)

    if("all" %in% method)
        method <- c("homosk", "ehw", "region_cluster", "akm", "akm0")

    if("homosk" %in% method) {
        rss <- sum(wgt * resid^2)
        ## no N/(N-p) small-sample correction in IV
        den <- sum(wgt * ddX^2)
        se.h <- sqrt(rss/NROW(Z)) / (sqrt(den)*abs(r2$coefficients[1]))
    }

    u <- wgt * resid * ddX
    if("ehw" %in% method)
        se.r <- sqrt(drop(crossprod(u)) / RX^2)

    if("region_cluster" %in% method)
        if (is.null(region_cvar))
            warning(paste0("Reporting NA for \"region_cluster\" Std. Error",
                           " because \"region_cvar\" not supplied."))
        else
            se.s <- sqrt(drop(crossprod(tapply(u, factor(region_cvar), sum))) /
                         RX^2)

    if ("akm" %in% method | "akm0" %in% method) {
        cR <- hX*drop(crossprod(wgt * resid, W))
        cR0 <- hX*drop(crossprod(wgt * (ddY1-ddY2*beta0), W))
        cW <- hX*drop(crossprod(wgt * ddY2, W))
        if (!is.null(sector_cvar)) {
            if (length(sector_cvar) != length(cR))
                stop("The length of \"sector_cvar\" is different",
                        "from the number of sectors.")
            cR <- tapply(cR, factor(sector_cvar), sum)
            cR0 <- tapply(cR0, factor(sector_cvar), sum)
            cW <- tapply(cW, factor(sector_cvar), sum)
        }
    }

    if ("akm" %in% method)
        se.akm <- sqrt(sum(cR^2) / RX^2)

    se0.akm0 <- cil.akm0 <- cir.akm0 <- NA
    cv <- stats::qnorm(1-alpha/2)

    if ("akm0" %in% method) {
        se0.akm0 <- sqrt(sum(cR0^2) / RX^2)

        ## Now build CI
        Q <- RX^2/cv^2 - sum(cW^2)
        Q2 <- sum(cR*cW) / Q
        mid <- betahat -  Q2
        dis <- Q2^2 + sum(cR^2) / Q

        if (Q>0) {
            cir.akm0 <- mid + sqrt(dis)
            cil.akm0 <- mid - sqrt(dis)
            se.akm0 <- sqrt(dis)/cv
        } else if (dis >0)  {
            cir.akm0 <- mid - sqrt(dis)
            cil.akm0 <- mid + sqrt(dis)
            se.akm0 <- Inf
        } else  {
            cir.akm0 <- Inf
            cil.akm0 <- -Inf
            se.akm0 <- Inf
        }
    }

    se <- c(se.h, se.r, se.s, se.akm, se.akm0)
    p <- 2*(1-stats::pnorm(abs(betahat-beta0)/c(se[-5], se0.akm0)))
    ci.l <- c(betahat-cv*se[-5], cil.akm0)
    ci.r <- c(betahat+cv*se[-5], cir.akm0)

    names(se) <- names(ci.l) <- names(ci.r) <- names(p) <-
        c("Homoscedastic", "EHW", "Reg. cluster", "AKM", "AKM0")

    structure(list(beta=betahat, se=se,
                   p=p, ci.l=ci.l, ci.r=ci.r), class="SSResults")
}
