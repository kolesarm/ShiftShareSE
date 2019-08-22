#' Inference in linear regression with a shift-share regressor
#'
#' Computes confidence intervals and p-values in a linear regression in which
#' the regressor of interest has a shift-share structure, as the instrument in
#' Bartik (1991). Several different inference methods can computed, as specified
#' by \code{method}.
#'
#' @template formula
#' @template shocks
#' @template method
#' @template value
#' @inheritParams reg_ss.fit
#' @references{
#'
#' \cite{Bartik, Timothy J., Who Benefits from State and Local Economic
#' Development Policies?, Kalamazoo, MI: W.E. Upjohn Institute for Employment
#' Research, 1991.}
#'
#' \cite{Adão, Rodrigo, Kolesár, Michal, and Morales, Eduardo,
#' "Shift-Share Designs: Theory and Inference", Quarterly Journal of Economics
#' 2019, forthcoming. doi:10.1093/qje/qjz025}
#'
#' }
#' @examples
#' ## Use ADH data from Autor, Dorn, and Hanson (2013)
#' reg_ss(d_sh_empl ~ 1, X=IV, data=ADH$reg, W=ADH$W,
#'          method=c("ehw", "akm", "akm0"))
#' @export
reg_ss <- function(formula, X, data, W, subset, weights, method, beta0=0,
                     alpha=0.05, region_cvar=NULL, sector_cvar=NULL) {

    ## construct model frame
    cl <- mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "X", "data", "subset", "weights", "region_cvar"),
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())

    y <- stats::model.response(mf, "numeric")
    w <- as.vector(stats::model.weights(mf))
    rc <- mf$"(region_cvar)"

    if (!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")
    mt <- attr(mf, "terms")

    Z <- if (stats::is.empty.model(mt)) NULL
         else stats::model.matrix(mt, mf, contrasts=NULL)

    ret <- reg_ss.fit(y, mf$"(X)", W, Z, w, method, beta0, alpha, rc,
                        sector_cvar)

    ret$call <- cl
    ret$terms <- mt
    ret
}


#' Inference in a shift-share regression
#'
#' Basic computing engine to calculate confidence intervals and p-values in
#' shift-share designs using different inference methods, as specified by
#' \code{method}.
#' @param y Outcome variable, vector of length \code{N}
#' @param Z Matrix of regional controls, matrix with \code{N} rows corresponding
#'     to regions.
#' @template shocks
#' @template method
#' @template value
#' @param w vector of weights (length \code{N}) to be used in the fitting
#'     process. If not \code{NULL}, weighted least squares is used with weights
#'     \code{w}, i.e., \code{sum(w * residuals^2)} is minimized.
#' @export
reg_ss.fit <- function(y, X, W, Z, w=NULL, method=c("akm", "akm0"), beta0=0,
                         alpha=0.05, region_cvar=NULL, sector_cvar=NULL) {
    mm <- cbind(X, Z)
    r <- if (is.null(w)) stats::lm.fit(mm, y) else stats::lm.wfit(mm, y, w)
    betahat <- unname(r$coefficients[1])
    n <- NROW(mm)
    p <- r$rank

    se.h <- se.r <- se.s <- se.akm <- se.akm0 <- NA

    if (qr(W)$rank < ncol(W))
        stop("Share matrix is collinear")

    if("all" %in% method)
        method <- c("homosk", "ehw", "region_cluster", "akm", "akm0")

    if (is.null(w)) {
        ddX <- stats::lm.fit(y=X, x=Z)$residuals # \ddot{X}
        ddY <- stats::lm.fit(y=y, x=Z)$residuals # \ddot{Y}
        hX <- stats::lm.fit(y=ddX, x=W)$coefficients #  \hat{\Xs}
    } else {
        ddX <- stats::lm.wfit(y=X, x=Z, w=w)$residuals
        ddY <- stats::lm.wfit(y=y, x=Z, w=w)$residuals
        hX <- stats::lm.wfit(y=ddX, x=W, w=w)$coefficients
    }



    wgt <- if (is.null(w)) 1 else w
    RX <- sum(wgt * ddX^2)

    if("homosk" %in% method) {
        resvar <- sum(wgt * r$residuals^2) / r$df.residual
        se.h <- sqrt(resvar / RX)
    }

    u <- wgt * r$residuals * ddX
    if("ehw" %in% method)
        se.r <- sqrt((n / (n - p)) * drop(crossprod(u))) / RX

    if("region_cluster" %in% method) {
        if (is.null(region_cvar))
            warning(paste0("Reporting NA for \"region_cluster\" Std. Error",
                           " because \"region_cvar\" not supplied."))
        else {
            nc <- length(unique(region_cvar))      # # of clusters
            se.s <-
                sqrt((nc/(nc-1)) * (n-1)/(n-p) *
                     drop(crossprod(tapply(u, factor(region_cvar), sum)))) / RX
        }
    }

    if ("akm" %in% method | "akm0" %in% method) {
        cR <- hX*drop(crossprod(wgt * r$residuals, W))
        cR0 <- hX*drop(crossprod(wgt * (ddY-ddX*beta0), W))
        cW <- hX*drop(crossprod(wgt * ddX, W))
        if (!is.null(sector_cvar)) {
            cR <- tapply(cR, factor(sector_cvar), sum)
            cR0 <- tapply(cR0, factor(sector_cvar), sum)
            cW <- tapply(cW, factor(sector_cvar), sum)
        }
    }

    if ("akm" %in% method)
        se.akm <- sqrt(sum(cR^2)) / RX

    cil.akm0 <- cir.akm0 <- NA
    cv <- stats::qnorm(1-alpha/2)

    if ("akm0" %in% method) {
        se.akm0 <- sqrt(sum(cR0^2)) / RX

        ## Now build CI
        Q <- RX^2/cv^2 - sum(cW^2)
        Q2 <- sum(cR*cW)/Q
        mid <- betahat -  Q2
        dis <- Q2^2 + sum(cR^2) / Q

        if (Q>0) {
            cir.akm0 <- mid + sqrt(dis)
            cil.akm0 <- mid - sqrt(dis)
        } else if (dis >0)  {
            cir.akm0 <- mid - sqrt(dis)
            cil.akm0 <- mid + sqrt(dis)
        } else  {
            cir.akm0 <- Inf
            cil.akm0 <- -Inf
        }
    }

    se <- c(se.h, se.r, se.s, se.akm)
    p <- 2*(1-stats::pnorm(abs(betahat-beta0)/c(se, se.akm0)))
    ci.l <- c(betahat-cv*se, cil.akm0)
    ci.r <- c(betahat+cv*se, cir.akm0)
    se.akm0 <- if (is.na(ci.l[5]) | ci.r[5]>ci.l[5])
                   (ci.r[5]-ci.l[5]) / (2*cv)
               else
                   Inf
    se <- c(se, se.akm0)
    names(se) <- names(ci.l) <- names(ci.r) <- names(p) <-
        c("Homoscedastic", "EHW", "Reg. cluster", "AKM", "AKM0")

    structure(list(beta=betahat, se=se,
                   p=p, ci.l=ci.l, ci.r=ci.r), class="SSResults")
}


#' @export
print.SSResults <- function(x, digits = getOption("digits"), ...) {

    fmt <- function(x) format(x, digits=digits, width=digits+1)

    cat("Estimate:", fmt(x$beta))
    s <- !is.na(x$se)


    r <- cbind("Std. Error"=x$se[s], "p-value"=x$p[s], "Lower CI"=x$ci.l[s],
               "Upper CI"=x$ci.r[s])
    cat("\n\nInference:\n")
    if (sum(r[, 3]<r[, 4]) > 0)
        print(r[r[, 3]<r[, 4], , drop=FALSE], digits=digits) # nolint
    ## AKM0 in format (-Inf, U) \cup (L, Inf)
    if (sum(r[, 3]>r[, 4]) > 0) {
        r2 <- r[r[, 3]>r[, 4], , drop=FALSE] # nolint
        r2 <- data.frame(r2[, 1:2, drop=FALSE],
                         "(-Inf", r2[, 4], "] + [", r2[, 3], "Inf )")
        names(r2) <- c(colnames(r)[1:2], "CI", rep("", 4))
        print(r2, digits=digits)
    }

    invisible(x)
}
