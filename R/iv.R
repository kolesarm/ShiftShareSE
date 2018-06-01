#' Inference in an IV regression with Bartik structure
#'
#' @template formulaiv
#' @template shocks
#' @inheritParams ivBartik.fit
#' @export
ivBartik <- function(formula, data, subset, weights, Xs, W, method,
                     beta0=0, alpha=0.05, region_cvar=NULL) {

    ## construct model frame
    cl <- mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "region_cvar"),
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

    ret <- ivBartik.fit(y1, y2, Xs, W, Z, w, method, beta0, alpha, rc)

    ret$call <- cl
    ret$terms <- mt
    ret
}



#' Inference in a regression with Bartik structure
#'
#' Basic computing engine to calculate, confidence intervals, and p-values in
#' Bartik designs using different inference methods, as specified by
#' \code{method}
#' @param y1 Outcome variable, vector of length \code{N}
#' @param y2 Endogenous variable, vector of length \code{N}
#' @param Z Matrix of regional controls, matrix with \code{N} rows
#' @template shocks
#' @param method Vector specifying which inference methods to use:
#'
#' \describe{
#'
#' \item{\code{"homosk"}}{Assume i.i.d. homoskedastic errors}
#'
#' \item{\code{"ehw"}}{Eicker-Huber-White standard errors}
#'
#' \item{\code{"region_cluster"}}{Clustered standard errors at regional level}
#'
#' \item{\code{"akm"}}{Adao-Kolesar-Morales}
#'
#' \item{\code{"akmnull"}}{Adao-Kolesar-Morales with null imposed}
#'
#' \item{\code{"all"}}{All of the methods above}},
#' @param alpha Significance level (confidence intervals will have coverage
#'     \code{1-alpha})
#' @param w vector of weights (length \code{N}) to be used in the fitting
#'     process. If not \code{NULL}, weighted least squares is used with weights
#'     \code{w}, i.e., \code{sum(w * residuals^2)} is minimized.
#' @param beta0 null that is tested (for p-values)
#' @param region_cvar A vector of cluster variables, for method \code{cluster_region}.
#'     If the vector \code{1:N} is used, clustering is effectively equivalent to
#'     \code{ehw}
#' @export
ivBartik.fit <- function(y1, y2, Xs, W, Z, w=NULL, method=c("akm", "akm0"),
                         beta0=0, alpha=0.05, region_cvar=NULL) {

    X <- drop(W %*% Xs)
    mm <- cbind(X, Z)

    r1 <- if (is.null(w)) stats::lm.fit(mm, y1) else stats::lm.wfit(mm, y1, w)
    r2 <- if (is.null(w)) stats::lm.fit(mm, y2) else stats::lm.wfit(mm, y2, w)

    betahat <- unname(r1$coefficients[1]/r2$coefficients[1])
    resid <- r1$residuals-r2$residuals*betahat

    se.h <- se.r <- se.s <- se.akm <- se.akm0 <- NA

    if (is.null(w)) {
        ddX <- stats::lm.fit(y=X, x=Z)$residuals # \ddot{X}
        ddY1 <- stats::lm.fit(y=y1, x=Z)$residuals # \ddot{Y}_{1}
        ddY2 <- stats::lm.fit(y=y2, x=Z)$residuals # \ddot{Y}_{2}
        hX <- stats::lm.fit(y=ddX, x=W)$coefficients #  \hat{\Xs}
    } else {
        ddX <- stats::lm.wfit(y=X, x=Z, w=w)$residuals
        ddY1 <- stats::lm.wfit(y=y1, x=Z, w=w)$residuals
        ddY2 <- stats::lm.wfit(y=y2, x=Z, w=w)$residuals
        hX <- stats::lm.wfit(y=ddX, x=W, w=w)$coefficients
    }
    wgt <- if (!is.null(w)) w else 1
    RY <- sum(wgt * ddY1*ddX)
    RX <- sum(wgt * ddY2*ddX)

    if("all" %in% method)
        method <- c("homosk", "robust", "region_cluster", "akm", "akm0")

    if("homosk" %in% method) {
        rss <- sum(wgt * resid^2)
        ## no N/(N-p) small-sample correction in IV
        den <- sum(wgt * ddX^2)
        se.h <- sqrt(rss/NROW(Z)) / (sqrt(den)*abs(r2$coefficients[1]))
    }

    u <- wgt * resid * ddX

    if("robust" %in% method)
        se.r <- sqrt(drop(crossprod(u)) / RX^2)

    if("region_cluster" %in% method)
        se.s <- sqrt(drop(crossprod(tapply(u, factor(region_cvar), sum))) /
                     RX^2)

    if ("akm" %in% method) {
        hR <- drop(crossprod(wgt * resid, W))
        se.akm <- sqrt(sum(hX^2*hR^2) / RX^2)
    }

    cil.akm0 <- cir.akm0 <- NA
    cv <- stats::qnorm(1-alpha/2)

    if ("akm0" %in% method) {
        hR <- drop(crossprod(wgt * (ddY1-ddY2*beta0), W))
        se.akm0 <- sqrt(sum(hX^2*hR^2) / RX^2)

        ## Now build CI
        wy <- drop(crossprod(wgt * ddY1, W))
        wx <- drop(crossprod(wgt * ddY2, W))
        SXY <- sum(hX^2*wy*wx)
        SXX <- sum(hX^2*wx^2)
        SYY <- sum(hX^2*wy^2)

        mid <- (RY*RX-cv^2*SXY)/(RX^2-cv^2*SXX)
        hl <- sqrt( (RY*RX-cv^2*SXY)^2-(RX^2-cv^2*SXX)*(RY^2-cv^2*SYY)) /
            (RX^2-cv^2*SXX)
        cir.akm0 <- mid+hl
        cil.akm0 <- mid-hl
    }

    se <- c(se.h, se.r, se.s, se.akm)
    p <- 2*(1-stats::pnorm(abs(betahat-beta0)/c(se, se.akm0)))
    ci.l <- c(betahat-cv*se, cil.akm0)
    ci.r <- c(betahat+cv*se, cir.akm0)
    se <- c(se, (ci.r[5]-ci.l[5]) / (2*cv))
    names(se) <- names(ci.l) <- names(ci.r) <- names(p) <-
        c("Homoscedastic", "EHW", "Reg. cluster", "AKM", "AKM0")

    structure(list(beta=betahat, se=se,
                   p=p, ci.l=ci.l, ci.r=ci.r), class="BartikResults")
}
