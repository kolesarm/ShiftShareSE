#' Inference in a regression with Bartik structure
#'
#' @template formula
#' @template shocks
#' @inheritParams lmBartik.fit
#' @export
lmBartik <- function(formula, data, subset, weights, Xs, W, method,
                     beta0=0, alpha=0.05, region_cvar=NULL) {

    ## construct model frame
    cl <- mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "region_cvar"),
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

    ret <- lmBartik.fit(y, Xs, W, Z, w, method, beta0, alpha, rc)

    ret$call <- cl
    ret$terms <- mt
    ret
}



#' Inference in a regression with Bartik structure
#'
#' Basic computing engine to calculate, confidence intervals, and p-values in
#' Bartik designs using different inference methods, as specified by
#' \code{method}
#' @param y Outcome variable, vector of length \code{N}
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
lmBartik.fit <- function(y, Xs, W, Z, w=NULL, method=c("akm", "akm0"),
                         beta0=0, alpha=0.05, region_cvar=NULL) {

    X <- drop(W %*% Xs)
    mm <- cbind(X, Z)
    n <- NROW(mm)
    colnames(mm)[1] <- "X"
    r <- if (is.null(w)) stats::lm.fit(mm, y) else stats::lm.wfit(mm, y, w)

    betahat <- unname(r$coefficients[1])
    p <- r$rank
    XXinv <- chol2inv(r$qr$qr[1L:p, 1L:p, drop = FALSE])

    se.h <- se.r <- se.s <- se.akm <- se.akm0 <- NA

    if("all" %in% method)
        method <- c("homosk", "robust", "region_cluster", "akm", "akm0")

    if("homosk" %in% method) {
        rss <- if(is.null(w)) sum(r$residuals^2) else sum(w * r$residuals^2)
        resvar <- rss/r$df.residual
        se.h <- sqrt(XXinv[1, 1] * resvar)
    }

    u <- r$residuals * mm
    if(!is.null(w)) u <- w * u

    if("robust" %in% method)
        se.r <- sqrt((n / (n - r$rank)) *
                     (XXinv %*% crossprod(u) %*% XXinv)[1, 1])

    if("region_cluster" %in% method) {
        nc <- length(unique(region_cvar))      # of clusters
        uu <- apply(u, 2, function(x) tapply(x, factor(region_cvar), sum))
        se.s <- sqrt((nc/(nc-1)) * (n-1)/(n-p) *
                     (XXinv %*% crossprod(uu) %*% XXinv)[1, 1])
    }

    if(("akm" %in% method) | ("akm" %in% method)) {
        if (is.null(w)) {
            ddX <- stats::lm.fit(y=X, x=Z)$residuals # \ddot{X}
            denom <- sum(ddX^2)
            hX <- stats::lm.fit(y=ddX, x=W)$coefficients #  \hat{\Xs}
        } else {
            ddX <- stats::lm.wfit(y=X, x=Z, w=w)$residuals
            denom <- sum(w*ddX^2)
            hX <- stats::lm.wfit(y=ddX, x=W, w=w)$coefficients
        }
    }

    if ("akm" %in% method) {
        u <- r$residuals
        if (!is.null(w)) u <- w*u

        hR <- drop(crossprod(u, W))
        se.akm <- sqrt(sum(hX^2*hR^2))/denom

    }

    cil.akm0 <- cir.akm0 <- NA
    cv <- stats::qnorm(1-alpha/2)

    if ("akm0" %in% method) {
        if (is.null(w)) {
            ## Compute restricted residuals and ddY
            u <- stats::lm.fit(Z, y-X*beta0)$residuals
            ddY <- stats::lm.fit(y=y, x=Z)$residuals # \ddot{Y}
            RY <- sum(ddY*ddX)
            wy <- drop(crossprod(ddY, W))
            wx <- drop(crossprod(ddX, W))
        } else {
            u <- w * stats::lm.wfit(Z, y-X*beta0, w)$residuals
            ddY <- stats::lm.wfit(y=y, x=Z, w=w)$residuals
            RY <- sum(w*ddY*ddX)
            wy <- drop(crossprod(w*ddY, W))
            wx <- drop(crossprod(w*ddX, W))
        }
        hR <- drop(crossprod(u, W))
        se.akm0 <- sqrt(sum(hX^2*hR^2))/denom

        ## Now build CI
        SXY <- sum(hX^2*wy*wx)
        SXX <- sum(hX^2*wx^2)
        SYY <- sum(hX^2*wy^2)

        mid <- (RY*denom-cv^2*SXY)/(denom^2-cv^2*SXX)
        hl <- sqrt( (RY*denom-cv^2*SXY)^2-(denom^2-cv^2*SXX)*(RY^2-cv^2*SYY)) /
            (denom^2-cv^2*SXX)
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



#' @export
print.BartikResults <- function(x, digits = getOption("digits"), ...) {

    fmt <- function(x) format(x, digits=digits, width=digits+1)

    cat("Estimate:", fmt(x$beta))

    r <- cbind("Std. Error"=x$se, "p-value"=x$p, "Lower CI"=x$ci.l,
               "Upper CI"=x$ci.r)
    cat("\n\nInference:\n")
    print(r, digits=digits)

    invisible(x)

}
