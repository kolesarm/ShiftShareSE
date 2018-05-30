#' Inference in a regression with Bartik structure
#'
#' Basic computing engine to calculate, confidence intervals, and p-values in
#' Bartik designs using different inference methods, as specified by
#' \code{method}
#' @param y Outcome variable, vector of length \code{N}
#' @param Z Matrix of regional controls, matrix with \code{N} rows
#' @param Xs Sector-level shocks, vector of length \code{S}
#' @param W matrix of sectoral weights with dimensions \code{N}-by-\code{S}
#' @param method Vector specifying which inference methods to use:
#'
#' \describe{
#'
#' \item{\code{"homosk"}}{Assume i.i.d. homoskedastic errors}
#'
#' \item{\code{"ehw"}}{Eicker-Huber-White standard errors}
#'
#' \item{\code{"cluster"}}{Clustered standard errors}
#'
#' \item{\code{"akm"}}{Adao-Kolesar-Morales}
#'
#' \item{\code{"akmnull"}}{Adao-Kolesar-Morales with null imposed}},
#' @param CI For method \code{akmnull} compute confidence
#'     intervals by inverting the tests?
#' @param clustervar A vector of cluster variables, for method \code{cluster}.
#'     If the vector \code{1:N} is used, clustering is effectively equivalent to
#'     \code{ehw}
#' @export
lmBartik.fit <- function(y, Xs, W, Z, beta0=0, CI=FALSE, alpha=0.05,
                         clustervar=NULL,
                         method=c("akm", "akm0")) {


ADH$X <- drop(ADH_W %*% ADH_Xs)
ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource + division"


## Xs <- ADH_Xs
## W <- ADH_W
Z <- cbind(1, ADH$t2)
y <- ADH$d_sh_empl_mfg
r1 <- lm(as.formula(paste("d_sh_empl_mfg ~ X+", ctrls)), data=ADH)
Z <- model.matrix(r1)



    r <- lm(y~0+I(W %*% Xs) + Z)


    betahat <- unname(r$coefficients[2])
    nc <- length(unique(clustervar))      # of clusters

    se.h <- se.r <- se.s <- NA
    ff <- list(p=NA, CI=c(NA, NA))
    if("fisher" %in% method) {
        ff <- FisherP(y, wt, X, beta0=beta0, nperm=nperm, CI=CI,
                      LastShockZero=LastShockZero, alpha=alpha)
    }
    if("homosk" %in% method) se.h <- summary(r)$coefficients[2, 2]
    if("robust" %in% method) {
        se.r <- sqrt(sandwich::vcovHC(r, type="HC1")[2, 2])
    }
    if("cluster" %in% method) {
        se.s <- multiwayvcov::cluster.vcov(r, clustervar,
                                           df_correction=FALSE)[2, 2]
        se.s <- sqrt((nc/(nc-1)) * (nrow(wt)-1)/(nrow(wt)-r$rank) * se.s)

    }

    cv <- qnorm(1-alpha/2)
    se <- c(se.h, se.r, se.s)
    p <- c(2*(1-pnorm(abs(betahat-0)/se)), ff$p)
    ci.l <- c(betahat-cv*se, ff$CI[1])
    ci.r <- c(betahat+cv*se, ff$CI[2])
    se <- c(se, (ff$CI[2]-ff$CI[1])/(2*cv))
    names(se) <- names(ci.l) <- names(ci.r) <- names(p) <-
        c("Homoscedastic", "EHW", "Cluster", "Fisher")

    structure(list(beta=betahat, se=se,
                   p=p, ci.l=ci.l, ci.r=ci.r), class="BartikResults")

}



#' Inference in Bartik designs
#'
#' Compute standard erorrs, confidence intervals, and p-values in Bartik designs
#' using different inference methods, as specified by \code{method}
#'
#' @inheritParams FisherP
BartikSE <-   {

}

print.BartikResults <- function(x, digits = getOption("digits"), ...) {

    fmt <- function(x) format(x, digits=digits, width=digits+1)

    cat("Estimate:", fmt(x$beta))

    r <- cbind("Std. Error"=x$se, "p-value"=x$p, "Lower CI"=x$ci.l,
               "Upper CI"=x$ci.r)
    cat("\n\nInference:\n")
    print(r, digits=digits)

    invisible(x)

}
