context("Check standard error formulas")


test_that("Homoscedastic and EHW standard errors on ADH data", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"
    ADH$X <- ADH_W %*% ADH_Xs

    ## Compare with stata
    ## readstata13::save.dta13(ADH, file="adh.dta")

    ## First stage
    r1 <- lm(as.formula(paste("shock ~ X+", ctrls)), data=ADH, weights=weights)
    b1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH_W, Xs=ADH_Xs,
                   data=ADH, weights=weights, region_cvar=statefip,
                   method=c("homosk", "robust", "region_cluster", "akm"))
    r2 <- lm(as.formula(paste("shock ~ X+", ctrls)), data=ADH)
    b2 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH_W, Xs=ADH_Xs,
                   data=ADH, method=c("homosk", "robust", "region_cluster", "akm"),
                   region_cvar=statefip)

    expect_equal(unname(summary(r1)$coefficients[2, 1:2]),
                 unname(c(b1$beta, b1$se[1])))
    expect_equal(unname(summary(r2)$coefficients[2, 1:2]),
                 unname(c(b2$beta, b2$se[1])))
    ## From stata
    expect_equal(c(.040996684, .239267122, .259477455, .038851385,
                   .120113015, .127405531),
                 unname(c(b2$se[1:3], b1$se[1:3])))

    ## Reduced form
    r3 <- lm(as.formula(paste("d_sh_empl_mfg ~ X+", ctrls)), data=ADH,
             weights=weights)
    b3 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH_W,
                   Xs=ADH_Xs, data=ADH, region_cvar=statefip,
                   weights=weights,
                   method=c("homosk", "robust", "region_cluster", "akm"))
    r4 <- lm(as.formula(paste("d_sh_empl_mfg ~ X+", ctrls)), data=ADH)
    b4 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH_W,
                   Xs=ADH_Xs, data=ADH, region_cvar=statefip,
                   method=c("homosk", "robust", "region_cluster", "akm"))

    expect_equal(unname(summary(r3)$coefficients[2, 1:2]),
                 unname(c(b3$beta, b3$se[1])))
    expect_equal(unname(summary(r4)$coefficients[2, 1:2]),
                 unname(c(b4$beta, b4$se[1])))
    ## From stata
    expect_equal(c(.039646104, .06338974, .06683718, .04297627,
                   .074301104, .059640098),
                 unname(c(b4$se[1:3], b3$se[1:3])))

    ## TODO: IV

})
