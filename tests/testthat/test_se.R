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
                   data=ADH, weights=weights, region_cvar=statefip, method="all")
    r2 <- lm(as.formula(paste("shock ~ X+", ctrls)), data=ADH)
    b2 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH_W, Xs=ADH_Xs,
                   data=ADH, method="all", region_cvar=statefip)

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
                   weights=weights, method="all")
    r4 <- lm(as.formula(paste("d_sh_empl_mfg ~ X+", ctrls)), data=ADH)
    b4 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH_W,
                   Xs=ADH_Xs, data=ADH, region_cvar=statefip, method="all")

    expect_equal(unname(summary(r3)$coefficients[2, 1:2]),
                 unname(c(b3$beta, b3$se[1])))
    expect_equal(unname(summary(r4)$coefficients[2, 1:2]),
                 unname(c(b4$beta, b4$se[1])))
    ## From stata
    expect_equal(c(.039646104, .06338974, .06683718, .04297627,
                   .074301104, .059640098),
                 unname(c(b4$se[1:3], b3$se[1:3])))

    ## IV
    r5 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                      ctrls, "| X+", ctrls)),
                   data=ADH, weights=weights)
    b5 <- ivBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH_W, Xs=ADH_Xs, data=ADH, region_cvar=statefip,
                   weights=weights, method="all")
    r6 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                      ctrls, "| X+", ctrls)), data=ADH)
    b6 <- ivBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH_W, Xs=ADH_Xs, data=ADH, region_cvar=statefip,
                   method="all")

    ## No small-sample correction, as is default in Stata
    expect_equal(unname(summary(r5)$coefficients[2, 1]), unname(b5$beta))
    expect_equal(unname(summary(r6)$coefficients[2, 1]), unname(b6$beta))
    ## ## From stata
    expect_equal(c(.039343218, .084235939, .095823617, .054857207,
                   .097547805, .09957397),
                 unname(c(b6$se[1:3], b5$se[1:3])))



})
