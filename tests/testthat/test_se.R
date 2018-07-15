context("Check standard error formulas")


test_that("Homoscedastic and EHW standard errors on ADH data", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"
    ADH$reg$X <- ADH$W %*% ADH$sec$X

    ## Compare with stata
    ## readstata13::save.dta13(ADH, file="adh.dta")
    methods <- c("homosk", "ehw", "region_cluster")

    ## First stage
    r1 <- lm(as.formula(paste("shock ~ X+", ctrls)), data=ADH$reg,
             weights=weights)
    b1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, Xs=ADH$sec$X,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method=methods)
    r2 <- lm(as.formula(paste("shock ~ X+", ctrls)), data=ADH$reg)
    b2 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, Xs=ADH$sec$X,
                   data=ADH$reg, method=methods, region_cvar=statefip)

    expect_equal(unname(summary(r1)$coefficients[2, 1:2]),
                 unname(c(b1$beta, b1$se[1])))
    expect_equal(unname(summary(r2)$coefficients[2, 1:2]),
                 unname(c(b2$beta, b2$se[1])))
    ## From stata
    expect_equal(c(.040996684, .239267122, .259477455, .038851385,
                   .120113015, .127405531),
                 unname(c(b2$se[1:3], b1$se[1:3])))

    ## Reduced form
    r3 <- lm(as.formula(paste("d_sh_empl_mfg ~ X+", ctrls)), data=ADH$reg,
             weights=weights)
    b3 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   Xs=ADH$sec$X, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method=methods)
    r4 <- lm(as.formula(paste("d_sh_empl_mfg ~ X+", ctrls)), data=ADH$reg)
    b4 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   Xs= ADH$ sec$ X, data=ADH$reg, region_cvar=statefip,
                   method=methods)

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
                   data=ADH$reg, weights=weights)
    b5 <- ivBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, Xs=ADH$sec$X, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method=methods)
    r6 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                      ctrls, "| X+", ctrls)), data=ADH$reg)
    b6 <- ivBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, Xs=ADH$sec$X, data=ADH$reg, region_cvar=statefip,
                   method=methods)

    ## No small-sample correction, as is default in Stata
    expect_equal(unname(summary(r5)$coefficients[2, 1]), unname(b5$beta))
    expect_equal(unname(summary(r6)$coefficients[2, 1]), unname(b6$beta))
    ## From stata
    expect_equal(c(.039343218, .084235939, .095823617, .054857207,
                   .097547805, .09957397),
                 unname(c(b6$se[1:3], b5$se[1:3])))

    ## RF and IV p-values need to match under the null
    expect_equal(b3$p["AKM0"], b5$p["AKM0"])
    expect_equal(b4$p["AKM0"], b6$p["AKM0"])
})

test_that("AKM and AKM0 standard errors on ADH data", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"
    Xs <- ADH$sec$X
    ## 1. No residual sector
    ## First stage
    b1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, Xs=Xs,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method="all")
    ## Reduced form
    b3 <- lmBartik(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   Xs=Xs, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all")
    ## IV
    b5 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                   W=ADH$W, Xs=Xs, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all")

    ## From rodrigo

    ## p-values:
    expect_equal(unname(b1$p[-1]),
                 c(0.000000000000435207425653061, 0.00000000000852184989241778,
                   0, 0.0000821954647416412))
    ## CI: RF
    expect_equal(unname(c(b3$ci.l[-c(1, 5)], round(b3$ci.l[5], 2))),
                 c(-0.967346848113025, -0.868235840622573,
                   -1.07651432083069, -1.45))
    expect_equal(unname(c(b3$ci.r[-c(1, 5)], round(b3$ci.r[5], 2))),
                 c(-0.345460968982221, -0.444571976472673,
                   -0.236293496264559, -0.32))
    ## CI: FS
    expect_equal(unname(c(b1$ci.l[-c(1, 5)], round(b1$ci.l[5], 2))),
                 c(0.634697101293926, 0.620404032001181,
                   0.68165700538552, 0.71))
    expect_equal(unname(c(b1$ci.r[-c(1, 5)], round(b1$ci.r[5], 2))),
                 c(1.10553146809938, 1.11982453739213,
                   1.05857156400779, 1.21))
    ## CI: IV
    expect_equal(unname(c(b5$ci.r[4], round(b5$ci.r[-c(1, 4)], 2))),
                 c(-0.335051871169529, -0.42, -0.40, -0.39))
    expect_equal(unname(c(b5$ci.l[4], round(b5$ci.l[-c(1, 4)], 2))),
                 c(-1.17372443578747, -1.09, -1.11, -1.40))

    ## RF and IV p-values need to match under the null
    expect_equal(b3$p["AKM0"], b5$p["AKM0"])

    ## Table 6: residual sector, and also clustering
    a1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, Xs=Xs,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method="all", residual_sector=TRUE)
    a3 <- lmBartik(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   Xs=Xs, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all", residual_sector=TRUE)
    a5 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                   W=ADH$W, Xs=Xs, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all", residual_sector=TRUE)
    ## 3-digit cluster
    cvar <- floor(ADH$sec$sic/10)
    c1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, Xs=Xs,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   sector_cvar=cvar, method="all", residual_sector=TRUE)
    c3 <- lmBartik(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   Xs=Xs, data=ADH$reg, region_cvar=statefip,
                   weights=weights, sector_cvar=cvar, method="all",
                   residual_sector=TRUE)
    c5 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")), W=ADH$W,
                   Xs=Xs, data=ADH$reg, region_cvar=statefip,
                   sector_cvar=cvar, weights=weights, method="all",
                   residual_sector=TRUE)
    c45 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W, Xs=Xs, data=ADH$reg, region_cvar=statefip,
                    sector_cvar=ADH$sec$sic, weights=weights, method="all",
                    residual_sector=TRUE)
    expect_equal(a3$p["AKM0"], a5$p["AKM0"])
    expect_equal(c3$p["AKM0"], c5$p["AKM0"])

    ## First-stage
    pr <- c(0.0000000000435207425653061, 0.000000000852184989241778,
            0,  0.00875147510965846,  0,  0.143127216829275)
    cir <- c(1.10553146809937, 1.11982453739212, 1.0603377820805,
             1.21385357311323, 1.0262765784931, 1.17197912208833)
    cil <- c(0.634697101293923, 0.620404032001176, 0.679890787312798,
             0.710340933073309, 0.713951990900194, 0.724475715586453)
    expect_equal(pr, unname(c(a1$p[-1], c1$p[4:5])*100))
    expect_equal(cir, unname(c(a1$ci.r[-1], c1$ci.r[4:5])))
    expect_equal(cil, unname(c(a1$ci.l[-1], c1$ci.l[4:5])))

    ## Reduced form
    pr <- c(0.00351102540439019, 0.000000125274324247471, 0.25580195590813,
            0.0247021115306945, 0.501528499038328, 0.0704212346456457)
    cir <- c(-0.345460968982168, -0.444571976472602, -0.229891759780033,
             -0.317452085930679, -0.197920330308309, -0.300338520376155)
    cil <- c(-0.967346848112947, -0.868235840622513, -1.08291605731508,
             -1.47097695120726, -1.11488748678681, -1.7206801861689)
    expect_equal(pr, unname(c(a3$p[-1], c3$p[4:5])*100))
    expect_equal(cir, unname(c(a3$ci.r[-1], c3$ci.r[4:5])))
    expect_equal(cil, unname(c(a3$ci.l[-1], c3$ci.l[4:5])))
    ## IV
    pr <- c(0.00126578872070571, 0.00415356406215928, 0.0549930277330191,
            0.0247021115306501, 0.0687236705003036, 0.0192738973482154,
            0.254004409578279, 0.0704212346455346)
    cil <- c(-1.09304937526038, -1.1151243239479, -1.18231798707372,
             -1.41231665617235, -1.18995183560929, -1.4728245147438,
             -1.24422052335195, -1.7182355293984)
    cir <- c(-0.415726931696755, -0.393651983009237, -0.326458319883414,
             -0.383458906476736, -0.31882447134785, -0.387982867640542,
             -0.264555783605192, -0.354229553701598)
    expect_equal(pr[-(1:2)],
                 unname(c(a5$p[-1], c45$p[4:5], c5$p[4:5])*100)[-(1:2)])
    expect_equal(cil[-(1:2)],
                 unname(c(a5$ci.l[-1], c45$ci.l[4:5], c5$ci.l[4:5]))[-(1:2)])
    expect_equal(cir[-(1:2)],
                 unname(c(a5$ci.r[-1], c45$ci.r[4:5], c5$ci.r[4:5]))[-(1:2)])
    ## Small-sample correction
    n <- 1444;p <- 17;nc <- 48;
    ssc <- sqrt((nc/(nc-1)) * (n-1)/(n-p))
    expect_equal(c(cil[2], cir[2]), unname(c(a5$beta-qnorm(0.975)*a5$se[3]*ssc,
                                      a5$beta+qnorm(0.975)*a5$se[3]*ssc)))
    ssc <- sqrt(n / (n - p))
    expect_equal(c(cil[1], cir[1]), unname(c(a5$beta-qnorm(0.975)*a5$se[2]*ssc,
                                      a5$beta+qnorm(0.975)*a5$se[2]*ssc)))
})
