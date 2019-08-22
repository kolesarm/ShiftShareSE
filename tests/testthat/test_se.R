context("Check standard error formulas")

test_that("Homoscedastic and EHW standard errors on ADH data", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"

    ## Compare with stata
    ## readstata13::save.dta13(ADH, file="adh.dta")
    methods <- c("homosk", "ehw", "region_cluster")

    ## First stage
    r1 <- lm(as.formula(paste("shock ~ IV+", ctrls)), data=ADH$reg,
             weights=weights)
    b1 <- reg_ss(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method=methods)
    r2 <- lm(as.formula(paste("shock ~ IV+", ctrls)), data=ADH$reg)
    b2 <- reg_ss(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, method=methods, region_cvar=statefip)

    expect_equal(unname(summary(r1)$coefficients[2, 1:2]),
                 unname(c(b1$beta, b1$se[1])))
    expect_equal(unname(summary(r2)$coefficients[2, 1:2]),
                 unname(c(b2$beta, b2$se[1])))
    ## From stata
    expect_equal(c(.029158413, .171520974, .181839343, .02732516, .087007189,
                   .091423724),
                 unname(c(b2$se[1:3], b1$se[1:3])))

    ## Reduced form
    r3 <- lm(as.formula(paste("d_sh_empl_mfg ~ IV+", ctrls)), data=ADH$reg,
             weights=weights)
    b3 <- reg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method=methods)
    r4 <- lm(as.formula(paste("d_sh_empl_mfg ~ IV+", ctrls)), data=ADH$reg)
    b4 <- reg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   X= IV, data=ADH$reg, region_cvar=statefip,
                   method=methods)

    expect_equal(unname(summary(r3)$coefficients[2, 1:2]),
                 unname(c(b3$beta, b3$se[1])))
    expect_equal(unname(summary(r4)$coefficients[2, 1:2]),
                 unname(c(b4$beta, b4$se[1])))
    ## From stata
    expect_equal(c(.028257944, .048665505, .051901337, .030314902, .050950162,
                   .040030546),
                 unname(c(b4$se[1:3], b3$se[1:3])))

    ## IV
    r5 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                      ctrls, "| IV+", ctrls)),
                   data=ADH$reg, weights=weights)
    b5 <- ivreg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method=methods)
    r6 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                      ctrls, "| IV+", ctrls)), data=ADH$reg)
    b6 <- ivreg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   method=methods)

    ## No small-sample correction, as is default in Stata
    expect_equal(unname(summary(r5)$coefficients[2, 1]), unname(b5$beta))
    expect_equal(unname(summary(r6)$coefficients[2, 1]), unname(b6$beta))
    ## From stata
    expect_equal(c(.0390714, .090163192, .100470868, .053969004,
                   .095215853, .098773877),
                 unname(c(b6$se[1:3], b5$se[1:3])))

    ## RF and IV p-values need to match under the null
    expect_equal(b3$p["AKM0"], b5$p["AKM0"])
    expect_equal(b4$p["AKM0"], b6$p["AKM0"])
})

test_that("AKM and AKM0 standard errors on ADH data", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"

    ## 3-digit cluster and unclustered
    cvar <- floor(ADH$sic/10)
    c1 <- reg_ss(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   sector_cvar=cvar, method="all")
    c3 <- reg_ss(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, sector_cvar=cvar, method="all")
    c5 <- ivreg_ss(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   sector_cvar=cvar, weights=weights, method="all")
    a1 <- reg_ss(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method="all")
    ## manufacturing
    b3 <- reg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   sector_cvar=cvar, weights=weights, method="all")
    b5 <- ivreg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   sector_cvar=cvar, weights=weights, method="all")
    a3 <- reg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all")
    a5 <- ivreg_ss(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all")
    expect_equal(b3$p["AKM0"], b5$p["AKM0"])
    expect_equal(c3$p["AKM0"], c5$p["AKM0"])
    expect_equal(a3$p["AKM0"], a5$p["AKM0"])
    expect_identical(b3$p[1:3], a3$p[1:3])
    expect_identical(b5$p[1:3], a5$p[1:3])

    ## Use Replication package/Paper/Table 5/, and
    ## ShiftShareSEMatlab/ADHapplication.m for non-clustered errors
    est <- c(c1$beta, c3$beta, c5$beta)
    expect_equal(est,
                 c(0.631040938185757, -0.488568717122902, -0.774226658776735))
    expect_true(c1$beta-a1$beta==0)

    ## p-value CI-l, Cl-r for FS, RF, and IV

    ## pvalueFS, CIlFS, CIuFS
    expect_true(max(abs(c1$p[1:3]-a1$p[1:3]))<=0)
    expect_true(max(abs(c1$ci.l[1:3]-a1$ci.l[1:3]))<=0)
    pfs0 <- c(c1$p[-1], a1$p[4:5]) -
        c(0.000000000000408, 0.000000000005114, 0, 0.001282890950014, 0,
          0.000056519706974)
    cilfs0 <- c(c1$ci.l[-1], a1$ci.l[4:5]) -
        c(0.460509980597489, 0.451853732302979, 0.527240172127609,
          0.537570958010291, 0.495188795569771, 0.522017706924849)
    cirfs0 <- c(c1$ci.r[-1], a1$ci.r[4:5]) -
        c(0.801571895774021, 0.810228144068530, 0.734841704243901,
          0.838282656411893, 0.766893080801748, 0.889797428206052)
    expect_lt(max(abs(pfs0)), 1e-13)
    expect_lt(max(abs(cilfs0)), 1e-13)
    expect_lt(max(abs(cirfs0)), 1e-13)
    ## pvalue(1, :, 1), CIl(1, :, 1), CIu(1, :, 1)
    prf0 <- c(c3$p[-1], a3$p[4:5])-
        c(0.000013926848098, 0.000000000114031, 0.002924641237718,
          0.000421803253836, 0.000000150063370, 0.000090458689727)
    cilrf0 <- c(c3$ci.l[-1], a3$ci.l[4:5]) -
        c(-0.708954124424006,  -0.637097672687110, -0.810383925354586,
          -1.236885319089530, -0.516754284756085, -0.629980318872062)
    cirrf0 <- c(c3$ci.r[-1], a3$ci.r[4:5]) -
        c(-0.268183309821816, -0.340039761558712, -0.166753508891236,
          -0.239754057137763, -0.235900929361803, -0.257494610447470)
    expect_lt(max(abs(prf0)), 1e-13)
    expect_lt(max(abs(cirrf0)), 1e-13)
    expect_lt(max(abs(cilrf0)), 3e-13)
    ## pvalue(1, :, 2), CIl(1, :, 2), CIu(1, :, 2)
    ## Small-sample correction
    n <- 1444
    p <- 17
    nc <- 48
    ssc <- sqrt((nc/(nc-1)) * (n-1)/(n-p))
    ssc2 <- sqrt(n / (n - p))

    piv0 <- c(c5$p[4:5], a5$p[4:5]) -
        c(0.001277718157450, 0.000421803253837, 0.000000051567025,
          0.000090458689726)
    ciliv0 <- c(unname(c(c5$beta-qnorm(0.975)*c5$se[2]*ssc2,
                         c5$beta-qnorm(0.975)*c5$se[3]*ssc)),
                c5$ci.l[4:5], a5$ci.l[4:5]) -
        c(-1.099125621404828,  -1.124400324750013, -1.245349169792894,
          -1.690324047107167, -0.810991466572728, -0.891427381124374)
    ciriv0 <- c(unname(c(c5$beta+qnorm(0.975)*c5$se[2]*ssc2,
                         c5$beta+qnorm(0.975)*c5$se[3]*ssc)),
                c5$ci.r[4:5], a5$ci.r[4:5]) -
        c(-0.449327696148525, -0.424052992803340, -0.303104147760458,
          -0.389313219498141, -0.381728638531380, -0.391771441695858)

    expect_lt(max(abs(piv0)), 1e-13)
    expect_lt(max(abs(ciliv0)), 1e-12)
    expect_lt(max(abs(ciriv0)), 3e-12)
})


test_that("AKM0 under weak ID", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"
    iv0 <- ivreg_ss(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W[as.numeric(ADH$reg$division)<8, ],
                    X=IV, data=ADH$reg, region_cvar=statefip,
                    method="akm0",
                    subset=as.numeric(division)<8)
    iv1 <- ivreg_ss(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W[as.numeric(ADH$reg$division)<7, ],
                    X=IV, data=ADH$reg, region_cvar=statefip,
                    method="akm0",
                    subset=as.numeric(division)<7)
    iv2 <- ivreg_ss(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W[as.numeric(ADH$reg$division)<6, ],
                    X=IV, data=ADH$reg, region_cvar=statefip,
                    method="akm0",
                    subset=as.numeric(division)<6)
    expect_equal(unname(c(iv1$se[5], iv2$se[5])), c(Inf, Inf))
    expect_lt(iv1$ci.r[5], iv1$ci.l[5])
    expect_lt(iv0$ci.l[5], iv0$ci.r[5])
    expect_equal(unname(c(iv2$ci.l[5], iv2$ci.r[5])), c(-Inf, Inf))

    r0 <- reg_ss(as.formula(paste("shock ~ ", ctrls)),
             W=ADH$W[as.numeric(ADH$reg$division)>4, ], X=IV,
             data=ADH$reg, weights=weights, region_cvar=statefip,
             method="akm0", subset=as.numeric(division)>4, alpha=0.05)
    expect_lt(r0$ci.l[5], r0$ci.r[5])
    r1 <- reg_ss(as.formula(paste("shock ~ ", ctrls)),
             W=ADH$W[as.numeric(ADH$reg$division)>4, ], X=IV,
             data=ADH$reg, weights=weights, region_cvar=statefip,
             method="akm0", subset=as.numeric(division)>4, alpha=0.045)

    r2 <- reg_ss(as.formula(paste("shock ~ ", ctrls)),
             W=ADH$W[as.numeric(ADH$reg$division)<6, ], X=IV,
             data=ADH$reg, weights=weights, region_cvar=statefip,
             method="akm0", subset=as.numeric(division)<6)
    expect_equal(unname(c(r1$se[5], r2$se[5])), c(Inf, Inf))
    expect_lt(r1$ci.r[5], r1$ci.l[5])
    expect_equal(unname(c(r2$ci.l[5], r2$ci.r[5])), c(-Inf, Inf))

    ## Check it displays properly, and at the same time, check that print
    ## function works properly
    expect2 <- c("Estimate: 0.533953", "", "Inference:",
                 "     Std. Error  p-value Lower CI Upper CI",
                 "AKM0        Inf 0.859052     -Inf      Inf")
    expect1 <-
        c("Estimate: 0.750267", "", "Inference:",
          "     Std. Error   p-value    CI                               ",
          "AKM0        Inf 0.0658442 (-Inf -14.0093 ] + [ -0.564209 Inf )")
    o1 <- utils::capture.output(print(r1, digits=6))
    o2 <- utils::capture.output(print(r2, digits=6))
    ## 1:5 because something weird happens on codecov.io
    expect_equal(o1[1:5], expect1[1:5])
    expect_equal(o2[1:5], expect2[1:5])
})

context("Check warnings")
test_that("Print warning if region_cvar not supplied", {

    expect_warning(reg_ss(d_sh_empl ~ 1, W=ADH$W, X=IV,
                            data=ADH$reg, method="all"))
    expect_warning(ivreg_ss(d_sh_empl ~ 1 | shock, W=ADH$W,
                              X=IV, data=ADH$reg, method="all"))
    expect_warning(reg_ss(d_sh_empl ~ 1, W=ADH$W, X=IV,
                            data=ADH$reg, method="region_cluster"))
    expect_warning(ivreg_ss(d_sh_empl ~ 1 | shock, W=ADH$W,
                            X=IV, data=ADH$reg, method="region_cluster"))

    ## collinear share matrix
    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"
    W <- ADH$W
    W <- cbind(W, W[, 1])
    expect_error(reg_ss(d_sh_empl ~ 1, W=W, X=IV, data=ADH$reg, method="all"))
    expect_error(ivreg_ss(d_sh_empl ~ 1 | shock, W=W,
                          X=IV, data=ADH$reg, method="region_cluster"))


})
