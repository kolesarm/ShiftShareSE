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
    b1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method=methods)
    r2 <- lm(as.formula(paste("shock ~ IV+", ctrls)), data=ADH$reg)
    b2 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
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
    b3 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method=methods)
    r4 <- lm(as.formula(paste("d_sh_empl_mfg ~ IV+", ctrls)), data=ADH$reg)
    b4 <- lmBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls)), W=ADH$W,
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
    b5 <- ivBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method=methods)
    r6 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                      ctrls, "| IV+", ctrls)), data=ADH$reg)
    b6 <- ivBartik(as.formula(paste("d_sh_empl_mfg ~ ", ctrls, "| shock")),
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
    ## 1. No residual sector: TODO check R's code with new ADH data
    ## ## First stage
    ## b1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
    ##                data=ADH$reg, weights=weights, region_cvar=statefip,
    ##                method="all")
    ## Reduced form
    b3 <- lmBartik(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all")
    ## IV
    b5 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all")

    ## ## From rodrigo

    ## ## p-values:
    ## expect_equal(unname(b1$p[-1]),
    ##           c(0.000000000000435207425653061, 0.00000000000852184989241778,
    ##                0, 0.0000821954647416412))
    ## ## CI: RF
    ## expect_equal(unname(c(b3$ci.l[-c(1, 5)], round(b3$ci.l[5], 2))),
    ##              c(-0.967346848113025, -0.868235840622573,
    ##                -1.07651432083069, -1.45))
    ## expect_equal(unname(c(b3$ci.r[-c(1, 5)], round(b3$ci.r[5], 2))),
    ##              c(-0.345460968982221, -0.444571976472673,
    ##                -0.236293496264559, -0.32))
    ## ## CI: FS
    ## expect_equal(unname(c(b1$ci.l[-c(1, 5)], round(b1$ci.l[5], 2))),
    ##              c(0.634697101293926, 0.620404032001181,
    ##                0.68165700538552, 0.71))
    ## expect_equal(unname(c(b1$ci.r[-c(1, 5)], round(b1$ci.r[5], 2))),
    ##              c(1.10553146809938, 1.11982453739213,
    ##                1.05857156400779, 1.21))
    ## ## CI: IV
    ## expect_equal(unname(c(b5$ci.r[4], round(b5$ci.r[-c(1, 4)], 2))),
    ##              c(-0.335051871169529, -0.42, -0.40, -0.39))
    ## expect_equal(unname(c(b5$ci.l[4], round(b5$ci.l[-c(1, 4)], 2))),
    ##              c(-1.17372443578747, -1.09, -1.11, -1.40))

    ## RF and IV p-values need to match under the null
    expect_equal(b3$p["AKM0"], b5$p["AKM0"])

    ## Simulations with US CZs/results_20180707.xlsx Table 6: residual
    ## sector, and also clustering
    a1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   method="all", residual_sector=TRUE)
    a3 <- lmBartik(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all", residual_sector=TRUE)
    a5 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                   W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, method="all", residual_sector=TRUE)
    ## 3-digit cluster
    cvar <- floor(ADH$sic/10)
    c1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
                   data=ADH$reg, weights=weights, region_cvar=statefip,
                   sector_cvar=cvar, method="all", residual_sector=TRUE)
    c3 <- lmBartik(as.formula(paste("d_sh_empl ~ ", ctrls)), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   weights=weights, sector_cvar=cvar, method="all",
                   residual_sector=TRUE)
    c5 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")), W=ADH$W,
                   X=IV, data=ADH$reg, region_cvar=statefip,
                   sector_cvar=cvar, weights=weights, method="all",
                   residual_sector=TRUE)
    c45 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W, X=IV, data=ADH$reg, region_cvar=statefip,
                    sector_cvar=ADH$sic, weights=weights, method="all",
                    residual_sector=TRUE)
    expect_equal(a3$p["AKM0"], a5$p["AKM0"])
    expect_equal(c3$p["AKM0"], c5$p["AKM0"])

    ## First-stage
    pr <- c(0.0000000000408340028457133, 0.00000000051139092960284, 0,
            0.00616158044735027, 0, 0.131003064802671)
    cir <- c(0.801571895773998, 0.810228144068509, 0.768456991583788,
             0.89616408526085, 0.734654399949352,  0.837791645754481)
    cil <- c(0.460509980597516, 0.451853732303005, 0.493624884787726,
             0.52121433426454, 0.527427476422162, 0.537540779056916)
    expect_equal(pr, unname(c(a1$p[-1], c1$p[4:5])*100))
    expect_equal(cir, unname(c(a1$ci.r[-1], c1$ci.r[4:5])))
    expect_equal(cil, unname(c(a1$ci.l[-1], c1$ci.l[4:5])))

    ## Reduced form
    pr <- c(0.00139268480980892, 0.000000011403056277004, 0.194793162123164,
            0.0109671394048494, 0.322105438136999, 0.0395206084935484)
    cir <- c(-0.268183309821802, -0.340039761558695, -0.179479273328377,
             -0.248312181652566, -0.163505116644179, -0.238657191113524)
    cil <- c(-0.708954124424002, -0.637097672687109, -0.797658160917427,
             -1.10172812169929, -0.813632317601625, -1.24990650017766)
    expect_equal(pr, unname(c(a3$p[-1], c3$p[4:5])*100))
    expect_equal(cir, unname(c(a3$ci.r[-1], c3$ci.r[4:5])))
    expect_equal(cil, unname(c(a3$ci.l[-1], c3$ci.l[4:5])))
    ## IV
    pr <- c(0.000300396525521052, 0.00146797690656619, 0.0317455114638321,
            0.0109671394048494, 0.0362744461506637, 0.00947847746970432,
            0.155981411934847, 0.0395206084935484)
    cir <- c(-0.449327696148537, -0.424052992803294, -0.352783981257112,
             -0.412527307627578, -0.348668495840406, -0.418641064338139,
             -0.294522760136827, -0.385713006721926)
    cil <- c(-1.09912562140493, -1.12440032475018, -1.19566933629636,
             -1.41976305923031, -1.19978482171306, -1.47638570382798,
             -1.25393055741664, -1.71727689062525)
    expect_equal(pr[-(1:2)],
                 unname(c(a5$p[-1], c45$p[4:5], c5$p[4:5])*100)[-(1:2)])
    expect_equal(cil[-(1:2)],
                 unname(c(a5$ci.l[-1], c45$ci.l[4:5], c5$ci.l[4:5]))[-(1:2)])
    expect_equal(cir[-(1:2)],
                 unname(c(a5$ci.r[-1], c45$ci.r[4:5], c5$ci.r[4:5]))[-(1:2)])
    ## Small-sample correction
    n <- 1444
    p <- 17
    nc <- 48
    ssc <- sqrt((nc/(nc-1)) * (n-1)/(n-p))
    expect_equal(c(cil[2], cir[2]), unname(c(a5$beta-qnorm(0.975)*a5$se[3]*ssc,
                                      a5$beta+qnorm(0.975)*a5$se[3]*ssc)))
    ssc <- sqrt(n / (n - p))
    expect_equal(c(cil[1], cir[1]), unname(c(a5$beta-qnorm(0.975)*a5$se[2]*ssc,
                                      a5$beta+qnorm(0.975)*a5$se[2]*ssc)))
})


test_that("AKM0 under weak ID", {

    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
          l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
          division"
    iv0 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W[as.numeric(ADH$reg$division)<8, ],
                    X=IV, data=ADH$reg, region_cvar=statefip,
                    method="akm0", residual_sector=TRUE,
                    subset=as.numeric(division)<8)
    iv1 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W[as.numeric(ADH$reg$division)<7, ],
                    X=IV, data=ADH$reg, region_cvar=statefip,
                    method="akm0", residual_sector=TRUE,
                    subset=as.numeric(division)<7)
    iv2 <- ivBartik(as.formula(paste("d_sh_empl ~ ", ctrls, "| shock")),
                    W=ADH$W[as.numeric(ADH$reg$division)<6, ],
                    X=IV, data=ADH$reg, region_cvar=statefip,
                    method="akm0", residual_sector=TRUE,
                    subset=as.numeric(division)<6)
    expect_equal(unname(c(iv1$se[5], iv2$se[5])), c(Inf, Inf))
    expect_lt(iv1$ci.r[5], iv1$ci.l[5])
    expect_lt(iv0$ci.l[5], iv0$ci.r[5])
    expect_equal(unname(c(iv2$ci.l[5], iv2$ci.r[5])), c(-Inf, Inf))

    r0 <- lmBartik(as.formula(paste("shock ~ ", ctrls)),
             W=ADH$W[as.numeric(ADH$reg$division)>4, ], X=IV,
             data=ADH$reg, weights=weights, region_cvar=statefip,
             method="akm0", subset=as.numeric(division)>4, alpha=0.05)
    expect_lt(r0$ci.l[5], r0$ci.r[5])
    r1 <- lmBartik(as.formula(paste("shock ~ ", ctrls)),
             W=ADH$W[as.numeric(ADH$reg$division)>4, ], X=IV,
             data=ADH$reg, weights=weights, region_cvar=statefip,
             method="akm0", subset=as.numeric(division)>4, alpha=0.045)

    r2 <- lmBartik(as.formula(paste("shock ~ ", ctrls)),
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
    expect1 <- c("Estimate: 0.750267", "", "Inference:",
                 "     Std. Error   p-value  Lower CI Upper CI",
                 "AKM0        Inf 0.0658442 -0.564209 -14.0093")
    o1 <- utils::capture.output(print(r1, digits=6))
    o2 <- utils::capture.output(print(r2, digits=6))
    expect_equal(o1, expect1)
    expect_equal(o2, expect2)
})

context("Check warnings")
test_that("Print warning if region_cvar not supplied", {

    expect_warning(lmBartik(d_sh_empl ~ 1, W=ADH$W, X=IV,
                            data=ADH$reg, method="all"))
    expect_warning(ivBartik(d_sh_empl ~ 1 | shock, W=ADH$W,
                              X=IV, data=ADH$reg, method="all"))
    expect_warning(lmBartik(d_sh_empl ~ 1, W=ADH$W, X=IV,
                            data=ADH$reg, method="region_cluster"))
    expect_warning(ivBartik(d_sh_empl ~ 1 | shock, W=ADH$W,
                              X=IV, data=ADH$reg, method="region_cluster"))
})
