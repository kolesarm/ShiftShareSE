context("Replicate ADH (2013)")

test_that("Point estimates match Table 3", {

    ur <- function(d, r=7) round(unname(d), r)

    ## list d_sh_empl d_sh_empl_mfg d_sh_empl_nmfg d_tradeusch_pw
    ## d_tradeotch_pw_lag timepwt48 in 1/5
    ## Also, increase precision in some entries of
    ## AHD13 to handle better rounding in R4.0.0
    ADH13 <- rbind(c(.1587391, -3.8059785, 3.964716, 5.293786, 2.278828,
                     .0021135),
                   c(2.535355, -.681448, 3.216803, 3.030479, 2.7977347,
                     .0007317),
                   c(-3.124082, -2.1658, -.9582818, 2.062596, .7209086,
                     .0002608))
    expect_equal(ur(as.matrix(ADH$reg[1:3, 1:6]), r=6), ur(ADH13, r=6))

    ## No weights, Table 3 (1) and (6), second and first stage
    ## d_tradeotc.. |   .8671441   .1302015     6.66 [.6117393,   1.122549]
    ## d_t~eusch_pw |  -.6215758   .1630867    -3.81 [-.9412199, -.3019317]
    ## d_tradeotc.. |   .7462433   .1842344     4.05 [ .3848439, 1.107643]
    ## d_t~eusch_pw |  -.3028266   .1004709    -3.01 [-.4997459, -.1059073]
    ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c +
              l_sh_popfborn + l_sh_empl_f + l_sh_routine33 + l_task_outsource +
              division"

    iv1 <- AER::ivreg(d_sh_empl_mfg ~ shock + t2 | IV + t2,
                      data=ADH$reg)
    fs1 <- lm(shock ~ IV + t2, data=ADH$reg)
    expect_equal(ur(iv1$coefficients[2]), -.6215758)
    expect_equal(ur(fs1$coefficients[2]), .8671441)

    fs2 <- lm(as.formula(paste("shock ~ IV+", ctrls)), data=ADH$reg)
    iv2 <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                       ctrls, "| IV+", ctrls)), data=ADH$reg)
    expect_equal(ur(fs2$coefficients[2]), .7462433)
    expect_equal(ur(iv2$coefficients[2]), -0.3028266)

    ## With weights
    ## d_tradeotc.. |   .7916466    .078037    10.14     [.6385683     .944725]
    ## d_t~eusch_pw |  -.7460301   .0680391   -10.96    [-.8793842   -.6126759]
    ## d_tradeotc.. |   .6310409   .0900049     7.01 [.4544848,    .8075971]
    ## d_t~eusch_pw |  -.5963601   .0987739    -6.04 [ -.7899533, -.4027668]

    iv1w <- AER::ivreg(d_sh_empl_mfg ~ shock + t2 | IV + t2,
                       weights=weights, data=ADH$reg)
    fs1w <- lm(shock + t2 ~ IV + t2, weights=weights, data=ADH$reg)
    expect_equal(ur(iv1w$coefficients[2]), -.7460301)
    expect_equal(ur(fs1w$coefficients[2]), .7916466)
    fs2w <- lm(as.formula(paste("shock ~ IV+", ctrls)),
               weights=weights, data=ADH$reg)
    iv2w <- AER::ivreg(as.formula(paste("d_sh_empl_mfg ~ shock+",
                                        ctrls, "| IV+", ctrls)),
                       weights=weights, data=ADH$reg)
    expect_equal(ur(iv2w$coefficients[2]), -.5963601)
    expect_equal(ur(fs2w$coefficients[2]), .6310409)
})
