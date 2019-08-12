## ---- include=FALSE, cache=FALSE-----------------------------------------
library("knitr")
knitr::opts_knit$set(self.contained = FALSE)
knitr::opts_chunk$set(tidy = TRUE, collapse=TRUE, comment = "#>",
                      tidy.opts=list(blank=FALSE, width.cutoff=55))

## ------------------------------------------------------------------------
library("ShiftShareSE")
ctrls <- "t2 + l_shind_manuf_cbp + l_sh_popedu_c + l_sh_popfborn +
          l_sh_empl_f + l_sh_routine33 + l_task_outsource + division"
sic <- floor(ADH$sic/10)

## ------------------------------------------------------------------------
reg_ss(as.formula(paste("shock ~ ", ctrls)), W=ADH$W, X=IV,
               data=ADH$reg, weights=weights, region_cvar=statefip,
               sector_cvar=sic, method="all")

## ------------------------------------------------------------------------
reg_ss(as.formula(paste("d_sh_empl ~", ctrls)), W=ADH$W, X=IV,
                  data=ADH$reg, region_cvar=statefip, weights=weights,
                  sector_cvar=sic, method="all")
ivreg_ss(as.formula(paste("d_sh_empl ~", ctrls, "| shock")), W=ADH$W,
                  X=IV, data=ADH$reg, region_cvar=statefip,
                  weights=weights, sector_cvar=sic, method="all")

