## Use DataADH_check.xlsx from May 30, 2018

## 1444 observations (722 regions in 1990-2000 and 2000-2007), and 775 sectors
## (a sector emp share interacted with the period)
d <- readxl::read_excel("DataADH_check.xlsx", sheet=1)
ctrls <- readxl::read_excel("DataADH_check.xlsx", sheet=2)

## weight matrix and shocks
ADH_Xs <- readxl::read_excel("DataADH_check.xlsx", sheet=4,
                             col_names=FALSE)$X__1
ADH_W <- unname(as.matrix(readxl::read_excel("DataADH_check.xlsx",
                                         sheet=3, col_names=FALSE)))
ADH <- cbind(d, ctrls)

## Get correct instrument, add statefip
adh <- "~/teaching/Datasets/AutorDornHanson2013/Public Release Data/dta/workfile_china.dta"
d1 <- readstata13::read.dta13(adh)
d1 <- d1[, c("d_sh_empl_mfg", "d_sh_empl_nmfg", "statefip",
             "d_tradeusch_pw", "d_tradeotch_pw_lag")]

ADH$rowid <- 1:nrow(ADH)
ADH$id <- 1000*round(ADH$d_sh_empl_mfg, 4)+10^6*round(ADH$d_sh_empl_nmfg, 4)+
    10^8*round(ADH$ADHshock, 5)
d1$id <- 1000*round(d1$d_sh_empl_mfg, 4)+10^6*round(d1$d_sh_empl_nmfg, 4)+
    10^8*round(d1$d_tradeusch_pw, 5)
d1[, c("d_sh_empl_mfg", "d_sh_empl_nmfg", "d_tradeusch_pw")] <- NULL
ADH1 <- merge(ADH, d1, by="id")
ADH <- ADH1[order(ADH1$rowid), ]
ADH$ADHshock_IV <- ADH$d_tradeotch_pw_lag
ADH[, c("d_tradeotch_pw_lag", "rowid", "id")] <- NULL

ADH$division <-  2*ADH$reg_midatl + 3*ADH$reg_encen + 4*ADH$reg_wncen +
    5*ADH$reg_satl+ 6*ADH$reg_escen + 7*ADH$reg_wscen + 8*ADH$reg_mount +
    9*ADH$reg_pacif
ADH$division[ADH$division==0] <-  1     # New England
ADH$division <- as.factor(ADH$division)
ADH[, c(7, 10:17)] <- NULL

ADH$t2 <- ADH$t2==1
names(ADH)[4:6] <- c("shock", "IV", "weights")

devtools::use_data(ADH, overwrite=TRUE, internal=FALSE)
devtools::use_data(ADH_Xs, overwrite=TRUE, internal=FALSE)
devtools::use_data(ADH_W, overwrite=TRUE, internal=FALSE)

## TODO: correct instrument from excel file, rename weights
