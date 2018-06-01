use adh.dta
local ctrls = "t2 l_shind_manuf_cbp l_sh_popedu_c l_sh_popfborn l_sh_empl_f l_sh_routine33 l_task_outsource i.division"

eststo clear
qui: eststo: regress shock X `ctrls'
qui: eststo: regress shock X `ctrls', robust
qui: eststo: regress shock X `ctrls', cluster(statefip)

qui: eststo: regress shock X `ctrls' [aw=weights]
qui: eststo: regress shock X `ctrls' [aw=weights], robust
qui: eststo: regress shock X `ctrls' [aw=weights], cluster(statefip)
esttab,  b(%11.0g) se(%11.0g)

eststo clear
qui: eststo: regress d_sh_empl_mfg X `ctrls'
qui: eststo: regress d_sh_empl_mfg X `ctrls', robust
qui: eststo: regress d_sh_empl_mfg X `ctrls', cluster(statefip)

qui: eststo: regress d_sh_empl_mfg X `ctrls' [aw=weights]
qui: eststo: regress d_sh_empl_mfg X `ctrls' [aw=weights], robust
qui: eststo: regress d_sh_empl_mfg X `ctrls' [aw=weights], cluster(statefip)
esttab,  b(%11.0g) se(%11.0g)
