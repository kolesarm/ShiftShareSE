[![R-CMD-check](https://github.com/kolesarm/ShiftShareSE/workflows/R-CMD-check/badge.svg)](https://github.com/kolesarm/ShiftShareSE/actions) [![Coverage status](https://codecov.io/gh/kolesarm/ShiftShareSE/branch/master/graph/badge.svg)](https://codecov.io/github/kolesarm/ShiftShareSE?branch=master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ShiftShareSE)](https://cran.r-project.org/package=ShiftShareSE)

# ShiftShareSE

This R package implements confidence intervals in shift-share designs (also called
[Bartik (1991)](http://research.upjohn.org/up_press/77/) designs) using the
`AKM` and `AKM0` procedures from [Adão, Kolesár, and Morales
(2019)](https://doi.org/10.1093/qje/qjz025). The released version is available
at [CRAN](https://CRAN.R-project.org/package=ShiftShareSE). See the
[ShiftShareSEMatlab](https://github.com/kolesarm/ShiftShareSEMatlab) package for
Matlab version of this code, and the
[ShiftShareSEStata](https://github.com/zhangxiang0822/ShiftShareSEStata) package
for a Stata version.

See vignette [ShiftShareSE](doc/ShiftShareSE.pdf) for description of the package
(available through `vignette("ShiftShareSE")` once package is installed), and the
package [manual](doc/manual.pdf) for documentation of the package functions.

## Example

IV regression using data from [Autor, Dorn, and Hanson
(2013)](https://doi.org/10.1257/aer.103.6.2121), included in the package as `ADH`:
``` r
ivreg_ss(d_sh_empl ~ 1 | shock, X=IV, data=ADH$reg, W=ADH$W,
         method=c("ehw", "akm", "akm0"))
```

Corresponding reduced-form and first-stage regressions:
```r
reg_ss(d_sh_empl ~ 1, X=IV, data=ADH$reg, W=ADH$W,
       method=c("ehw", "akm", "akm0"))
reg_ss(shock ~ 1, X=IV, data=ADH$reg, W=ADH$W,
       method=c("ehw", "akm", "akm0"))
```

## Installation

You can install the released version of `ShiftShareSE` from
[CRAN](https://CRAN.R-project.org/package=ShiftShareSE) with:

``` r
install.packages("ShiftShareSE")
```

Alternatively, you can get the current development version from GitHub:

``` r
install.packages("remotes") # if not installed
remotes::install_github("kolesarm/ShiftShareSE")
```
