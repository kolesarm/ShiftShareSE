[![Travis build status](https://travis-ci.org/kolesarm/ShiftShareSE.svg?branch=master)](https://travis-ci.org/kolesarm/ShiftShareSE) [![Coverage status](https://codecov.io/gh/kolesarm/ShiftShareSE/branch/master/graph/badge.svg)](https://codecov.io/github/kolesarm/ShiftShareSE?branch=master)

# ShiftShareSE

Confidence intervals in shift-share designs (also called [Bartik
(1991)](http://research.upjohn.org/up_press/77/) designs ) using procedures from
[Adão, Kolesár, and Morales (2019)](https://arxiv.org/abs/1806.07928). See the
[ShiftShareSEMatlab](https://github.com/kolesarm/ShiftShareSEMatlab) package for
`Matlab` version of this code.

See vignette [ShiftShareSE](doc/ShiftShareSE.pdf) for description of the package
(available through `vignette("ShiftShareSE")` once package is installed), and the
package [manual](doc/manual.pdf) for documentation of the package functions.

## Installation

The package can be installed manually with `R CMD INSTALL` by downloading the
source code here, or using the function `install_github()` from the `devtools`
package:

```
install.packages("devtools") ## if devtools package not installed
devtools::install_github("kolesarm/ShiftShareSE")
```
