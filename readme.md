[![Travis build status](https://travis-ci.org/kolesarm/BartikSE.svg?branch=master)](https://travis-ci.org/kolesarm/BartikSE) [![Coverage status](https://codecov.io/gh/kolesarm/BartikSE/branch/master/graph/badge.svg)](https://codecov.io/github/kolesarm/BartikSE?branch=master)

# BartikSE

Confidence intervals in shift-share designs (also called [Bartik
(1991)](http://research.upjohn.org/up_press/77/) designs ) using procedures from
[Adão, Kolesár, and Morales (2018)](https://arxiv.org/abs/1806.07928). See the
[BartikSEMatlab](https://github.com/kolesarm/BartikSEMatlab) package for
`Matlab` version of this code.

See vignette `BartikSE.pdf` (stored under `doc/`) for description of the
package, and `manual.pdf` (also stored under `doc/`) for documentation.

## Installation

The package can be installed manually with `R CMD INSTALL` by downloading the
source code here, or using the function `install_github()` from the `devtools`
package:

```
install.packages("devtools") ## if devtools package not installed
devtools::install_github("kolesarm/BartikSE")
```
