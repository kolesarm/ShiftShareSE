# ShiftShareSE 1.1.1

## Minor improvements and fixes

- Fix test failures on CRAN's MKL additional-issues check. New tests no longer
  require exact equality, and allow for numerical rounding error
- Add examples to `reg_ss.fit` and `ivreg_ss.fit`

## New Features

- Automatically handle collinear share matrix


# ShiftShareSE 1.1.0

## New Features

- Automatically handle collinear share matrix


# ShiftShareSE 1.0.1

## Minor improvements and fixes

- Fix a broken check on R-devel due to new implementation of round() in R4.0.0
- Add section on collinear sectors in vignette, and make clarifications in
  documentation

# ShiftShareSE 1.0.0

## New Features

- The functions `reg_ss` and `ivreg_ss` implement the 'AKM' and 'AKM0' methods
  for constructing confidence intervals in shift-share least squares and
  instrumental variables regressions. These methods have been developed in
  [Adão, Kolesár, and Morales (2019)](https://doi.org/10.1093/qje/qjz025)
