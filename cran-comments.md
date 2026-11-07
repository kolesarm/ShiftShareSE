# Submission note

- This update fixes the test failures reported on CRAN's MKL additional-issues
  check. The failing tests required exact equality between results of identical
  computations. Intel MKL does not guarantee bit-for-bit reproducibility of such
  computations, so the tests now compare these results up to rounding error
  (testthat's default tolerance) rather than exactly. No numerical optimization
  is involved, so the remaining differences are at the level of machine
  precision.
- This update also adds examples for reg_ss.fit() and ivreg_ss.fit().
- The tests also pass with MKL 2025.3 on R-hub and with MKL 2026.2 on GitHub
  Actions, although neither setup reproduced the failures in the previous
  version, presumably due to hardware differences.


## Test environments
* local Ubuntu 24.04.5 LTS, R 4.6.1
* Github actions
  - Ubuntu 24.04.5 LTS, R-devel with BLIS
  - macOS 26.7.2, 4.6.2
  - Windows Server 2025, R 4.6.2
  - Ubuntu 24.04.5 LTS, R-devel
  - Ubuntu 24.04.5 LTS, R 4.6.2
  - Ubuntu 24.04.5 LTS, R-oldrel 4.5.3
* win-builder, R-devel and R-release
* Rhub
  - mkl (Intel MKL 2025.3), R-devel (2026-11-06 r90845)
  - ubuntu-gcc12 24.04.5 LTS, R-devel (2026-04-13 r89874)
  - windows server 2025, R-devel (2026-11-06 r90845 ucrt)
  - ubuntu-release 24.04.5 LTS, R 4.6.2
  - macOS-arm64 26.7.2, R-devel (2026-11-06 r90845)
  - macOS 15.7.9 R-devel (2026-11-06 r90845)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package
