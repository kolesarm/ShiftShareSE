## Submission note
- This update fixes a broken check on R-devel due to new implementation of
  round()

## Test environments
* local Debian 10 ("buster") install, R 3.6.2
* Ubuntu 16.04.6 (on travis-ci), R-devel (2020-01-03 r77629) and release (3.6.1)
* win-builder,  R-devel (2020-01-03 r77629) and release (3.6.2)
* Rhub
  - Ubuntu Linux 16.04 LTS, R-release (3.6.1), GCC
  - Debian Linux, R-devel (2020-01-03 r77629), GCC, no long double

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package
