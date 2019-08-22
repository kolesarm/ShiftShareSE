## Resubmission
This is a resubmission. In this version I have:

* For all exported functions: added `\value` explaining the functions' results
* In `ivreg_ss.fit` and `reg_ss.fit`: added check for collinearity of one of the
  function inputs (share matrix `W`), and added a unit test to check this works
  properly.


## Test environments
* local Debian 10 ("buster") install, R 3.6.1
* Ubuntu 16.04.6 (on travis-ci), R 3.6.1
* win-builder (devel)
* Rhub, Ubuntu Linux 16.04 LTS, R-release

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Michal Kolesár <kolesarmi@googlemail.com>’

New submission

## Downstream dependencies
There are currently no downstream dependencies for this package
