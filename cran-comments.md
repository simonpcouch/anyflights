This is an initial release. Our plan is to update yearly (February/March) to update the data to the previous year. Since the current size of the data is smaller than that of `nycflights13`, we're confident that the size of this data won't blow up over time.

---

## Test environments
* local OS X install, R 3.3.3
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* Checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    data   6.1Mb
  
  This is a data package that will be rarely updated.

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Jay Lee <jaylee@reed.edu>'

  This is my first package as maintainer, but I am an active coauthor on `rcv` and was involved in all but name in previously submitting that to CRAN.

## Reverse dependencies

Initial release, none exist.
