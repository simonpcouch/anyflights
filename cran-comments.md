## Test environments

  - local OS X install, R 3.6.3
  - ubuntu 16.04 (on travis-ci), R 4.0.0
  - win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE on win-builder:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Simon P. Couch <simonpatrickcouch@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  nycflights (2:15)
```

The word `nycflights` appears in line 2 in the description in reference to
the CRAN package `nycflights13`, which `anyflights` is inspired by.

## Downstream dependencies

There are currently no downstream dependencies for this package.

