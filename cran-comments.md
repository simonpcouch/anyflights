## Test environments

  - local OS X install, R 3.6.3
  - ubuntu 16.04 (on travis-ci), R 3.6.2
  - win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE (from win-builder):

```
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Simon P. Couch <simonpatrickcouch@gmail.com>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
nycflights (2:15)


CRAN repository db overrides:
 X-CRAN-Comment: Archived on 2019-02-12 at the request of the
    maintainer.
```

`anyflights` is currently an archived package. I've decided to resubmit this 
package as I now have the resources to maintain it.

The word `nycflights` appears in line 2 in the description in reference to
the CRAN package `nycflights13`, which `anyflights` is inspired by.

## Downstream dependencies

There are currently no downstream dependencies for this package.

