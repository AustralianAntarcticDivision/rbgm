<!-- badges: start -->

[![R-CMD-check](https://github.com/AustralianAntarcticDivision/rbgm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AustralianAntarcticDivision/rbgm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/AustralianAntarcticDivision/rbgm/graph/badge.svg)](https://app.codecov.io/gh/AustralianAntarcticDivision/rbgm)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rbgm)](https://cran.r-project.org/package=rbgm)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

## rbgm - R tools for Box Geometry Model files

`rbgm` allows reading of geometry from BGM files, either in raw
coordinate form or as Spatial objects or sf objects. The aim is to make
all of the following straightforward:

- read of the BGM format, maintaining all topology and attributes
- use of the BGM specification for visualization and data extraction
- creation of BGM from shapefiles, R spatial objects and whatever else

## Installation

Install from CRAN:

``` r
install.packages("rbgm")
```

Install the development version from Github using `devtools`.

``` r
# install.packages("devtools")
devtools::install_github("AustralianAntarcticDivision/rbgm")
```

### How can I contribute to rbgm?

Install, use, test the package, and let me know!

Please use the Issues tab on GitHub to add feature requests and bug
reports: <https://github.com/AustralianAntarcticDivision/rbgm/issues/>

## Examples

See the package vignettes for examples.

## License

The code in this package is released under the MIT license; the
copyright holder is the Department of Climate Change, Energy, the
Environment and Water (see `LICENSE`). Versions up to 0.2.0 were
released under GPL-3. Note that rbgm depends on other packages that
carry their own licenses.

## Code of Conduct

Please note that the rbgm project is released with a [Contributor Code
of
Conduct](https://australianantarcticdivision.github.io/rbgm/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
