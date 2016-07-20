[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/AustralianAntarcticDivision/rbgm.png?branch=master)](https://travis-ci.org/AustralianAntarcticDivision/rbgm) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AustralianAntarcticDivision/rbgm?branch=master&svg=true)](https://ci.appveyor.com/project/AustralianAntarcticDivision/rbgm) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rbgm)](https://cran.r-project.org/package=rbgm) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/rbgm)](http://cran.r-project.org/web/packages/rbgm/index.html) [![Coverage Status](https://img.shields.io/codecov/c/github/AustralianAntarcticDivision/rbgm/master.svg)](https://codecov.io/github/AustralianAntarcticDivision/rbgm?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
rbgm - R tools for Box Geometry model files
-------------------------------------------

`rbgm` allows reading of geometry from BGM files, either in raw coordinate form or as Spatial objects. The aim is to make all of the following straightforward:

-   read of the BGM format, maintaining all topology and attributes
-   use of the BGM specification for visualization and data extraction
-   creation of BGM from from shapefiles, R spatial objects and whatever else

Installation
------------

Install from CRAN:

``` r
install.packages("rbgm")
```

Install the development version from Github using `devtools`.

``` r
# install.packages("devtools")
devtools::install_github("mdsumner/rbgm")
```

### How can I contribute to rbgm?

Install, use, test the package, and let me know!

Please use the Issues tab on GitHub to add feature requests and bug reports: <https://github.com/AustralianAntarcticDivision/rbgm/issues/>

use [Pull Requests](http://r-pkgs.had.co.nz/git.html#git-pullreq) if you have changes you'd like to contribute.

Related work
------------

-   [mfdbatlantis](https://github.com/mareframe/mfdbatlantis) MareFrame Atlantis routines
-   [atlantistools](https://github.com/alketh/atlantistools) data processing and visualisation tool for R
-   [shinyrAtlantis](https://github.com/shanearichards/shinyrAtlantis)
-   [ratlantis](https://github.com/jsgosnell/ratlantis) R code for interfacing with Atlantis ecosystem modeling software
-   [vat](https://github.com/mareframe/vat) Visualizing Atlantis Tool (vat)
-   [EastAntarctica\_Atlantis](https://github.com/AustralianAntarcticDivision/EastAntarctica_Atlantis) Project work at Australian Antarctic Division and the Antarctic Climate and Ecosystems CRC
-   [AtlantisNEUS\_R](https://github.com/erikjsolsen/AtlantisNEUS_R) R code (scripts and functions) to interact with and analyze output from the Atlantis NEAU End-2-end marine ecosystem model

Examples
--------

See the package vignettes for examples.
