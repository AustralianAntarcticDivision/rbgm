# Changelog

## rbgm (development version)

- relicensed from GPL-3 to MIT; the copyright holder is the Department
  of Climate Change, Energy, the Environment and Water. Versions up to
  0.2.0 were GPL-3.

- default branch is now `main`; GitHub Actions refreshed (R-CMD-check,
  test-coverage, pkgdown) and the CRAN, check and coverage badges
  updated.

- pkgdown site with a Bootstrap 5 theme; Code of Conduct updated to
  Contributor Covenant 2.1.

- tests updated to testthat 3rd edition, and skipped when bgmfiles is
  not installed.

- replaced deprecated `.data$x` in tidyselect
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) calls.

- DESCRIPTION: software name ‘Atlantis’ single-quoted, BGM acronym
  explained, `Language: en-US`, copyright holder role added; added
  `inst/WORDLIST`.

## rbgm 0.2.0

CRAN release: 2025-04-03

- Fixed broken doc links, thanks CRAN.

## rbgm 0.1.0

CRAN release: 2020-04-12

- rbgm has growed up a bit, cut a lot of ties from the anti-mesh
  brigade, so we celebrate with a 0.1.0 release.

- spdplyr and rgeos requirement removed.

- New functions
  [`box_sp()`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md),
  [`boundary_sp()`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md),
  [`node_sp()`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md),
  [`point_sp()`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md),
  `face_sp`,
  [`box_sf()`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md),
  [`boundary_sf()`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md),
  [`node_sf()`](https://australianantarcticdivision.github.io/rbgm/reference/nodeSpatial.md),
  `point_sf`. The `_sp` versions are simply aliases of
  `boxSpatial(), boundarySpatial(), nodeSpatial(), and pointSpatial()` -
  but with new names to be consistent with the sf versions.

- rbgm now imports sfheaders.

## rbgm 0.0.5

CRAN release: 2018-05-18

- Included `rgeos` and `spdplyr` in Suggests, thanks to CRAN.

- added new function ‘read_bgm’ as a more obvious version of the
  original read function ‘bgmfile’

- removed names from component sp lists to avoid troubles in sf causing
  troubles in mapview

## rbgm 0.0.4

CRAN release: 2016-07-22

- added vignettes, examples, more doc

- removed some old functions

- cleaned up for CRAN

- fixed a gnarly bug due to lexicographic sorting on a list! Lines from
  faceSpatial were coming out in the sorted order of “face0”, “face1”,
  “face10”, “face30” etc.

- renamed “leftbox” and “rightbox” to “left” and “right”

- readme includes all example files in bgmfiles

- new example files added

- added functions pointSpatial and nodeSpatial

- new function bgmfile to replace deprecated read_bgm

- new tests and changes to capture Guam_utm1 variant

## rbgm 0.0.2

- First fully working build ready for check.
