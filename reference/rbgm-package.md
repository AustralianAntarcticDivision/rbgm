# Utilities for BGM files for Atlantis

Tools for handling network data for Atlantis from box-geometry model
(BGM) files

## rbgm features

- read .bgm files and faithfully store all information so it can be
  round-tripped

- conversion from .bgm forms to Spatial classes (lines and polygons)

- (not yet implemented: write to .bgm)

## I. Import

|  |  |
|----|----|
| [`bgmfile`](https://australianantarcticdivision.github.io/rbgm/reference/bgmfile.md) | read directly from a .bgm file |

## II. Conversion

|  |  |
|----|----|
| [`boxSpatial`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md) | convert boxes to a [`SpatialPolygonsDataFrame`](https://edzer.github.io/sp/reference/SpatialPolygons.html) |
| [`faceSpatial`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md) | convert faces to a [`SpatialLinesDataFrame`](https://edzer.github.io/sp/reference/SpatialLines.html) |
| [`boundarySpatial`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md) | convert boundary to a single-row [`SpatialPolygonsDataFrame`](https://edzer.github.io/sp/reference/SpatialPolygons.html) |
| [`nodeSpatial`](https://australianantarcticdivision.github.io/rbgm/reference/nodeSpatial.md) | obtain all vertices as points |
| [`pointSpatial`](https://australianantarcticdivision.github.io/rbgm/reference/nodeSpatial.md) | obtain all instances of vertices as points |

## III. Miscellaneous

|  |  |
|----|----|
| [`build_dz`](https://australianantarcticdivision.github.io/rbgm/reference/build_dz.md) | Build Atlantis dz Values |

## See also

Useful links:

- <https://github.com/AustralianAntarcticDivision/rbgm/>

- <https://australianantarcticdivision.github.io/rbgm/>

- Report bugs at
  <https://github.com/AustralianAntarcticDivision/rbgm/issues/>

## Author

**Maintainer**: Michael D. Sumner <mdsumner@gmail.com>

Authors:

- Michael D. Sumner <mdsumner@gmail.com>

Other contributors:

- Department of Climate Change, Energy, the Environment and Water
  (Australian Antarctic Division) \[copyright holder\]
