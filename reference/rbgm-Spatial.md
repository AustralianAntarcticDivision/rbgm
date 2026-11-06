# Convert to spatial format

Take the output of
[`bgmfile`](https://australianantarcticdivision.github.io/rbgm/reference/bgmfile.md)
and return a
[`Spatial`](https://edzer.github.io/sp/reference/Spatial-class.html)
object or a sf object.

## Usage

``` r
boxSpatial(bgm)

box_sp(bgm)

box_sf(bgm)

boundarySpatial(bgm)

boundary_sp(bgm)

boundary_sf(bgm)

node_sp(bgm)

point_sp(bgm)

faceSpatial(bgm)

face_sp(bgm)

face_sf(bgm)
```

## Arguments

- bgm:

  output of a BGM file, as returned by
  [`bgmfile`](https://australianantarcticdivision.github.io/rbgm/reference/bgmfile.md)

## Value

Spatial\* object or sf object

- box_sp
  [`SpatialPolygonsDataFrame`](https://edzer.github.io/sp/reference/SpatialPolygons.html)

- face_sp
  [`SpatialLinesDataFrame`](https://edzer.github.io/sp/reference/SpatialLines.html)

- boundary_sp
  [`SpatialPolygonsDataFrame`](https://edzer.github.io/sp/reference/SpatialPolygons.html)

- node_sp
  [`SpatialPointsDataFrame`](https://edzer.github.io/sp/reference/SpatialPoints.html)

- point_sp
  [`SpatialPointsDataFrame`](https://edzer.github.io/sp/reference/SpatialPoints.html)

&nbsp;

- box_sf `sf with sfc_POLYGON column`

- face_sf `sf with sfc_LINESTRING column`

- boundary_sf `sf with sfc_POLYGON column`

- node_sf `sf with sfc_POINT column`

- point_sf `sf with sfc_POINT column`

## Details

Note that the \`\_sp\` forms are aliased to original functions called
\`\*Spatial\`, and now have \`\_sf\` counterparts to return that format.

## Warning

The sf objects created by \`box_sf()\`, \`node_sf()\`, \`face_sf()\`,
\`boundary_sf()\` and \`point_sf()\` were not created by the sf package.
They were created with reference to the sf format prior to November
2019. If you have problems it may be necessary to recreate the 'crs'
part of the of the object with code like \`x \<- box_sf(bgm);
library(sf); st_crs(x) \<- st_crs(attr(x\$geometry, "crs")\$proj)\`.

Get in touch (\[create an
issue\](https://github.com/AustralianAntarcticDivision/rbgm/issues)) if
you have any troubles.

## Examples

``` r
fname <- bgmfiles::bgmfiles(pattern = "antarctica_28")
bgm <- bgmfile(fname)
spdf <- box_sp(bgm)
sfdf <- box_sf(bgm)
sldf <- face_sp(bgm)

plot(spdf, col = grey(seq(0, 1, length = nrow(bgm$boxes))))
plot(sldf, col = rainbow(nrow(bgm$faces)), lwd = 2,  add = TRUE)
```
