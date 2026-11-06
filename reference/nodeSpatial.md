# Vertices as Spatial points.

Obtain all vertices as a
[`SpatialPointsDataFrame`](https://edzer.github.io/sp/reference/SpatialPoints.html)
or a sf dataframe.

## Usage

``` r
nodeSpatial(bgm)

node_sf(bgm)

pointSpatial(bgm)

point_sf(bgm)
```

## Arguments

- bgm:

  BGM object from
  [`bgmfile`](https://australianantarcticdivision.github.io/rbgm/reference/bgmfile.md)

## Value

[`SpatialPointsDataFrame`](https://edzer.github.io/sp/reference/SpatialPoints.html)
or sf data frame

## Details

Nodes are the unique coordinates (or vertices), points are the instances
of those coordinates that exist in the model.
[`point_sp`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md)
or `point_sf` return all instances of the vertices with information
about which boxes they belong to.
[`node_sp`](https://australianantarcticdivision.github.io/rbgm/reference/rbgm-Spatial.md)
and `node_sf` return all vertices.

## Warning

The sf objects created by \`box_sf()\`, \`node_sf()\`, \`face_sf()\`,
\`boundary_sf()\` and \`point_sf()\` were not created by the sf package.
They were created with reference to the sf format prior to November
2019. If you have problems it may be necessary to recreate the 'crs'
part of the of the object with code like \`x \<- node_sf(bgm);
library(sf); st_crs(x) \<- st_crs(attr(x\$geometry, "crs")\$proj)\`.

Get in touch (\[create an
issue\](https://github.com/AustralianAntarcticDivision/rbgm/issues)) if
you have any troubles.

## Examples

``` r
fname <- bgmfiles::bgmfiles(pattern = "antarctica_28")
bgm <- bgmfile(fname)
spnode <- node_sp(bgm)
names(spnode)
#> [1] ".vx0"
nrow(spnode)  ## only unique vertices
#> [1] 112
nrow(bgm$vertices)
#> [1] 112

sppoints <- point_sp(bgm)
#> Joining with `by = join_by(.vx0)`
names(sppoints)
#> [1] ".vx0" ".fx0" ".p0" 
nrow(sppoints)
#> [1] 180
names(point_sf(bgm))
#> Joining with `by = join_by(.vx0)`
#> [1] ".vx0"     ".fx0"     ".p0"      "geometry"
```
