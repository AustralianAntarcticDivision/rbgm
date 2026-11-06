# BGM examples

## Available BGM examples

The `bgmfiles` package contains several real-world example BGM files.

Obtain every available BGM file and plot in native coordinates.

``` r

library(rbgm)
```

    ## Loading required package: raster

    ## Loading required package: sp

``` r

library(bgmfiles)

## turn +proj into line separated text
breakproj <- function(x) {
  paste(strsplit(x, " ")[[1]], collapse = "\n")
}
files <- bgmfiles()
for (i in seq_along(files)) {
  bgm <- bgmfile(files[i])
  boxes <- boxSpatial(bgm)
  plot(boxes, col = ifelse(boxes$boundary, "#88888880", sample(rainbow(nrow(boxes), alpha = 0.5))))
  op <- par(xpd = NA)
  par(op)
  title(basename(files[i]), cex = 0.8)
  mtext(breakproj(proj4string(boxes)), cex = 0.75, side = 2, las = 1, adj = 0, line = 2, at = par("usr")[3], xpd = NA)
 
}
```

![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-1.png)

    ## /home/runner/work/_temp/Library/bgmfiles/extdata/bgm/ams71.bgm 
    ## file declares 187 faces but contains data for 193 faces
    ## 
    ##  ... returning all 193 faces

![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-2.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-3.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-4.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-5.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-6.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-7.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-8.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-9.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-10.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-11.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-12.png)![](BGM_examplefiles_files/figure-html/unnamed-chunk-1-13.png)

Make one interactive map with all (code not run here to avoid
dependencies).

``` r

library(rbgm)
library(bgmfiles)
library(mapview)


mp <- mapview()  ## empty map view

fs <- bgmfiles::bgmfiles()  ## paths of all files collected
cols <- rainbow(length(fs), alpha = 0.2)
## read in all boxes, convert to SpatialPolygonsDataFrame, and add to the map view
for (i in seq_along(fs)) {
  mp <- mp + mapview(boxSpatial(bgmfile(fs[i])), color = cols[i], alpha = 0.3, layer.name = basename(fs[i]))
}

## view the mapview
mp
```
