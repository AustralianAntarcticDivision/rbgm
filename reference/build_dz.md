# Build Atlantis dz Values

Build dz layer values for Atlantis from a bottom value, up through
successive intervals. Each value is the positive offset required to rise
to the top of the current interval.

## Usage

``` r
build_dz(
  z,
  zlayers = c(-Inf, -2000, -1000, -750, -400, -300, -200, -100, -50, -20, 0)
)
```

## Arguments

- z:

  lowermost value

- zlayers:

  intervals of layer values

## Value

numeric vector of offset values

## Details

Offset values are returned to move from `z` against the intervals in
`zlayers`. The intervals are assumed to be sorted and increasing in
value from `-Inf` to `Inf`. Once the maximum layer is reached the result
is padded by that top value.

## Examples

``` r
## sanity tests
build_dz(-5000)
#>  [1] 3000 1000  250  350  100  100  100   50   30   20
build_dz(-1500)
#>  [1] 500 250 350 100 100 100  50  30  20   0
##build_dz(300)  ## error
build_dz(0)    ## ok
#>  [1] 0 0 0 0 0 0 0 0 0 0
## data
dd <- c(-4396.49, -2100.84, -4448.81, -411.96, -2703.56, -5232.96, 
       -4176.25, -2862.37, -3795.6, -1024.64, -897.93, -1695.82, -4949.76, 
    -5264.24, -2886.81)
## all values in a matrix for checking
## [zlayers, dd]
dzvals <- sapply(dd, build_dz)
## process into text
f1 <- function(x) sprintf("somelabel,%i,%s", x, paste(build_dz(dd[x]), collapse = ","))
tex1 <- sapply(seq(length(dd)),  f1)
## for example
f2 <- function(x) {
sprintf("morelabel,%i,%s", x, paste(as.integer(build_dz(dd[x])), collapse = ","))
}
tex2 <- sapply(seq(length(dd)),  f2)
```
