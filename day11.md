Day 11: Hex Ed
================

Part 1
------

We have to find the distance from the center in an hexagonal grid. We are given a path and we hve to find how close we land from the center. The path is given by directions : `n, nw, sw, s, se, ne`.

``` r
library(tidyverse)
library(testthat)
```

I'm going to give coordinates to each direction, convert the input in a sum of coordinates, and find the distance to the center from the final coordinate.

``` r
coord <- tribble(
  ~dir, ~vec,
  "n",  c(0, 1),
  "ne", c(0.5, 0.5),
  "se", c(0.5, -0.5),
  "s",  c(0, -1),
  "sw", c(-0.5, -0.5),
  "nw", c(-0.5, 0.5)
)
coord
```

    ## # A tibble: 6 x 2
    ##     dir       vec
    ##   <chr>    <list>
    ## 1     n <dbl [2]>
    ## 2    ne <dbl [2]>
    ## 3    se <dbl [2]>
    ## 4     s <dbl [2]>
    ## 5    sw <dbl [2]>
    ## 6    nw <dbl [2]>
