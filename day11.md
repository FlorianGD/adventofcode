Day 11: Hex Ed
================

Part 1
------

We have to find the distance from the center in an hexagonal grid. We are given a path and we hve to find how close we land from the center. The path is given by directions : `n, nw, sw, s, se, ne`.

``` r
library(tidyverse)
library(testthat)
library(stringr)
```

I'm going to give coordinates to each direction, convert the input in a sum of coordinates, and find the distance to the center from the final coordinate. See [this link](https://www.redblobgames.com/grids/hexagons/#neighbors-cube) for a description of the cubic grid system.

``` r
coord <- tribble(
  ~dir, ~x, ~y,  ~z,
  "n",   0,   1,  -1,
  "ne",  1,   0,  -1,
  "se",  1,  -1,   0,
  "s",   0,  -1,   1,
  "sw", -1,  -0,   1,
  "nw", -1,   1,   0
)
coord
```

    ## # A tibble: 6 x 4
    ##     dir     x     y     z
    ##   <chr> <dbl> <dbl> <dbl>
    ## 1     n     0     1    -1
    ## 2    ne     1     0    -1
    ## 3    se     1    -1     0
    ## 4     s     0    -1     1
    ## 5    sw    -1     0     1
    ## 6    nw    -1     1     0

Let's define a distance

``` r
hex_dist <- function(vec){
  max(abs(vec))
}
```

Let's walk the stream

``` r
walk_stream <- function(input){
  instructions <- str_split(input, ",")[[1]]
  path <- tibble(dir = instructions)
  path %>% 
    left_join(coord, by = "dir") %>% 
    select(-dir) %>% 
    summarise_all(sum)
}

dist_stream <- function(input){
  hex_dist(walk_stream(input))
}

expect_equal(dist_stream("ne,ne,ne"), 3)
expect_equal(dist_stream("ne,ne,sw,sw"), 0)
expect_equal(dist_stream("ne,ne,s,s"), 2)
expect_equal(dist_stream("se,sw,se,sw,sw"), 3)
```

And now with the input

``` r
my_input <- read_lines("day11-input.txt")
dist_stream(my_input)
```

    ## [1] 696

Part 2
------

We have to compute the furthest away we got from the starting point.

``` r
walk_stream_2 <- function(input){
  instructions <- str_split(input, ",")[[1]]
  path <- tibble(dir = instructions)
  path %>% 
    left_join(coord, by = "dir") %>% 
    select(-dir) %>% 
    mutate_all(cumsum) %>% 
    mutate(x = abs(x), 
           y = abs(y), 
           z = abs(z)) %>% 
    summarise(dist = max(x, y, z))
}

walk_stream_2(my_input)
```

    ## # A tibble: 1 x 1
    ##    dist
    ##   <dbl>
    ## 1  1461
