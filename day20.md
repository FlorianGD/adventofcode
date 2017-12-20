Advent of code 2017 - Day 20: Particle Swarm
================

Part 1
------

Particles move in 3D given an acceleration, a velocity and an initial position. We have to find which particle will stay the closest to `<0, 0, 0>` in the long run, using the Manhattan distance.

``` r
library(tidyverse)
library(testthat)
library(stringr)
```

We read the input file.

``` r
input <- read_csv("day20-input.txt", 
                  col_names = c("px", "py", "pz", "vx", "vy", "vz", "ax", "ay", "az"), 
                  col_types = "cicciccic") %>% 
  mutate_if(is.character, ~as.integer(str_replace_all(.x, "[^0-9-]", "")))
```

In the long run, particles move away from the origin as twice the acceleration. We can drop the 2 as it is for all the particles, and compute the Manhattan distance for the acceleration for the particles and select the minimum.

``` r
input %>% 
  select(starts_with("a")) %>% 
  mutate(particle = row_number() - 1,
         dist = abs(ax) + abs(ay) + abs(az)) %>% 
  filter(dist == min(dist))
```

    ## # A tibble: 1 x 5
    ##      ax    ay    az particle  dist
    ##   <int> <int> <int>    <dbl> <int>
    ## 1     0     0     0      376     0
