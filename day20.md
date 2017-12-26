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
particles <- read_csv("day20-input.txt", 
                  col_names = c("px", "py", "pz", "vx", "vy", "vz", "ax", "ay", "az"), 
                  col_types = "cicciccic") %>% 
  mutate_if(is.character, ~as.integer(str_replace_all(.x, "[^0-9-]", ""))) %>% 
  mutate(particle = row_number() - 1)
```

In the long run, particles move away from the origin as half the acceleration. We can drop the 1/2 as it is for all the particles, and compute the Manhattan distance for the acceleration for the particles and select the minimum.

``` r
particles %>% 
  select(particle, starts_with("a")) %>% 
  mutate(dist = abs(ax) + abs(ay) + abs(az)) %>% 
  filter(dist == min(dist))
```

    ## # A tibble: 1 x 5
    ##   particle    ax    ay    az  dist
    ##      <dbl> <int> <int> <int> <int>
    ## 1      376     0     0     0     0

And particle 376 is the right answer!

Part 2
------

Let's get the example data.

``` r
ex <- tribble(
  ~px, ~py, ~pz, ~vx, ~vy, ~vz, ~ax, ~ay, ~az,
  -6, 0, 0, 3, 0, 0, 0,0,0,  
  -4, 0, 0,  2, 0, 0, 0,0,0, 
  -2, 0, 0,  1, 0, 0, 0,0,0, 
  3, 0, 0, -1, 0, 0, 0,0,0)

ex <- mutate(ex, particle = row_number() - 1)
```

In this discrete form, the position in the x axis is `ax * t * (t + 1) / 2 + vx * t + px`, and in particular, there is an extra linear coefficient. This got me for quite a long time... (and it did not show in the example that `t^2` alone wouldn't work...)

``` r
collision <- function(parts, t) {
  parts %>% 
    mutate(px_t = ax * t * (t + 1) / 2 + vx * t + px,
           py_t = ay * t * (t + 1) / 2 + vy * t + py,
           pz_t = az * t * (t + 1) / 2 + vz * t + pz) %>% 
    group_by(px_t, py_t, pz_t) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>%
    select(particle)
}

collision(ex, 2)
```

    ## # A tibble: 3 x 1
    ##   particle
    ##      <dbl>
    ## 1        0
    ## 2        1
    ## 3        2

``` r
coll_t <- function(particles, max_t) {
  res <- rep(NA, max_t)
  p <- progress_estimated(max_t)
  for (t in seq(max_t)) {
    p$tick()$print()
    particles <- anti_join(particles, collision(particles, t), by = "particle")
    res[t] <- nrow(particles)
  }
  res
}

expect_equal(coll_t(ex, 3), c(4, 1, 1))
```

``` r
coll_t(particles, 100)
```

    ##   [1] 1000 1000 1000 1000 1000 1000 1000 1000 1000  983  983  969  948  943
    ##  [15]  929  921  906  898  885  858  855  817  817  809  809  802  796  762
    ##  [29]  742  717  699  689  665  641  613  610  596  584  574  574  574  574
    ##  [43]  574  574  574  574  574  574  574  574  574  574  574  574  574  574
    ##  [57]  574  574  574  574  574  574  574  574  574  574  574  574  574  574
    ##  [71]  574  574  574  574  574  574  574  574  574  574  574  574  574  574
    ##  [85]  574  574  574  574  574  574  574  574  574  574  574  574  574  574
    ##  [99]  574  574

And so the answer is 574.
