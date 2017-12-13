Advent of code 2017 - Day 13: Packet Scanners
================

Part 1
------

A firewall is sending signals on some layers. Those layers are at a given depth, and have a range. The signal is moving up and down in this range each picosecond. We are moving on the top of the layers one layer each picosecond. The firewall detects us if there is a signal on the layer before we enter it. We are not detected if the signal arrives and we are already on the layer. We are moving before the signal.

``` r
library(tidyverse)
library(testthat)
library(stringr)
library(magrittr)
```

The example input is as follow.

``` r
ex_input <- "0: 3
1: 2
4: 4
6: 4"

ex_input <- str_split(ex_input, "\\n")[[1]]
ex_input
```

    ## [1] "0: 3" "1: 2" "4: 4" "6: 4"

I want to compute where each signal is at a given time. It is not `%% range` directly because the signal moves up if it reached the bottom and down if is reached the top.

``` r
signal_pos <- function(time, layer_range) {
  pos <- seq(1, layer_range)
  if(layer_range > 2) {
    pos <- c(pos, seq(layer_range - 1, 2, -1))
  }
  # (2 * layer_range - 2)) is length(pos), + 1 because index starts at 1
  pos[(time %% (2 * layer_range - 2)) + 1]
}

signal_pos(6, 4)
```

    ## [1] 1

I will create a tibble from the input that gives the depth and the range. We will walk into a signal if the `signal_pos` for this depth and this range is `1`.

``` r
create_scan <- function(input) {
  input %>% 
    as_tibble() %>% 
    separate(value, c("depth", "range"), sep = ": ", convert = TRUE)
}
scanners <- create_scan(ex_input)
scanners
```

    ## # A tibble: 4 x 2
    ##   depth range
    ## * <int> <int>
    ## 1     0     3
    ## 2     1     2
    ## 3     4     4
    ## 4     6     4

The severity is the product of the depth and the range if we are caught

``` r
caught <- function(scann) {
  scann %>% 
    mutate(scan = map2_dbl(depth, range, signal_pos),
           caught = scan == 1,
           severity = depth  * range * caught)
}

caught(scanners)
```

    ## # A tibble: 4 x 5
    ##   depth range  scan caught severity
    ##   <int> <int> <dbl>  <lgl>    <int>
    ## 1     0     3     1   TRUE        0
    ## 2     1     2     2  FALSE        0
    ## 3     4     4     3  FALSE        0
    ## 4     6     4     1   TRUE       24

``` r
my_input <- read_lines("day13-input.txt")
```

``` r
my_scan <- create_scan(my_input)
caught(my_scan) %>% 
  select(severity) %>% 
  sum()
```

    ## [1] 2508

Part 2
------

Brute force solution, test all input on all daly until I find something good.

``` r
delayed <- function(scan) {
  delay = 1
  scan <- scan %>% 
    mutate(depth = depth + 1) %>% 
    mutate(scan = map2_dbl(depth, range, signal_pos))
  while(any(scan$scan == 1)) {
    scan <- scan %>% 
      mutate(depth = depth + 1) %>% 
    mutate(scan = map2_dbl(depth, range, signal_pos))
    delay = delay + 1
    if (delay %% 1000 == 0) print(delay)
  }
  delay
}

delayed(scanners)
```

    ## [1] 10

Don't run, it is way too slow, and the answer is above 50k...

``` r
# delayed(my_scan)
```

Turns out, I don't have to know the exact position of a signal, I just have to know if it is one (well actually if check for 0 as I take the modulo)

``` r
signal_one <- function(time, layer_range) {
  time %% (2 * layer_range - 2) == 0
}

for(t in seq(0, 8)) print(signal_one(t, 4))
```

    ## [1] TRUE
    ## [1] FALSE
    ## [1] FALSE
    ## [1] FALSE
    ## [1] FALSE
    ## [1] FALSE
    ## [1] TRUE
    ## [1] FALSE
    ## [1] FALSE

``` r
test_caught <- function(scan) {
  t <- 2e6
  while(any(map2_lgl(scan$depth + t, scan$range, signal_one))){
    t <- t + 1
  }
  t
}
test_caught(scanners)
```

    ## [1] 2000002

It is faster but not enough, as the answer is above `2e6`...

``` r
# test_caught(my_scan)
```

Answer is 3913186

``` r
test_caught <- function(scan) {
  t <- 1
  j <- 1
  rows <- nrow(scan)
  while(j < rows) {
    for (i in seq(rows)) {
      if (signal_one(scan$depth[i] + t, scan$range[i])) {
        break
      }
    }
    t <- t + 1
    j <- i
    #print(c(t = t - 1, j = j))
  }
  t
}
test_caught(scanners)
```

    ## [1] 7

``` r
for (i in seq_along(scanners)) {
      if (signal_one(scanners$depth[i] + 10, scanners$range[i])) {
        break
      }
    }
```
