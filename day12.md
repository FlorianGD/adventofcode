Day 12: Digital Plumber
================

Part 1
------

Programs commnunicate with each other given pipes `<->`, but not directly. Given the input, we have to find all the programs that communicate with the program 0.

``` r
library(tidyverse)
library(testthat)
library(stringr)
library(magrittr)
```

The example input is as follow.

``` r
ex_input <- "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"

ex_input <- str_split(ex_input, "\\n")[[1]]
ex_input
```

    ## [1] "0 <-> 2"       "1 <-> 1"       "2 <-> 0, 3, 4" "3 <-> 2, 4"   
    ## [5] "4 <-> 2, 3, 6" "5 <-> 6"       "6 <-> 4, 5"

Let's split it to `program` and `links`.

``` r
separate_links <- function(input) {
  input %>% 
    as_tibble() %>% 
    separate(value, c("program", "links"), sep = " <-> ", convert = TRUE) %>% 
    mutate(links = str_split(links, ", ")) %>% 
    mutate(links = map(links, as.integer))
}
ex_links <- separate_links(ex_input)

ex_links
```

    ## # A tibble: 7 x 2
    ##   program     links
    ##     <int>    <list>
    ## 1       0 <int [1]>
    ## 2       1 <int [1]>
    ## 3       2 <int [3]>
    ## 4       3 <int [2]>
    ## 5       4 <int [3]>
    ## 6       5 <int [1]>
    ## 7       6 <int [2]>

I will make a recursive function to look into the list in links and to follow the programs inside of it, if they are not already in a vector.

``` r
next_links <- function(prev_links, programs) {
  programs %>% 
      filter(program %in% prev_links) %>% 
      use_series(links) %>% 
      reduce(union)
}

rec_lookup <- function(links, programs, control = c()) {
  # Get the next links
  next_links_vec <- next_links(links, programs)
  # Check if there are new values
  new_vec <- setdiff(next_links_vec, control)

  if (!is_empty(new_vec)) {
    # Add the new values to control
    control <- c(control, new_vec)
    # Repeat with the new values
    control <- rec_lookup(new_vec, programs, control)
  }
  control
}

rec_lookup(0, ex_links)
```

    ## [1] 2 0 3 4 6 5

``` r
my_input <- read_lines("day12-input.txt")
my_programs <- separate_links(my_input)
```

Now we only need the length of the result.

``` r
length(rec_lookup(0, my_programs))
```

    ## [1] 378

Part 2
------
