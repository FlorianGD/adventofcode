Advent of code 2017 - Day 14: Disk Defragmentation
================

Part 1
------

We have to compute a knot hash as in day 10, to 128 inputs given by the concatenation of the puzzle input, a dash and a number between 0 and 127. We have to convert the result to bites afterwards.

``` r
library(tidyverse)
library(testthat)
library(stringr)
library(R.utils) # for conversion to bits
```

I copied and pasted the functions from day 10, and I just have to run `knot_has(input)` to compute the hash.

``` r
conv_input <- function(input) {
  c(as.integer(charToRaw(input)), c(17, 31, 73, 47, 23))
}

indices <- function(pos, len, list_size) {
  ind <- (pos + 1):(pos + len) %% list_size
  ind[ind == 0] <- list_size
  ind
}

rev_indices <- function(my_list, ind) {
  my_list[ind] <- rev(my_list[ind])
  my_list
}

next_pos <- function(current_pos, len, skip_size, list_size){
  (current_pos + len + skip_size) %% list_size
}

hash_list_2 <- function(my_list, lengths, pos = 0, skip_size = 0) {
  list_size <- length(my_list)
  
  for (i in lengths) {
    ind <- indices(pos, i, list_size)
    my_list <- rev_indices(my_list, ind)
    pos <- next_pos(pos, i, skip_size, list_size)
    skip_size <- skip_size + 1    
  }
  return(list(result = my_list, 
              position = pos, 
              skip = skip_size))
}

run_hashes <- function(input){
  my_list <- 0:255
  pos <- 0
  skip_size <- 0
  
  lengths <- conv_input(input)
  for (i in seq(64)) {
    res <- hash_list_2(my_list, lengths, pos, skip_size)
    my_list <- res[[1]]
    pos <- res[[2]]
    skip_size <- res[[3]]
  }
  my_list
}

knot_hash <- function(input){
  hash <- run_hashes(input)
  matrix(hash, ncol = 16) %>% 
    as_tibble() %>% 
    summarise_all(~as.character(as.hexmode(reduce(.x, bitwXor)), width = 2)) %>% 
    paste0(collapse = "")
}

expect_equal(knot_hash(""), "a2582a3a0e66e6e86e3812dcb672a272")
```

We have to convert the result of the hash to a binary vector.

``` r
hex_to_bin <- function(hex){
  # For one character
  hex %>% 
    strtoi(base = 16L) %>% 
    intToBin() %>% 
    str_pad(4, "left", "0")
}
expect_equal(hex_to_bin("0"), "0000")
expect_equal(hex_to_bin("1"), "0001")
expect_equal(hex_to_bin("f"), "1111")
expect_equal(hex_to_bin("e"), "1110")
```

``` r
hash_to_bin <- function(hash) {
  hash %>% 
    str_split( "") %>%
    pluck(1) %>% 
    map_chr(hex_to_bin) %>% 
    paste0(collapse = "")
}
expect_equal(hash_to_bin('a0c2017'), "1010000011000010000000010111")
```

``` r
expect_equal(str_sub(hash_to_bin(knot_hash("flqrgnkx-0")), 1, 8), "11010100")
```

``` r
generate_all <- function(key) {
  keys <- paste(key, 0:127, sep = "-")
  keys %>% 
    map_chr(knot_hash) %>% 
    map_chr(hash_to_bin)
}

count_ones <- function(bin_vec) {
  bin_vec %>% 
    str_count("1") %>% 
    sum()
}
expect_equal(generate_all("flqrgnkx") %>% count_ones(), 8108)
```

``` r
my_grid <- generate_all("hfdlxzhv")
count_ones(my_grid)
```

    ## [1] 8230

Part 2
------

Now we have to find the contiguous regions with ones.

``` r
grid_long <- expand.grid(x = 1:128, y =1:128) %>% 
  mutate(val = str_sub(my_grid[y], x, x))
head(grid_long)
```

    ##   x y val
    ## 1 1 1   1
    ## 2 2 1   1
    ## 3 3 1   0
    ## 4 4 1   0
    ## 5 5 1   1
    ## 6 6 1   1

As someone pointed out on reddit, we can use the logic of day 12 to count the number of programs. We'll assign each `1` to a label, then find what are the neighbouring `1s`, and finally apply the logic from day 12.

``` r
grid_long <- grid_long %>% 
  group_by(val) %>% 
  mutate(label = if_else(val == 1, row_number() - 1, NA_real_)) %>% 
  ungroup() %>% 
  mutate(label_self = label,
         label_left = lag(label, 1),
         label_above = lag(label, 128),
         label_right = lead(label, 1),
         label_under = lead(label, 128)) %>% 
  mutate(label_left = if_else(x == 1, NA_real_, label_left),
         label_right = if_else(x == 128, NA_real_, label_right)) %>% 
  filter(val == 1) %>% 
  select(starts_with("label")) %>% 
  nest(-label) %>% 
  mutate(data = map(data, unlist)) %>% 
  mutate(data = map(data, na.omit)) %>% 
  rename(program = label, links = data)
```

Let's copy the functions for day 12.

``` r
next_links <- function(prev_links, programs) {
  programs %>% 
      filter(program %in% prev_links) %>% 
      magrittr::use_series(links) %>% 
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

all_groups <- function(prog_list, programs) {
  start <- prog_list[1]
  group_count <- 0
  while(! is_empty(prog_list)) {
      connected <- rec_lookup(start, programs)
      prog_list <- setdiff(prog_list, connected)
      start <- prog_list[1]
      group_count <- group_count + 1
  }
  group_count
}
```

And now apply it to our case.

``` r
all_groups(0:max(grid_long$program), grid_long)
```

    ## [1] 1103
