Day 10
================

Part 1
------

We have a list, and given an inout we must reverse some sublists. The list is circular, meaning that if we reach the end, we must go back to the beginning. Here are the rules:

> To achieve this, begin with a list of numbers from 0 to 255, a current position which begins at 0 (the first element in the list), a skip size (which starts at 0), and a sequence of lengths (your puzzle input). Then, for each length:
>
> -   Reverse the order of that length of elements in the list, starting with the element at the current position.
> -   Move the current position forward by that length plus the skip size.
> -   Increase the skip size by one.

``` r
library(tidyverse)
library(testthat)
```

Those are the examples that we will consider first

``` r
ex_list <- 0:4
ex_lengths <- c(3, 4, 1, 5)
ex_list_size <- length(ex_list)
ex_first_step_indices <- 1:3
ex_first_step_result <- c(2, 1, 0, 3, 4)
ex_second_step_indices <- c(4, 5, 1, 2)
ex_result <- c(3, 4, 2, 1, 0)
```

Let's first make a function that given a `position`, a `length`, and the `list_size` gives the indices to consider, going back to the beginning of the list if needed.

``` r
indices <- function(pos, len, list_size) {
  ind <- (pos + 1):(pos + len) %% list_size
  ind[ind == 0] <- list_size
  ind
}

expect_equal(indices(0, ex_lengths[1], ex_list_size), ex_first_step_indices)
expect_equal(indices(3, ex_lengths[2], ex_list_size), ex_second_step_indices)
```

Now let's define a function that compute the next position.

``` r
next_pos <- function(current_pos, len, skip_size, list_size){
  (current_pos + len + skip_size) %% list_size
}
expect_equal(next_pos(0, ex_lengths[1], 0, ex_list_size), 3)
expect_equal(next_pos(3, ex_lengths[2], 1, ex_list_size), 3)
```

Let's modify the list in order to reverse the elements given by the indices.

``` r
rev_indices <- function(my_list, ind) {
  my_list[ind] <- rev(my_list[ind])
  my_list
}
expect_equal(rev_indices(ex_list, ex_first_step_indices), ex_first_step_result)
```

Let's wrap it all in a function

``` r
hash_list <- function(my_list, lengths) {
  pos <- 0
  skip_size <- 0
  list_size <- length(my_list)
  
  for (i in lengths) {
    ind <- indices(pos, i, list_size)
    my_list <- rev_indices(my_list, ind)
    pos <- next_pos(pos, i, skip_size, list_size)
    skip_size <- skip_size + 1    
  }
  my_list
}

expect_equal(hash_list(ex_list, ex_lengths), ex_result)
```

What we want is the product of the first two elements

``` r
result <- function(my_list, lengths) {
  res <- hash_list(my_list, lengths)
  res[1] * res[2]
}

expect_equal(result(ex_list, ex_lengths), 12)
```

Now with my input

``` r
my_list <- 0:255
lengths <- c(187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216)
result(my_list, lengths)
```

    ## [1] 1980

Part 2
------

It gets (over)complicated. We now have to treat the input as a string of bytes that has to be converted to numbers. And the we have to add this arbitrary list: `17, 31, 73, 47, 23`

``` r
conv_input <- function(input) {
  c(as.integer(charToRaw(input)), c(17, 31, 73, 47, 23))
}
conv_input("1,2,3")
```

    ##  [1] 49 44 50 44 51 17 31 73 47 23

We now have to do 64 rounds, in place of just one as above, but we must preserve the `position` and `skip length` from one round to the other. Let's modify the `hash` function to return also those values.

``` r
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
hash_list_2(ex_list, ex_lengths)
```

    ## $result
    ## [1] 3 4 2 1 0
    ## 
    ## $position
    ## [1] 4
    ## 
    ## $skip
    ## [1] 4

``` r
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
```

``` r
dense_hash <- function(input){
  hash <- run_hashes(input)
  matrix(hash, ncol = 16) %>% 
    as_tibble() %>% 
    summarise_all(~as.character(as.hexmode(reduce(.x, bitwXor)), width = 2)) %>% 
    paste0(collapse = "")
}

expect_equal(dense_hash(""), "a2582a3a0e66e6e86e3812dcb672a272")
expect_equal(dense_hash("AoC 2017"),"33efeb34ea91902bb2f59c9920caa6cd")
expect_equal(dense_hash("1,2,3"),"3efbe78a8d82f29979031a4aa0b16a9d")
expect_equal(dense_hash("1,2,4"), "63960835bcdc130f0b66d7ff4f6a5a8e")
```

``` r
dense_hash("187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216")
```

    ## [1] "899124dac21012ebc32e2f4d11eaec55"
