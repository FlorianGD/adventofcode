Day 7
================

Part 1
------

The goal is to find the name of the program at the bottom of the pile. The input looks like below. This is an example, we are supposed to find `tknk` as the answer.

``` r
library(tidyverse)
library(stringr)
library(testthat)

example_input <-  'pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)'
```

Let's pre process the example a bit, so as we have a nice character vector with an entry for each line.

``` r
example_input <- example_input %>% 
  str_split('\\n') %>% 
  pluck(1)
example_input
```

    ##  [1] "pbga (66)"                     "xhth (57)"                    
    ##  [3] "ebii (61)"                     "havc (66)"                    
    ##  [5] "ktlj (57)"                     "fwft (72) -> ktlj, cntj, xhth"
    ##  [7] "qoyq (66)"                     "padx (45) -> pbga, havc, qoyq"
    ##  [9] "tknk (41) -> ugml, padx, fwft" "jptl (61)"                    
    ## [11] "ugml (68) -> gyxo, ebii, jptl" "gyxo (61)"                    
    ## [13] "cntj (57)"

Now, I will find all the program names and their weight.

``` r
 get_name_weight <- function(input){
   input %>% 
     str_match('([a-z]+) \\((\\d+)\\)') %>% 
     as_tibble() %>% 
     select("name" = V2, "weight" = V3)
}
prog_names <- get_name_weight(example_input)
prog_names
```

    ## # A tibble: 13 x 2
    ##     name weight
    ##    <chr>  <chr>
    ##  1  pbga     66
    ##  2  xhth     57
    ##  3  ebii     61
    ##  4  havc     66
    ##  5  ktlj     57
    ##  6  fwft     72
    ##  7  qoyq     66
    ##  8  padx     45
    ##  9  tknk     41
    ## 10  jptl     61
    ## 11  ugml     68
    ## 12  gyxo     61
    ## 13  cntj     57

Ok now, let's find the programs that are in towers, i.e. the programs whose names are after the `"->"` in the input. I flatten the result, so as to have a single character vector.

``` r
get_towers <- function(input){
  input %>% 
    `[`(str_detect(., '->')) %>%  # Get only the lines where there is a '->'
    str_split('-> ') %>%          # Split the line
    map(2) %>%                    # Get the values after the '-> '
    str_split(', ') %>%           # Split the lines after commas
    flatten_chr()                 # Flatten the result
}
prog_in_towers <- get_towers(example_input)
prog_in_towers
```

    ##  [1] "ktlj" "cntj" "xhth" "pbga" "havc" "qoyq" "ugml" "padx" "fwft" "gyxo"
    ## [11] "ebii" "jptl"

Now, the bottom of the tower is the name that is not in the above list.

``` r
prog_names %>% 
  filter(!(name %in% prog_in_towers)) %>% 
  `[[`('name')
```

    ## [1] "tknk"

That works! Let's make it a function for use with the real file.

``` r
get_bottom <- function(input){
  input %>% 
    get_name_weight() %>% 
    filter(!(name %in% get_towers(input))) %>% 
    `[[`('name')
}
expect_equal(get_bottom(example_input), "tknk")
```

Now with the file

``` r
my_input <- read_lines("day7-input.txt")

get_bottom(my_input)
```

    ## [1] "vmpywg"

Part 2
------

Now we have to find which tower is not balanced. The weights of all the towers above one disk must be equal. We already have the weight of each individual program, in our example in `prog_names`.

``` r
prog_names
```

    ## # A tibble: 13 x 2
    ##     name weight
    ##    <chr>  <chr>
    ##  1  pbga     66
    ##  2  xhth     57
    ##  3  ebii     61
    ##  4  havc     66
    ##  5  ktlj     57
    ##  6  fwft     72
    ##  7  qoyq     66
    ##  8  padx     45
    ##  9  tknk     41
    ## 10  jptl     61
    ## 11  ugml     68
    ## 12  gyxo     61
    ## 13  cntj     57
