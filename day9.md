Day 9
================

Part 1
------

We have to find groups in a big stream of characters. The groups are determined by `{` and `}`, separated by `,`. The groups can be nested. There is also garbage, delimited by `<` and `>`. Anything inside has to be discarded. Inside garbage, some characters are escaped: anything that comes after `!`, even `!`, should be ignored. We have to compute a score, that depends on the number of groups and their level of nesting.

My idea is to read the stream character by character and populate a (nested) list as we move. We have to know at anytime if we are in a group or in garbage, and if in garbage, we have to be careful about the `!`.

First, load some libraries that we will need.

``` r
library(tidyverse)
library(stringr)
```

Let's first focus on detecting the groups correctly, using empty groups.

``` r
ex_empty <- c('{}', '{{{}}}', '{{},{}}', '{{{},{},{{}}}}')

detect_group <- function(string){
  groups = list()
  group = 1
  level = 1
  for (i in seq(nchar(string))) {
    letter <- str_sub(string, i, i)
    
    if(letter == "{"){
      groups = append(groups, list(list(group = group, level = level)))
      level = level + 1
    }
    else if (letter == "}") {
      level = level - 1
    }
    else if (letter == ",") {
      group = group + 1
    }
  }
  groups
}

ex_empty_groups <- map(ex_empty, detect_group)
ex_empty_groups[[2]]
```

    ## [[1]]
    ## [[1]]$group
    ## [1] 1
    ## 
    ## [[1]]$level
    ## [1] 1
    ## 
    ## 
    ## [[2]]
    ## [[2]]$group
    ## [1] 1
    ## 
    ## [[2]]$level
    ## [1] 2
    ## 
    ## 
    ## [[3]]
    ## [[3]]$group
    ## [1] 1
    ## 
    ## [[3]]$level
    ## [1] 3

Now we can compute the number of group, just with the length of the list.

``` r
map(ex_empty_groups, length)
```

    ## [[1]]
    ## [1] 1
    ## 
    ## [[2]]
    ## [1] 3
    ## 
    ## [[3]]
    ## [1] 3
    ## 
    ## [[4]]
    ## [1] 6

Let's check if with this structure I can correctly compute the score for each group. Is is given by:

> Your goal is to find the total score for all groups in your input. Each group is assigned a score which is one more than the score of the group that immediately contains it. (The outermost group gets a score of 1.)

``` r
map(ex_empty_groups, ~sum(map_dbl(., "level")))
```

    ## [[1]]
    ## [1] 1
    ## 
    ## [[2]]
    ## [1] 6
    ## 
    ## [[3]]
    ## [1] 5
    ## 
    ## [[4]]
    ## [1] 16

Ok, so far so good, we have the expected score.
