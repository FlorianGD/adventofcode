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
library(testthat)
```

Let's first focus on detecting the groups correctly, using empty groups.

``` r
ex_empty <- c('{}', '{{{}}}', '{{},{}}', '{{{},{},{{}}}}')

detect_group_empty <- function(string){
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

ex_empty_groups <- map(ex_empty, detect_group_empty)
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
score <- function(group_list) {
  sum(map_dbl(group_list, "level"))
}
map(ex_empty_groups, score)
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

Ok, so far so good, we have the expected scores. Now we have to handle the garbage. I think I will change my for loop to a while, so I can make the cursor move further away on the string following the rules. I add a garbage flag that tells us if we are inside some garbage

``` r
detect_group_garbage <- function(string){
  groups = list()
  group = 1
  level = 1
  i = 1
  garbage_flag = FALSE
  while (i <= nchar(string)) {
    letter <- str_sub(string, i, i)

    # If there is a !, ignore the next character
    if(letter == "!" & garbage_flag){
      i = i + 2
    }
    # If there is garbage, set the garbage flage to TRUE
    else if(letter == "<" & ! garbage_flag){
      garbage_flag = TRUE
      i = i + 1
    }
    else if(letter == ">" & garbage_flag){
      garbage_flag = FALSE
      i = i + 1
    }
    else if(letter == "{" & ! garbage_flag){
      groups = append(groups, list(list(group = group, level = level)))
      level = level + 1
      i = i + 1
    }
    else if (letter == "}" & ! garbage_flag) {
      level = level - 1
      i = i + 1
    }
    else if (letter == "," & ! garbage_flag) {
      group = group + 1
      i = i + 1
    }
    else{
      i = i + 1
    }
  }
  groups
}

expect_identical(map(ex_empty, detect_group_garbage), ex_empty_groups)
```

Now with other examples.

``` r
ex_garbage <- c('{<{},{},{{}}>}', '{<a>,<a>,<a>,<a>}', 
                '{{<a>},{<a>},{<a>},{<a>}}', '{{<!>},{<!>},{<!>},{<a>}}')
ex_garbage_groups <- map(ex_garbage, detect_group_garbage)
```

The number of groups:

``` r
map(ex_garbage_groups, length)
```

    ## [[1]]
    ## [1] 1
    ## 
    ## [[2]]
    ## [1] 1
    ## 
    ## [[3]]
    ## [1] 5
    ## 
    ## [[4]]
    ## [1] 2

It is good! And now the scores:

``` r
expect_equal(score(detect_group_garbage('{}')), 1)
expect_equal(score(detect_group_garbage('{{{}}}')), 6)
expect_equal(score(detect_group_garbage('{{},{}}')), 5)
expect_equal(score(detect_group_garbage('{{{},{},{{}}}}')), 16)
expect_equal(score(detect_group_garbage('{<a>,<a>,<a>,<a>}')), 1)
expect_equal(score(detect_group_garbage(' {{<ab>},{<ab>},{<ab>},{<ab>}}')), 9)
expect_equal(score(detect_group_garbage('{{<!!>},{<!!>},{<!!>},{<!!>}}')), 9)
expect_equal(score(detect_group_garbage('{{<a!>},{<a!>},{<a!>},{<ab>}}')), 3)
```

Good!

Now the real deal:

``` r
my_input <- read_file("day09-input.txt")
score(detect_group_garbage(my_input))
```

    ## [1] 11898

YES!

Part 2
------

Now we have to count the non canceled character (and without the `!` canceling it) inside of the garbage.

I will modifiy the `detect_group_garbage` function to make this count.

``` r
count_garbage <- function(string){
  count = 0
  i = 1
  garbage_flag = FALSE
  while (i <= nchar(string)) {
    letter <- str_sub(string, i, i)

    # If there is a !, ignore the next character
    if(letter == "!" & garbage_flag){
      i = i + 2
    }
    # If there is garbage, set the garbage flage to TRUE
    else if(letter == "<" & ! garbage_flag){
      garbage_flag = TRUE
      i = i + 1
    }
    else if(letter == ">" & garbage_flag){
      garbage_flag = FALSE
      i = i + 1
    }
    else if (garbage_flag) {
      count = count + 1
      i = i + 1
    }
    else{
      i = i + 1
    }
  }
  count
}
```

Let's do some test

``` r
expect_equal(count_garbage('<>'), 0)
expect_equal(count_garbage('<random characters>'), 17)
expect_equal(count_garbage('<<<<>'), 3)
expect_equal(count_garbage('<{!>}>'), 2)
expect_equal(count_garbage('<!!>'), 0)
expect_equal(count_garbage('<!!!>>'), 0)
expect_equal(count_garbage('<{o"i!a,<{i<a>'), 10)
```

Seems good!

``` r
count_garbage(my_input)
```

    ## [1] 5601

That's the good answer!
