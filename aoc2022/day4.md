– Day 4 –
================

## --- Day 4: Camp Cleanup ---

Space needs to be cleared before the last supplies can be unloaded from
the ships, and so several Elves have been assigned the job of cleaning
up sections of the camp. Every section has a unique *ID number*, and
each Elf is assigned a range of section IDs.

However, as some of the Elves compare their section assignments with
each other, they’ve noticed that many of the assignments *overlap*. To
try to quickly find overlaps and reduce duplicated effort, the Elves
pair up and make a *big list of the section assignments for each pair*
(your puzzle input).

For example, consider the following list of section assignment pairs:

    2-4,6-8
    2-3,4-5
    5-7,7-9
    2-8,3-7
    6-6,4-6
    2-6,4-8

For the first few pairs, this list means:

- Within the first pair of Elves, the first Elf was assigned sections
  `2-4` (sections `2`, `3`, and `4`), while the second Elf was assigned
  sections `6-8` (sections `6`, `7`, `8`).

- The Elves in the second pair were each assigned two sections.

- The Elves in the third pair were each assigned three sections: one got
  sections `5`, `6`, and `7`, while the other also got `7`, plus `8` and
  `9`.

This example list uses single-digit section IDs to make it easier to
draw; your actual list might contain larger numbers. Visually, these
pairs of section assignments look like this:

    .234.....  2-4
    .....678.  6-8

    .23......  2-3
    ...45....  4-5

    ....567..  5-7
    ......789  7-9

    .2345678.  2-8
    ..34567..  3-7

    .....6...  6-6
    ...456...  4-6

    .23456...  2-6
    ...45678.  4-8

Some of the pairs have noticed that one of their assignments *fully
contains* the other. For example, `2-8` fully contains `3-7`, and `6-6`
is fully contained by `4-6`. In pairs where one assignment fully
contains the other, one Elf in the pair would be exclusively cleaning
sections their partner will already be cleaning, so these seem like the
most in need of reconsideration. In this example, there are *`2`* such
pairs.

*In how many assignment pairs does one range fully contain the other?*

``` r
library(tidyverse)
```

``` r
assignment_pairs <- read_csv("data/input4a.txt", 
  show_col_types = FALSE,
  col_names = c("pair1", "pair2")) %>%
  separate(pair1, c("start1", "stop1"), sep = "-") %>%
  separate(pair2, c("start2", "stop2"), sep = "-")
```

The key to creating a list column is using map\*.

``` r
#' Cleaning assignments
#' 
#' @return a numeric vector for each elf and a list column in the dataframe
#'  
cleaning_assignments <- function(start, stop) {
  map2(start, stop, seq)  
}

# unit tests 
cleaning_assignments(2, 4)
```

    [[1]]
    [1] 2 3 4

``` r
cleaning_assignments(c(2, 6), c(4, 8))
```

    [[1]]
    [1] 2 3 4

    [[2]]
    [1] 6 7 8

map2_lgl ensures the output is a logical vector

``` r
#' Is either assignment fully contained in the other

fully_contains <- function(assignment1, assignment2) {
  map2_lgl(
  assignment1, 
  assignment2, 
  function(.x, .y) {
    setequal(union(.x, .y), .x) |
    setequal(union(.x, .y), .y)
  })
}

# unit tests
fully_contains(list(1:4), list(2:3)) == c(TRUE)
```

    [1] TRUE

``` r
fully_contains(list(1:4), list(1:5)) == c(TRUE)
```

    [1] TRUE

``` r
fully_contains(list(1:4), list(4:5)) == c(FALSE)
```

    [1] TRUE

``` r
fully_contains(list(1:4, 1:4), list(2:3, 1:5)) == c(TRUE, TRUE)
```

    [1] TRUE TRUE

``` r
assignment_pairs %>% 
  mutate(
    assignments1 = cleaning_assignments(start1, stop1),
    assignments2 = cleaning_assignments(start2, stop2)
  ) %>%
  mutate(fully_contained = fully_contains(assignments1, assignments2)) %>%
  
summarize(
  Answer = sum(fully_contained)
)
```

    # A tibble: 1 x 1
      Answer
       <int>
    1    511

## --- Part Two ---

It seems like there is still quite a bit of duplicate work planned.
Instead, the Elves would like to know the number of pairs that *overlap
at all*.

In the above example, the first two pairs (`2-4,6-8` and `2-3,4-5`)
don’t overlap, while the remaining four pairs (`5-7,7-9`, `2-8,3-7`,
`6-6,4-6`, and `2-6,4-8`) do overlap:

- `5-7,7-9` overlaps in a single section, `7`.

- `2-8,3-7` overlaps all of the sections `3` through `7`.

- `6-6,4-6` overlaps in a single section, `6`.

- `2-6,4-8` overlaps in sections `4`, `5`, and `6`.

So, in this example, the number of overlapping assignment pairs is
*`4`*.

*In how many assignment pairs do the ranges overlap?*

``` r
#' Is either assignment fully contained in the other

overlap <- function(assignment1, assignment2) {
  map2_lgl(
  assignment1, 
  assignment2, 
  function(.x, .y) {
    length(intersect(.x, .y)) != 0
  })
}

# unit tests
overlap(list(1:4), list(2:3)) == c(TRUE)
```

    [1] TRUE

``` r
overlap(list(1:4), list(1:5)) == c(TRUE)
```

    [1] TRUE

``` r
overlap(list(1:4), list(4:5)) == c(TRUE)
```

    [1] TRUE

``` r
overlap(list(2:4, 2:4), list(2:3, 1:5)) == c(TRUE, TRUE)
```

    [1] TRUE TRUE

``` r
overlap(list(2:4, 2:4), list(5:6, 1:1)) == c(FALSE, FALSE)
```

    [1] TRUE TRUE

``` r
assignment_pairs %>% 
  mutate(
    assignments1 = cleaning_assignments(start1, stop1),
    assignments2 = cleaning_assignments(start2, stop2)
  ) %>%
  mutate(overlaps = overlap(assignments1, assignments2)) %>%
  
summarize(
  Answer = sum(overlaps)
)
```

    # A tibble: 1 x 1
      Answer
       <int>
    1    821
