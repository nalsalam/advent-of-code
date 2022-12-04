– Day 3 –
================

## --- Day 3: Rucksack Reorganization ---

One Elf has the important job of loading all of the
[rucksacks](https://en.wikipedia.org/wiki/Rucksack) with supplies for
the jungle journey. Unfortunately, that Elf didn’t quite follow the
packing instructions, and so a few items now need to be rearranged.

Each rucksack has two large *compartments*. All items of a given type
are meant to go into exactly one of the two compartments. The Elf that
did the packing failed to follow this rule for exactly one item type per
rucksack.

The Elves have made a list of all of the items currently in each
rucksack (your puzzle input), but they need your help finding the
errors. Every item type is identified by a single lowercase or uppercase
letter (that is, `a` and `A` refer to different types of items).

The list of items for each rucksack is given as characters all on a
single line. A given rucksack always has the same number of items in
each of its two compartments, so the first half of the characters
represent items in the first compartment, while the second half of the
characters represent items in the second compartment.

For example, suppose you have the following list of contents from six
rucksacks:

    vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw

- The first rucksack contains the items `vJrwpWtwJgWrhcsFMMfFFhFp`,
  which means its first compartment contains the items `vJrwpWtwJgWr`,
  while the second compartment contains the items `hcsFMMfFFhFp`. The
  only item type that appears in both compartments is lowercase *`p`*.

- The second rucksack’s compartments contain `jqHRNqRjqzjGDLGL` and
  `rsFMfFZSrLrFZsSL`. The only item type that appears in both
  compartments is uppercase *`L`*.

- The third rucksack’s compartments contain `PmmdzqPrV` and `vPwwTWBwg`;
  the only common item type is uppercase *`P`*.

- The fourth rucksack’s compartments only share item type *`v`*.

- The fifth rucksack’s compartments only share item type *`t`*.

- The sixth rucksack’s compartments only share item type *`s`*.

To help prioritize item rearrangement, every item type can be converted
to a *priority*:

- Lowercase item types `a` through `z` have priorities 1 through 26.

- Uppercase item types `A` through `Z` have priorities 27 through 52.

In the above example, the priority of the item type that appears in both
compartments of each rucksack is 16 (`p`), 38 (`L`), 42 (`P`), 22 (`v`),
20 (`t`), and 19 (`s`); the sum of these is *`157`*.

Find the item type that appears in both compartments of each rucksack.
*What is the sum of the priorities of those item types?*

``` r
library(tidyverse)
```

``` r
rucksacks <- tibble(items = read_lines("data/input3a.txt"))
```

``` r
#' Find common items
#' 
#' Finds items that are in both rucksacks
#' 
#' It is intended for use on RHS of mutate, i.e.
#' it returns a vector equal in length to the number of
#' rows in the input vector
#' 
#' @return character vector of the common items
#' 
common_items <- function(items) {
  
 c1 <- substr(items, 1, str_length(items) / 2)
 c2 <- substr(items, str_length(items) / 2 + 1, str_length(items))
 
 map2(str_split(c1, ""), str_split(c2, ""), intersect) %>%
 map(paste0, collapse = "") %>% 
 unlist()
 
}

# unit tests
common_items("abcdbe") == "b"
```

    [1] TRUE

``` r
common_items("abcabd") == "ab"
```

    [1] TRUE

``` r
common_items("abcdef") == ""
```

    [1] TRUE

``` r
common_items(c("abbd", "deef")) == c("b", "e")
```

    [1] TRUE TRUE

``` r
common_items(c("abcbcd", "defefh")) == c("bc", "ef")
```

    [1] TRUE TRUE

``` r
#' Assign priority to mistakes
#' 
#' This function is vectorized over rucksacks but 
#' assumes only a single mistake in each rucksack
#' 
#' It is intended for use on RHS of mutate, i.e.
#' it returns a vector equal in length to the number of
#' rows in the input vector

assign_priority <- function(letter) {
  if_else(str_detect(letter, "^[:lower:]+$"),
    match(letter, letters),
    match(letter, LETTERS) + 26L
  )
}

# unit tests
assign_priority("a") == 1
```

    [1] TRUE

``` r
assign_priority("A") == 27
```

    [1] TRUE

``` r
assign_priority(c("a", "A")) == c(1, 27)
```

    [1] TRUE TRUE

``` r
rucksacks %>% 
  mutate(
    common = common_items(items),
    priority = assign_priority(common)
    ) %>%
  summarize(
    Answer = sum(priority)
  )
```

    # A tibble: 1 x 1
      Answer
       <int>
    1   8185

## --- Part Two ---

As you finish identifying the misplaced items, the Elves come to you
with another issue.

For safety, the Elves are divided into groups of three. Every Elf
carries a badge that identifies their group. For efficiency, within each
group of three Elves, the badge is the *only item type carried by all
three Elves*. That is, if a group’s badge is item type `B`, then all
three Elves will have item type `B` somewhere in their rucksack, and at
most two of the Elves will be carrying any other item type.

The problem is that someone forgot to put this year’s updated
authenticity sticker on the badges. All of the badges need to be pulled
out of the rucksacks so the new authenticity stickers can be attached.

Additionally, nobody wrote down which item type corresponds to each
group’s badges. The only way to tell which item type is the right one is
by finding the one item type that is *common between all three Elves* in
each group.

Every set of three lines in your list corresponds to a single group, but
each group can have a different badge item type. So, in the above
example, the first group’s rucksacks are the first three lines:

    vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg

And the second group’s rucksacks are the next three lines:

    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw

In the first group, the only item type that appears in all three
rucksacks is lowercase `r`; this must be their badges. In the second
group, their badge item type must be `Z`.

Priorities for these items must still be found to organize the sticker
attachment efforts: here, they are 18 (`r`) for the first group and 52
(`Z`) for the second group. The sum of these is *`70`*.

Find the item type that corresponds to the badges of each three-Elf
group. *What is the sum of the priorities of those item types?*

``` r
#' Find badge
#' 
#' This is a summarize function because it return a single
#' value for each group
#' 
#' I strayed from good practice because this function takes
#' a data frame as inputs, add a grouping variable and then
#' summarizes.
#' 
#' The number of rucksacks per group is a magic number as
#' is the magic name of the new variable `badge`. 

find_badges <- function(rucksacks) {
  rucksacks %>% 
    # assign group 1, 1, 1, 2, 2, 2, etc.
    mutate(group = rep(1:(n() / 3), each = 3)) %>%
    group_by(group) %>% 
    # intersect the items 
    summarize(
      badge = map2(str_split(items[1], ""), 
                   str_split(items[2], ""), 
                   intersect) %>%
              map2(str_split(items[3], ""), 
                   intersect) %>% 
              map(paste0, collapse = "") %>% 
              unlist()
    )
}

# Unit tests
find_badges(tibble(items = c("abc", "bcd", "cde"))) %>% pull(badge) == "c"
```

    [1] TRUE

``` r
find_badges(tibble(items = c("abc", "bcd", "cde", "def", "efg", "fgh"))) %>% pull(badge) == c("c", "f")
```

    [1] TRUE TRUE

``` r
rucksacks <- tibble(items = read_lines("data/input3a.txt"))

rucksacks %>% 
  find_badges() %>%
  mutate(priority = assign_priority(badge)) %>%
  summarize(Answer = sum(priority))
```

    # A tibble: 1 x 1
      Answer
       <int>
    1   2817
