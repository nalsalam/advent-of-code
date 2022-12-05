– Day 5 –
================

## --- Day 5: Supply Stacks ---

The expedition can depart as soon as the final supplies have been
unloaded from the ships. Supplies are stored in stacks of marked
*crates*, but because the needed supplies are buried under many other
crates, the crates need to be rearranged.

The ship has a *giant cargo crane* capable of moving crates between
stacks. To ensure none of the crates get crushed or fall over, the crane
operator will rearrange them in a series of carefully-planned steps.
After the crates are rearranged, the desired crates will be at the top
of each stack.

The Elves don’t want to interrupt the crane operator during this
delicate procedure, but they forgot to ask her *which* crate will end up
where, and they want to be ready to unload them as soon as possible so
they can embark.

They do, however, have a drawing of the starting stacks of crates *and*
the rearrangement procedure (your puzzle input). For example:

        [D]    
    [N] [C]    
    [Z] [M] [P]
     1   2   3 

    move 1 from 2 to 1
    move 3 from 1 to 3
    move 2 from 2 to 1
    move 1 from 1 to 2

In this example, there are three stacks of crates. Stack 1 contains two
crates: crate `Z` is on the bottom, and crate `N` is on top. Stack 2
contains three crates; from bottom to top, they are crates `M`, `C`, and
`D`. Finally, stack 3 contains a single crate, `P`.

Then, the rearrangement procedure is given. In each step of the
procedure, a quantity of crates is moved from one stack to a different
stack. In the first step of the above rearrangement procedure, one crate
is moved from stack 2 to stack 1, resulting in this configuration:

    [D]        
    [N] [C]    
    [Z] [M] [P]
     1   2   3 

In the second step, three crates are moved from stack 1 to stack 3.
Crates are moved *one at a time*, so the first crate to be moved (`D`)
ends up below the second and third crates:

            [Z]
            [N]
        [C] [D]
        [M] [P]
     1   2   3

Then, both crates are moved from stack 2 to stack 1. Again, because
crates are moved *one at a time*, crate `C` ends up below crate `M`:

            [Z]
            [N]
    [M]     [D]
    [C]     [P]
     1   2   3

Finally, one crate is moved from stack 1 to stack 2:

            [Z]
            [N]
            [D]
    [C] [M] [P]
     1   2   3

The Elves just need to know *which crate will end up on top of each
stack*; in this example, the top crates are `C` in stack 1, `M` in stack
2, and `Z` in stack 3, so you should combine these together and give the
Elves the message *`CMZ`*.

*After the rearrangement procedure completes, what crate ends up on top
of each stack?*

``` r
library(testthat)
library(tidyverse)
```

9 stacks, each a list from top (first) to bottom (last).

``` r
read_stacks <- function(file, n) {
  read_fwf(
    file = file, 
    col_positions = fwf_empty(file, n = n),
    n_max = n,
    show_col_types = FALSE
  ) %>%
  mutate(across(.fns = ~ str_remove(.x, "\\]") %>% str_remove("\\["))) %>%
  map(~.x[!is.na(.x)])
}

stacks_init <- read_stacks("data/input5a.txt", 8)
stacks_init
```

    $X1
    [1] "T" "V" "J" "W" "N" "R" "M" "S"

    $X2
    [1] "V" "C" "P" "Q" "J" "D" "W" "B"

    $X3
    [1] "P" "R" "D" "H" "F" "J" "B"

    $X4
    [1] "D" "N" "M" "B" "P" "R" "F"

    $X5
    [1] "B" "T" "P" "R" "V" "H"

    $X6
    [1] "T" "P" "B" "C"

    $X7
    [1] "L" "P" "R" "J" "B"

    $X8
    [1] "W" "B" "Z" "T" "L" "S" "C" "N"

    $X9
    [1] "G" "S" "L"

``` r
stacks_init_test <- read_stacks("data/input5a_test.txt", 3)
stacks_init_test
```

    $X1
    [1] "N" "Z"

    $X2
    [1] "D" "C" "M"

    $X3
    [1] "P"

The moves to be executed.

``` r
read_moves <- function(file, skip) {
  read_lines(
    file = file, 
    skip = skip) %>%
  str_remove("move ") %>%
  str_remove("from ") %>%
  str_remove("to ") %>%
  as_tibble() %>%
  separate(value, c("n", "origin", "dest")) %>%
  mutate(across(.fns = as.numeric))
}

moves_n <- read_moves("data/input5a.txt", skip = 10)
sum(moves_n$n)
```

    [1] 2416

``` r
head(moves_n)
```

    # A tibble: 6 x 3
          n origin  dest
      <dbl>  <dbl> <dbl>
    1     7      3     9
    2     6      2     1
    3     2      4     8
    4    10      8     4
    5     1      2     4
    6    15      4     1

``` r
tail(moves_n)
```

    # A tibble: 6 x 3
          n origin  dest
      <dbl>  <dbl> <dbl>
    1     7      1     5
    2     6      5     6
    3     3      1     2
    4     5      7     8
    5    13      2     8
    6     9      6     3

``` r
moves_1 <- map_dfr(moves_n, rep, moves_n$n) %>% select(origin, dest)
nrow(moves_1)
```

    [1] 2416

``` r
moves_n_test <- read_moves("data/input5a_test.txt", skip = 5)
sum(moves_n_test$n)
```

    [1] 7

``` r
moves_1_test <- map_dfr(moves_n_test, rep, moves_n_test$n) %>% select(origin, dest)
nrow(moves_1_test)
```

    [1] 7

The 9000 crane moves one crate at a time.

``` r
move_9000 <- function(origin, dest, n, stacks) {
  new_origin <- tail(stacks[[origin]], length(stacks[[origin]]) - n)
  # NB: rev()
  new_dest <- append(rev(head(stacks[[origin]], n)), stacks[[dest]]) 
  stacks[[dest]] <- new_dest
  stacks[[origin]] <- new_origin
  stacks
}

expect_identical(
  move_9000(1, 2, 1, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
  list(X1 = c("B"), X2 = c("A", "C", "D"), X3 = c("X", "Y", "Z")))
expect_identical(
  move_9000(2, 1, 2, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
  list(X1 = c("D", "C", "A", "B"), X2 = character(0), X3 = c("X", "Y", "Z")))
expect_identical(
  move_9000(3, 1, 3, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("Z", "Y", "X", "A", "B"), X2 = c("C", "D"), X3 = character(0)))
```

Similar to move_9000(), does not need the `n` parameter, but requires
the `move_1` data. I created this because I thought I could use it with
`Reduce` but I did not figure that out.

``` r
move <- function(origin, dest, stacks) {
  new_origin <- tail(stacks[[origin]], length(stacks[[origin]]) - 1)
  new_dest <- append(head(stacks[[origin]], 1), stacks[[dest]])
  stacks[[dest]] <- new_dest
  stacks[[origin]] <- new_origin
  stacks
}

expect_identical(
  move(1, 2, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("B"), X2 = c("A", "C", "D"), X3 = c("X", "Y", "Z")))
expect_identical(
  move(2, 1, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("C", "A", "B"), X2 = c("D"), X3 = c("X", "Y", "Z")))
expect_identical(
  move(2, 3, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("A", "B"), X2 = c("D"), X3 = c("C", "X", "Y", "Z")))
# character() is an empty (length 0) character vector
expect_identical(
  move(1, 2, list(X1 = c("A"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = character(), X2 = c("A", "C", "D"), X3 = c("X", "Y", "Z")))
```

Try out the test data

``` r
stacks <- stacks_init_test
for(i in 1:nrow(moves_1_test)) {
stacks <- move(moves_1_test$origin[i], moves_1_test$dest[i], stacks)
}
map_chr(stacks, ~ .x[1]) %>% paste0(collapse = "")
```

    [1] "CMZ"

Try out the puzzle data with move & moves_1

``` r
stacks <- stacks_init
for(i in 1:nrow(moves_1)) {
stacks <- move(moves_1$origin[i], moves_1$dest[i], stacks)
}
map_chr(stacks, ~ .x[1]) %>% paste0(collapse = "")
```

    [1] "LJSVLTWQM"

Try out the puzzle data with move_9000 & moves_n

``` r
stacks <- stacks_init
for(i in 1:nrow(moves_n)) {
stacks <- move_9000(moves_n$origin[i], moves_n$dest[i], moves_n$n[i], stacks)
}
map_chr(stacks, ~ .x[1]) %>% paste0(collapse = "")
```

    [1] "LJSVLTWQM"

--- Part Two ---

As you watch the crane operator expertly rearrange the crates, you
notice the process isn’t following your prediction.

Some mud was covering the writing on the side of the crane, and you
quickly wipe it away. The crane isn’t a CrateMover 9000 - it’s a
*CrateMover 9001*.

The CrateMover 9001 is notable for many new and exciting features: air
conditioning, leather seats, an extra cup holder, and *the ability to
pick up and move multiple crates at once*.

Again considering the example above, the crates begin in the same
configuration:

        [D]    
    [N] [C]    
    [Z] [M] [P]
     1   2   3 

Moving a single crate from stack 2 to stack 1 behaves the same as
before:

    [D]        
    [N] [C]    
    [Z] [M] [P]
     1   2   3 

However, the action of moving three crates from stack 1 to stack 3 means
that those three moved crates *stay in the same order*, resulting in
this new configuration:

            [D]
            [N]
        [C] [Z]
        [M] [P]
     1   2   3

Next, as both crates are moved from stack 2 to stack 1, they *retain
their order* as well:

            [D]
            [N]
    [C]     [Z]
    [M]     [P]
     1   2   3

Finally, a single crate is still moved from stack 1 to stack 2, but now
it’s crate `C` that gets moved:

            [D]
            [N]
            [Z]
    [M] [C] [P]
     1   2   3

In this example, the CrateMover 9001 has put the crates in a totally
different order: *`MCD`*.

Before the rearrangement process finishes, update your simulation so
that the Elves know where they should stand to be ready to unload the
final supplies. *After the rearrangement procedure completes, what crate
ends up on top of each stack?*

Remove the rev() function from the original move_9000().

``` r
move_9001 <- function(origin, dest, n, stacks) {
  new_origin <- tail(stacks[[origin]], length(stacks[[origin]]) - n)
  # new_dest <- append(rev(head(stacks[[origin]], n)), stacks[[dest]])
  new_dest <- append(head(stacks[[origin]], n), stacks[[dest]])
  stacks[[dest]] <- new_dest
  stacks[[origin]] <- new_origin
  stacks
}

expect_identical(
  move_9001(1, 2, 1, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("B"), X2 = c("A", "C", "D"), X3 = c("X", "Y", "Z")))
expect_identical(
  move_9001(2, 1, 2, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("C", "D", "A", "B"), X2 = character(0), X3 = c("X", "Y", "Z")))
expect_identical(
  move_9001(3, 1, 3, list(X1 = c("A", "B"), X2 = c("C", "D"), X3 = c("X", "Y", "Z"))),
       list(X1 = c("X", "Y", "Z", "A", "B"), X2 = c("C", "D"), X3 = character(0)))
```

``` r
stacks <- stacks_init
for(i in 1:nrow(moves_n)) {
stacks <- move_9001(moves_n$origin[i], moves_n$dest[i], moves_n$n[i], stacks)
}
map_chr(stacks, ~ .x[1]) %>% paste0(collapse = "")
```

    [1] "BRQWDBBJM"
