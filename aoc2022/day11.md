– Day 11 Monkey Business –
================

## Pseudo Code

Puzzle inputs provides

1.  The worry levels of items each monkey is holding
2.  How to build a monkey throwing function

Initialize monkey state

- What each monkey is holding, and the number of items previously
  inspected

  - Find line starting with “^Monkey” and set monkey number

  - Read next line and set vector with worry levels

Update_monkey_state

- A function that updates the monkey_state after each throw

- Write a function factory with the following parameters and returns a
  function for

  - monkey number

  - operation: \* or +

  - value to use with worry level and operation

  - divisible number

  - true monkey

  - false monkey

Build monkey throw functions

- Iterates over what the monkey is holding

- Calls the update function after each throw

Main

- Iterate over 20 rounds

- Within each round iterate over the monkeys

Solution

- Find monkeys that inspected most and next to most items and multiply

``` r
library(testthat)
library(tidyverse)
```

## Puzzle Input

``` r
monkeys_raw <- read_lines("data/input11a_test.txt") %>%
split(factor(0:(length(.) - 1) %/% 7))

monkeys_raw <- set_names(monkeys_raw, 0:(length(monkeys_raw) - 1))
```

## Monkey Behavior – Function Factory

A function factory (https://adv-r.hadley.nz/function-factories.html) is
a function that makes functions. `make_monkey` returns a function that
behaves like the monkeys in the example (and puzzle input).

``` r
# an inside function to make a monkey with certain behavior
make_monkey <- function(value, op, div, t, f) {
  function(worry) {
    new_worry <- floor(op(worry, value) / 3)
    receiver <- if_else(new_worry %% div == 0, t, f) %>% as.character()
    return(list(receiver, new_worry))
  }
}

# Tests from the example
# expect_identical is silent if the test passes

monkey0 <- make_monkey(19, `*`, 23, 2, 3)
expect_identical(monkey0(79), list("3", 500))
expect_identical(monkey0(98), list("3", 620))

monkey1 <- make_monkey(6, `+`, 19, 2, 0)
expect_identical(monkey1(54), list("0", 20))
expect_identical(monkey1(65), list("0", 23))
expect_identical(monkey1(75), list("0", 27))
expect_identical(monkey1(74), list("0", 26))

monkey2 <- make_monkey(2, `^`, 13, 1, 3)  # old * old = old ^ 2
expect_identical(monkey2(79), list("1", 2080)) 
expect_identical(monkey2(60), list("3", 1200))
expect_identical(monkey2(97), list("3", 3136))

monkey3 <- make_monkey(3, `+`, 17, 0, 1)
expect_identical(monkey3(74), list("1", 25))
expect_identical(monkey3(500), list("1", 167))
expect_identical(monkey3(620), list("1", 207))

#' Parse monkeys_raw to get the parameters and then 
#' build the monkey behavior using the above function

build_monkey <- function(monkeys_raw) {
  
if(str_detect(monkeys_raw[3], "old \\* old")) {
  op <- "^" %>% as.symbol()
  value <- 2
} else {
  op <- str_extract(monkeys_raw[3], "[*+]") %>% as.symbol()
  value <- str_extract(monkeys_raw[3], "\\d+$") %>% as.numeric()
}
div <- str_extract(monkeys_raw[4], "\\d+$") %>% as.numeric()
t   <- str_extract(monkeys_raw[5], "\\d$")  %>% as.numeric()
f   <- str_extract(monkeys_raw[6], "\\d$")  %>% as.numeric()

make_monkey(value, eval(op), div, t, f)

}

monkey0 <- build_monkey(monkeys_raw[["0"]])
expect_identical(monkey0(79), list("3", 500))
expect_identical(monkey0(98), list("3", 620))

monkey1 <- build_monkey(monkeys_raw[["1"]])
expect_identical(monkey1(54), list("0", 20))
expect_identical(monkey1(65), list("0", 23))
expect_identical(monkey1(75), list("0", 27))
expect_identical(monkey1(74), list("0", 26))

monkey2 <- build_monkey(monkeys_raw[["2"]])  # old * old = old ^ 2
expect_identical(monkey2(79), list("1", 2080)) 
expect_identical(monkey2(60), list("3", 1200))
expect_identical(monkey2(97), list("3", 3136))

monkey3 <- build_monkey(monkeys_raw[["3"]])
expect_identical(monkey3(74), list("1", 25))
expect_identical(monkey3(500), list("1", 167))
expect_identical(monkey3(620), list("1", 207))
```

## Who Has What?

``` r
parse_starting_items <- function(monkey_raw) {
  str_remove(monkey_raw[[2]], "Starting items: ") %>% str_split(", ") %>%
  unlist() %>% as.numeric()
}

# Example
who_what_0 <- map(monkeys_raw, parse_starting_items)

# Keep track of this in a separate list to avoid list depth hell
init_inspected <- function() {
inspected <<- rep(0, length(monkeys_raw)) %>% set_names(as.character(0:(length(monkeys_raw) - 1)))
}
init_inspected()
inspected
```

    0 1 2 3 
    0 0 0 0 

### Throw and Update

``` r
monkey_fns <- c("0" = monkey0, "1" = monkey1, "2" = monkey2, "3" = monkey3)
monkey_fns[["0"]](79)
```

    [[1]]
    [1] "3"

    [[2]]
    [1] 500

``` r
monkey_fns[["0"]](98)
```

    [[1]]
    [1] "3"

    [[2]]
    [1] 620

``` r
throw_update <- function(who_what, thrower,
                         .monkey_fns = monkey_fns) {
  
  # Comment out after testing
  # who_what <- throw_update(who_what_0, "0")
  # thrower <- "0"
  # .monkey_fns <- monkey_fns
  # .monkey_fns[[thrower]](who_what_0[[thrower]][1])
  
  throw <- .monkey_fns[[thrower]](who_what[[thrower]][1])
    receiver <- throw[[1]]
    value <- throw[[2]]

  # receiver
  who_what[receiver][[1]] <- c(who_what[receiver][[1]], value)
  # remove from throwing monkey
  who_what[thrower][[1]] <- tail(who_what[[thrower]], -1)
  
  inspected[thrower] <<- inspected[thrower] + 1 
  
  who_what

}

# Round 1 - thrower and number of items thrower holds
init_inspected()
who_what_1 <- reduce(rep("0", length(who_what_0[["0"]])), throw_update, .init = who_what_0)
who_what_2 <- reduce(rep("1", length(who_what_1[["1"]])), throw_update, .init = who_what_1)
who_what_3 <- reduce(rep("2", length(who_what_2[["2"]])), throw_update, .init = who_what_2)
who_what_4 <- reduce(rep("3", length(who_what_3[["3"]])), throw_update, .init = who_what_3)
who_what_4
```

    $`0`
    [1] 20 23 27 26

    $`1`
    [1] 2080   25  167  207  401 1046

    $`2`
    numeric(0)

    $`3`
    numeric(0)

``` r
inspected
```

    0 1 2 3 
    2 4 3 5 

``` r
# Use the magrittr . pronoun
init_inspected()
reduce(rep("0", length(who_what_0[["0"]])), throw_update, .init = who_what_0) %>%
reduce(rep("1", length(.[["1"]])), throw_update, .init = .) %>% 
reduce(rep("2", length(.[["2"]])), throw_update, .init = .) %>% 
reduce(rep("3", length(.[["3"]])), throw_update, .init = .)
```

    $`0`
    [1] 20 23 27 26

    $`1`
    [1] 2080   25  167  207  401 1046

    $`2`
    numeric(0)

    $`3`
    numeric(0)

``` r
inspected
```

    0 1 2 3 
    2 4 3 5 

``` r
# Do 20 rounds

do_round <- function(who_what_0, round) {
  reduce(rep("0", length(who_what_0[["0"]])), throw_update, .init = who_what_0) %>%
  reduce(rep("1", length(.[["1"]])), throw_update, .init = .) %>% 
  reduce(rep("2", length(.[["2"]])), throw_update, .init = .) %>% 
  reduce(rep("3", length(.[["3"]])), throw_update, .init = .)
}
```

## Part 1 - Example

``` r
init_inspected()
reduce(1:20, do_round, .init = who_what_0)
```

    $`0`
    [1] 10 12 14 26 34

    $`1`
    [1] 245  93  53 199 115

    $`2`
    numeric(0)

    $`3`
    numeric(0)

``` r
sort(inspected, decreasing = TRUE)[1] * sort(inspected, decreasing = TRUE)[2]
```

        3 
    10605 

## Part 2 - Example (1e5 rounds!)

No longer divided by 3

``` r
make_monkey <- function(value, op, div, t, f) {
  function(worry) {
    new_worry <- floor(op(worry, value) / 1) # <<<- dividing by 1
    receiver <- if_else(new_worry %% div == 0, t, f) %>% as.character()
    return(list(receiver, new_worry))
  }
}

build_monkey <- function(monkeys_raw) {
  
if(str_detect(monkeys_raw[3], "old \\* old")) {
  op <- "^" %>% as.symbol()
  value <- 2
} else {
  op <- str_extract(monkeys_raw[3], "[*+]") %>% as.symbol()
  value <- str_extract(monkeys_raw[3], "\\d+$") %>% as.numeric()
}
div <- str_extract(monkeys_raw[4], "\\d+$") %>% as.numeric()
t   <- str_extract(monkeys_raw[5], "\\d$")  %>% as.numeric()
f   <- str_extract(monkeys_raw[6], "\\d$")  %>% as.numeric()

make_monkey(value, eval(op), div, t, f)

}
```

``` r
monkeys_raw <- read_lines("data/input11a_test.txt") %>%
split(factor(0:(length(.) - 1) %/% 7))
monkeys_raw <- set_names(monkeys_raw, 0:(length(monkeys_raw) - 1))
names(monkeys_raw)
```

    [1] "0" "1" "2" "3"

``` r
who_what_0 <- map(monkeys_raw, parse_starting_items)

monkey0 <- build_monkey(monkeys_raw[["0"]])
monkey1 <- build_monkey(monkeys_raw[["1"]])
monkey2 <- build_monkey(monkeys_raw[["2"]])
monkey3 <- build_monkey(monkeys_raw[["3"]])

monkey_fns <- c("0" = monkey0, "1" = monkey1, "2" = monkey2, "3" = monkey3)

do_round <- function(who_what_0, round) {
  reduce(rep("0", length(who_what_0[["0"]])), throw_update, .init = who_what_0) %>%
  reduce(rep("1", length(.[["1"]])), throw_update, .init = .) %>% 
  reduce(rep("2", length(.[["2"]])), throw_update, .init = .) %>% 
  reduce(rep("3", length(.[["3"]])), throw_update, .init = .) 
}

init_inspected()
reduce(1:1, do_round, .init = who_what_0)
```

    $`0`
    [1] 60 71 81 80

    $`1`
    [1]   77 1504 1865 6244 3603 9412

    $`2`
    numeric(0)

    $`3`
    numeric(0)

``` r
inspected
```

    0 1 2 3 
    2 4 3 6 

``` r
init_inspected()
reduce(1:20, do_round, .init = who_what_0)
```

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    $`0`
    [1] 2.694442e+13 1.126905e+28 7.140117e+28 1.375887e+29

    $`1`
    [1] 1.439401e+102 3.469072e+149  4.383712e+14  4.996819e+14  5.774545e+16
    [6]  9.377364e+15

    $`2`
    numeric(0)

    $`3`
    numeric(0)

``` r
inspected
```

      0   1   2   3 
     99  97  13 101 

``` r
sort(inspected, decreasing = TRUE)[1] * sort(inspected, decreasing = TRUE)[2]
```

       3 
    9999 

## Part 1 - Solution

``` r
monkeys_raw <- read_lines("data/input11a.txt") %>%
split(factor(0:(length(.) - 1) %/% 7))
monkeys_raw <- set_names(monkeys_raw, 0:(length(monkeys_raw) - 1))
names(monkeys_raw)
```

    [1] "0" "1" "2" "3" "4" "5" "6" "7"

``` r
who_what_0 <- map(monkeys_raw, parse_starting_items)

monkey0 <- build_monkey(monkeys_raw[["0"]])
monkey1 <- build_monkey(monkeys_raw[["1"]])
monkey2 <- build_monkey(monkeys_raw[["2"]])
monkey3 <- build_monkey(monkeys_raw[["3"]])
monkey4 <- build_monkey(monkeys_raw[["4"]])
monkey5 <- build_monkey(monkeys_raw[["5"]])
monkey6 <- build_monkey(monkeys_raw[["6"]])
monkey7 <- build_monkey(monkeys_raw[["7"]])

monkey_fns <- c("0" = monkey0, "1" = monkey1, "2" = monkey2, "3" = monkey3, "4" = monkey4, "5" = monkey5, "6" = monkey6, "7" = monkey7)

do_round <- function(who_what_0, round) {
  reduce(rep("0", length(who_what_0[["0"]])), throw_update, .init = who_what_0) %>%
  reduce(rep("1", length(.[["1"]])), throw_update, .init = .) %>% 
  reduce(rep("2", length(.[["2"]])), throw_update, .init = .) %>% 
  reduce(rep("3", length(.[["3"]])), throw_update, .init = .) %>%
  reduce(rep("4", length(.[["4"]])), throw_update, .init = .) %>%
  reduce(rep("5", length(.[["5"]])), throw_update, .init = .) %>%
  reduce(rep("6", length(.[["6"]])), throw_update, .init = .) %>%
  reduce(rep("7", length(.[["7"]])), throw_update, .init = .) 
}

init_inspected()
reduce(1:20, do_round, .init = who_what_0)
```

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    Warning in if_else(new_worry%%div == 0, t, f): probable complete loss of
    accuracy in modulus

    $`0`
     [1] 3.312851e+10 2.504623e+19 1.432210e+10 1.010369e+23 9.273636e+19
     [6] 4.742550e+24 3.047089e+11 1.937095e+18 3.273525e+13 9.686279e+10
    [11] 2.177158e+13 4.543348e+12 3.376853e+12 5.109465e+12

    $`1`
     [1] 1.038027e+33 6.623150e+23 1.116901e+25 3.312851e+10 3.312851e+10
     [6] 1.733474e+20 5.283740e+23 1.359910e+20 4.648008e+12 1.131977e+15
    [11] 1.471570e+16 7.962883e+14 7.866064e+13

    $`2`
    [1] 1.984854e+35 1.447888e+29

    $`3`
    [1] 7.054570e+26 7.156672e+18 9.707558e+16

    $`4`
    [1] 1.483470e+12 7.840204e+16 3.249326e+11 3.249326e+11

    $`5`
    numeric(0)

    $`6`
    numeric(0)

    $`7`
    numeric(0)

``` r
sort(inspected, decreasing = TRUE)[1] * sort(inspected, decreasing = TRUE)[2]
```

        2 
    81468 

## 
