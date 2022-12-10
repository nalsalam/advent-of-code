– Day 10 –
================

``` r
library(tidyverse)
```

Functions

``` r
build_stack <- function(file) {

  read_fwf(file = file, 
    col_positions = fwf_widths(widths = c(4, 4),
     col_names = c("op", "x")), 
    show_col_types = "spec") %>%

  mutate(
    curr_cycles = if_else(op == "addx", 2, 1), # number of current cycles
    CYCLE = cumsum(curr_cycles), # completed cycles at end of current cycles, 
    x = if_else(is.na(x), 0, x), # addition to x at end of current cycles
    X = 1 + cumsum(x) # X at the end of the current cycle
  )
}
```

## Part 1 - Example

``` r
# Need value of X DURING CYCLE 20, 40, 100, etc.
# which is the value of X at the end of the prior CYCLE

stack <- build_stack("data/input10a_test.txt")
max(stack$CYCLE)
```

    [1] 240

``` r
map_dfr(seq(20, 220, 40), ~ stack %>% filter(CYCLE == max(stack$CYCLE[stack$CYCLE < .x])) %>% mutate(SS = .x * X)) %>%

pull(SS) %>% sum()
```

    [1] 13140

## Part 1

``` r
stack <- build_stack("data/input10a.txt")
max(stack$CYCLE)
```

    [1] 240

``` r
map_dfr(seq(20, 220, 40), ~ stack %>% filter(CYCLE == max(stack$CYCLE[stack$CYCLE < .x])) %>% mutate(SS = .x * X)) %>%
pull(SS) %>% sum()
```

    [1] 14360

## Part 2
