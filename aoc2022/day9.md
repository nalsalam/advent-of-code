– Day 9 –
================

``` r
library(tidyverse)
```

## Functions

``` r
move_head <- function(h_pos, dir) {
  case_when(
    dir == "R" ~ c(h_pos["x"] + 1, h_pos["y"]    ),
    dir == "D" ~ c(h_pos["x"]    , h_pos["y"] - 1),
    dir == "L" ~ c(h_pos["x"] - 1, h_pos["y"]    ),
    dir == "U" ~ c(h_pos["x"]    , h_pos["y"] + 1),
  ) %>% 
  set_names(c("x", "y")) # just in case I think of a use 
}

# the sign() function is useful here

move_tail <- function(h_pos, t_pos) {
  diff <- c(h_pos["x"] - t_pos["x"], h_pos["y"] - t_pos["y"])
  
  case_when(
     abs(diff["x"]) <= 1 & abs(diff["y"]) <= 1 ~ t_pos, # don't move
     TRUE ~ t_pos + sign(diff) # move 
  ) %>%
  set_names(c("x", "y"))
}

#' Move the head and then the tail
#' 
#' @param pos, a list of two x,y positions for the head and tail and 
#'   a list of all position, including the current, that have been visited
#' @param dir, a character, R, D, L, U to indicate the direction
#' @value, a list of wo x,y positions that the head and tail have moved to
#' 
move <- function(pos, dir) {
  new_h_pos <- move_head(pos[[1]], dir) 
  new_t_pos <- move_tail(new_h_pos, pos[[2]])
  visited <- append(pos[[3]], list(new_t_pos))
  return(list(new_h_pos, new_t_pos, visited))
}
```

## Part 1 - Example

``` r
moves <- read_delim("data/input9a_test.txt", 
                    col_names = c("dir", "steps"), 
                    show_col_types = FALSE) 
# explode into single moves
moves_1 <- map_dfr(moves, rep, moves$steps) %>% pull(dir) 

pos <- list(c("x" = 1, "y" = 1), 
            c("x" = 1, "y" = 1),
            list(c("x" = 1, "y" = 1)))

Answer <- reduce(moves_1, move, .init = pos)
Answer[[1]]
```

    x y 
    3 3 

``` r
Answer[[2]]
```

    x y 
    2 3 

``` r
Answer[[3]] %>% unique() %>% length()
```

    [1] 13

## Part 1

``` r
moves <- read_delim("data/input9a.txt", col_names = c("dir", "steps"), show_col_types = FALSE) 
moves_1 <- map_dfr(moves, rep, moves$steps) %>% pull(dir) 

Answer <- reduce(moves_1, move, .init = pos)
Answer[[1]]
```

       x    y 
     -16 -234 

``` r
Answer[[2]]
```

       x    y 
     -16 -233 

``` r
Answer[[3]] %>% unique() %>% length()
```

    [1] 5981

## Part 2

Woah!
