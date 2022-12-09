– Day 8 –
================

``` r
library(tidyverse)
```

## Functions

``` r
read_map <- function(file) {
  mat_chr <-
  read_lines(file) %>% 
  str_split("", simplify = TRUE)
  matrix(as.numeric(mat_chr), ncol = ncol(mat_chr))
}

set_visible <- function(map) {
  
  ncol <- ncol(map)
  nrow <- nrow(map)
  map_viz <- matrix(FALSE, nrow = nrow, ncol = ncol)
  
  # set edges to visible
  for(i in 1:nrow) {
    map_viz[i, 1] <- TRUE
    map_viz[i, ncol] <- TRUE
  }
  for(j in 1:ncol) {
    map_viz[1, j] <- TRUE
    map_viz[nrow, j] <- TRUE
  }
  # set interior values
  for(i in 2:(nrow-1)) {
  for(j in 2:(ncol-1)) {
    
  # consider all 4 directions
    map_viz[i, j] <- 
      # Must have one true in every direction to be not visible
      # down
      !(
      any(map[i, j] <= map[(i+1):nrow, j]) &&
      # up
      any(map[i, j] <= map[1:(i-1), j]) &&
      # right
      any(map[i, j] <= map[i, (j+1):ncol]) &&
      # left 
      any(map[i, j] <= map[i, 1:(j-1)])
      )
  }}
  map_viz
}

scenic_score <- function(map) {
  
  count_view <- function(v) {
      i <- 1
      n <- 0
      while(i <= length(v)) {
        if(v[i] == TRUE) {
          n <- n + 1
        } else {
          n <- n + 1
          break
        }
        i <- i + 1
      }
      n
  }

  # initialize; edge values are zero
  ncol <- ncol(map)
  nrow <- nrow(map)
  map_scenic <- matrix(0, nrow = nrow, ncol = ncol)
  
  # set interior values
  for(i in 2:(nrow-1)) {
  for(j in 2:(ncol-1)) {
    
  # consider all 4 directions
    map_scenic[i, j] <- 
      # Count up to and including the first FALSE (block)
      # up
      count_view(map[i, j] > map[seq(i-1, 1, -1), j]) * 
      # left 
      count_view(map[i, j] > map[i, seq((j-1), 1, -1)]) * 
      # right
      count_view(map[i, j] > map[i, (j+1):ncol]) * 
      # down
      count_view(map[i, j] > map[(i+1):nrow, j])

  }}
  map_scenic
}
```

## Part 1 – Example

``` r
map <- read_map("data/input8a_test.txt")
map_viz <- set_visible(map)
sum(map_viz)
```

    [1] 21

## Part 2 - Example

``` r
map <- read_map("data/input8a_test.txt")
map_scn <- scenic_score(map)
map_scn[2,3]
```

    [1] 4

``` r
map_scn[4,3]
```

    [1] 8

``` r
max(map_scn)
```

    [1] 8

## Part 1

``` r
map <- read_map("data/input8a.txt")
map_viz <- set_visible(map)
sum(map_viz)
```

    [1] 1854

## Part 2

``` r
map <- read_map("data/input8a.txt")
map_scn <- scenic_score(map)
max(map_scn)
```

    [1] 527340
