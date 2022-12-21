– Day 13 –
================

``` r
library(tidyverse)
```

# Read Input as json and put into a tibble

``` r
parse_and_setup_pairs <- function(file_name) {
  input <- read_lines(file_name) %>% 
  # remove blank lines
  {.[. != ""]} 

  parsed_input <-
    map(input, ~jsonlite::parse_json(.x)) %>% set_names(nm = rep(c("L", "R"), times = length(input) / 2))
  
  n_pairs <<- length(input) / 2

# short name to conciseness
  p <<-

  tibble(L = parsed_input[seq(1, 2*n_pairs, 2)],
         R = parsed_input[seq(2, 2*n_pairs, 2)],
        correct_order = NA)
}
```

# The algorithm

- I did not use recursion.

- With ties I dropped elements

- With lists I used unlist(., recursive = FALSE) to remove a level

``` r
determine_order <- function(p) {
  
for(i in 1:nrow(p)) {
  L <- p$L[[i]]  
  R <- p$R[[i]]  

  while(is.na(p$correct_order[[i]])) {
    
    # Case 1: zero length list
    if(length(L) == 0 | length(R) == 0) {  
      case <- 1
    
      if(length(L) == 0 & length(R) == 0) {
        L <- L[-1]
        R <- R[-1]
        next
      } else if(length(L) == 0) {
        p$correct_order[[i]] <- TRUE
        break
      } else if(length(R) == 0) {
        p$correct_order[[i]] <- FALSE
        break
      } else {
        cat("Oops: Case 1", "pair: ", i)
        stop()
      } 
    
    # Case 2: one or both is a list with positive length
    } else if(!is.integer(L) | !is.integer(R)) {
      # cat("2", !is.integer(L), !is.integer(R), "; ")

      if(!is.integer(L)) {
        L <- unlist(L, recursive = FALSE)  
      }
      if(!is.integer(R)) {
        R <- unlist(R, recursive = FALSE)  
      }
      next
    
    # case 3: both integers
    } else if(is.integer(L) & is.integer(R)) {
      # cat("3", is.integer(L), is.integer(R), "; ")

        if(L[[1]] < R[[1]]) {
          p$correct_order[[i]] <- TRUE
          break
        } else if(L[[1]] > R[[1]]) {
          p$correct_order[[i]] <- FALSE
          break
        } else if(L[[1]] == R[[1]]) {
          L <- L[-1]
          R <- R[-1]
          next
        } else {
          cat("Oops: Case 3", "pair: ", i)
          stop()
        } 
    
    # case 4: What did I miss
    } else {
        cat("Oops: Case 4", "pair: ", i)
        stop()
    }
  } # while
} # for
  return(p)
} # function
```

# Part 1 - Example

``` r
p <- parse_and_setup_pairs("data/input13_test.txt")

pplus <- determine_order(p)

sum(which(pplus$correct_order)) # 13 is correct
```

    [1] 13

# Part 1

``` r
p <- parse_and_setup_pairs("data/input13.txt")

pplus <- determine_order(p)

sum(which(pplus$correct_order))  # 5478 is too low
```

    [1] 5478
