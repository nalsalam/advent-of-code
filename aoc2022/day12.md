– Day 12 Hill Climbing –
================

## Algorithm

Either the Breadth-First-Search (BFS) or the (DFS) DFS.

``` r
library(tidyverse)
```

## Functions

``` r
#' Setup grid
#' 
#' Setup grid in global environment
#' 
#' Also determine number of rows, number of columns, number of vertices

setup_grid <- function(filename) {
  
  # get the dimensions of the grid
  input <- read_lines(file_name)
  n_row <<- length(input)
  n_col <<- str_length(input[1])
  
  # turn into a vector
  input_v <- paste0(input, collapse = "") %>%
    str_split("") %>% unlist()
  n_vertices <<- length(input_v)
  
  # clean up
  S_vertex <<- which(input_v == "S") # 1
  a_vertices <<- c(S_vertex, which(input_v == "a"))
  
  # need to extract solution
  E_vertex <<- which(input_v == "E") # 22
  input_v[S_vertex] <- "a"
  input_v[E_vertex] <- "z"
  
  # a-z to 1-26
  input_n <- as.numeric(factor(input_v))
  
  # grid as a matrix
  matrix(input_n, nrow = n_row, ncol = n_col, byrow = TRUE) 

}

#' Setup tibble
#' 
#' Setup tibble to represent the G(V, E) data structure and initialize
#' color, d, and pi
#' 
setup_G <- function(grid, n_row, n_col) {
  
  i  <- function(n) ((n-1) %/% n_col) + 1
  j  <- function(n) ((n-1) %% n_col) + 1
  nd <- function(i, j) (i-1) * n_col + j
  
  E <- vector(mode = "list", length = n_vertices)
  for(n in 1:n_vertices) {
    E[[n]] <- 
      c(
      if(j(n)>1) if(grid[i(n),j(n)-1]-grid[i(n),j(n)] <=1) nd(i(n),j(n)-1),
      if(j(n)<n_col) if(grid[i(n),j(n)+1]-grid[i(n),j(n)] <=1) nd(i(n),j(n)+1),
      if(i(n)>1) if(grid[i(n)-1,j(n)]-grid[i(n),j(n)] <=1) nd(i(n)-1,j(n)),
      if(i(n)<n_row) if(grid[i(n)+1,j(n)]-grid[i(n),j(n)] <=1) nd(i(n)+1,j(n)))
  }
  
  G <- tibble(V = 1:n_vertices, E = E, color = "white", d = Inf, pi = NA_integer_)

  return(G)
}

dequeue <- function(Q) {
  u <- Q[1]
  Q <<- Q[-1]
  u
}
enqueue <- function(Q, s) {
  Q <<- c(Q, s)
}

setup_G_down <- function(grid, n_row, n_col) {
  
  i  <- function(n) ((n-1) %/% n_col) + 1
  j  <- function(n) ((n-1) %% n_col) + 1
  nd <- function(i, j) (i-1) * n_col + j
  
  E <- vector(mode = "list", length = n_vertices)
  for(n in 1:n_vertices) {
    E[[n]] <- 
      c(
      if(j(n)>1) if(grid[i(n),j(n)-1]-grid[i(n),j(n)] >=-1) nd(i(n),j(n)-1),
      if(j(n)<n_col) if(grid[i(n),j(n)+1]-grid[i(n),j(n)] >=-1) nd(i(n),j(n)+1),
      if(i(n)>1) if(grid[i(n)-1,j(n)]-grid[i(n),j(n)] >=-1) nd(i(n)-1,j(n)),
      if(i(n)<n_row) if(grid[i(n)+1,j(n)]-grid[i(n),j(n)] >=-1) nd(i(n)+1,j(n)))
  }
  
  G <- tibble(V = 1:n_vertices, E = E, color = "white", d = Inf, pi = NA_integer_)

  return(G)
}

dequeue <- function(Q) {
  u <- Q[1]
  Q <<- Q[-1]
  u
}
enqueue <- function(Q, s) {
  Q <<- c(Q, s)
}
```

## Some notes

- Translate matrix to graph data structure G which is G(V,E)
  list-adjacent

- Be careful of the assumed order of the vertices. Above we identified E
  using the byrow order. So maintain that mapping in the create if the
  `i` and `j` functions.

- Tried attributes but it did not work. One can assign attributes to
  objects like G\[\[1\]\] but not to G\[1\] because ….

- Initially tried to set up data structure with vectorized functions but
  ran into problems with the edges

- Garden variety tibble works. Take-away: Don’t try to be too fancy.

# Part 1 - Example

``` r
# Read input into a matrix 
file_name <- "data/input12_test.txt"

grid <- setup_grid(file_name)

G <- setup_G(grid, n_row, n_col)

G$color[S_vertex] <- "gray"
G$d[] <- 0

Q <- NULL
Q <- enqueue(Q, S_vertex)

while(!(length(Q) == 0)) {
  u <- dequeue(Q)
  for(v in G$E[[u]]) {
        if(G$color[v] == "white") {
          G$color[v] <- "gray"
          G$d[v] <- G$d[u] + 1
          G$pi[v] <- u
          enqueue(Q, v)
        }
  G$color[u] <- "black"
  }
}

G[E_vertex, ]
```

    # A tibble: 1 x 5
          V E         color     d    pi
      <int> <list>    <chr> <dbl> <dbl>
    1    22 <dbl [4]> black    31    21

# Part 1 - Solution

``` r
# Read input into a matrix 
file_name <- "data/input12.txt"

grid <- setup_grid(file_name)

G <- setup_G(grid, n_row, n_col)

G$color[S_vertex] <- "gray"
G$d[S_vertex] <- 0
G$pi[S_vertex] <- NA_integer_

Q <- NULL
Q <- enqueue(Q, S_vertex)

while(!(length(Q) == 0)) {
  u <- dequeue(Q)
  for(v in G$E[[u]]) {
        if(G$color[v] == "white") {
          G$color[v] <- "gray"
          G$d[v] <- G$d[u] + 1
          G$pi[v] <- u
          enqueue(Q, v)
        }
  G$color[u] <- "black"
  }
}

G[E_vertex, ]  # 534 
```

    # A tibble: 1 x 5
          V E         color     d    pi
      <int> <list>    <chr> <dbl> <dbl>
    1  3399 <dbl [4]> black   534  3400

``` r
# 3654, 3401, 3400, 3399 looks right
grid[(i(E_vertex)-3):(i(E_vertex)+3),
     (j(E_vertex)-3):(j(E_vertex)+3)]
```

# Part 2

find the shortest path from any square at elevation a to the square
marked E.

Thoughts:

- turn the problem into a descending algorithm for and then look for the
  shortest distince to an `a` square: `setup_G_down`

- for this the setup_G function would need to use tests `>= -1`, i.e.
  cannot descend more than one unit

- I need the vertices of the `a` squares: `a_vertices`

# Part 2 example

``` r
# Read input into a matrix 
file_name <- "data/input12_test.txt"

grid <- setup_grid(file_name)

G <- setup_G_down(grid, n_row, n_col)

G$color[E_vertex] <- "gray"
G$d[] <- 0

Q <- NULL
Q <- enqueue(Q, E_vertex)

while(!(length(Q) == 0)) {
  u <- dequeue(Q)
  for(v in G$E[[u]]) {
        if(G$color[v] == "white") {
          G$color[v] <- "gray"
          G$d[v] <- G$d[u] + 1
          G$pi[v] <- u
          enqueue(Q, v)
        }
  G$color[u] <- "black"
  }
}

G[a_vertices, ] %>% filter(d == min(d))
```

    # A tibble: 1 x 5
          V E         color     d    pi
      <int> <list>    <chr> <dbl> <dbl>
    1    33 <dbl [2]> black    29    34

# Part 2 - Solution

``` r
# Read input into a matrix 
file_name <- "data/input12.txt"

grid <- setup_grid(file_name)

G <- setup_G_down(grid, n_row, n_col)

G$color[E_vertex] <- "gray"
G$d[] <- 0

Q <- NULL
Q <- enqueue(Q, E_vertex)

while(!(length(Q) == 0)) {
  u <- dequeue(Q)
  for(v in G$E[[u]]) {
        if(G$color[v] == "white") {
          G$color[v] <- "gray"
          G$d[v] <- G$d[u] + 1
          G$pi[v] <- u
          enqueue(Q, v)
        }
  G$color[u] <- "black"
  }
}

G[S_vertex, ] # 534 same as part 1. That's good.
```

    # A tibble: 1 x 5
          V E         color     d    pi
      <int> <list>    <chr> <dbl> <dbl>
    1  3261 <dbl [3]> black   534  3424

``` r
# a bunch of a_vertices are white, i.e. not reachable from E??
G[a_vertices, ] %>% filter(color == "black") %>% arrange(d) # 525
```

    # A tibble: 424 x 5
           V E         color     d    pi
       <int> <list>    <chr> <dbl> <dbl>
     1  4728 <dbl [3]> black   525  4729
     2  4565 <dbl [3]> black   526  4728
     3  4891 <dbl [3]> black   526  4728
     4  4402 <dbl [3]> black   527  4565
     5  5054 <dbl [3]> black   527  4891
     6  4239 <dbl [3]> black   528  4402
     7  5217 <dbl [3]> black   528  5054
     8  4076 <dbl [3]> black   529  4239
     9  5380 <dbl [3]> black   529  5217
    10  3913 <dbl [3]> black   530  4076
    # ... with 414 more rows
