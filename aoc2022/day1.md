## Part 1

The calories of each snack that each elf is carrying is in the input.
Each elf is separate from the next by a blank in the input.

``` r
calories <- readr::read_lines("data/input1a.txt") %>% as.numeric()
```

It’s best to preallocate the vector because R copies on modify.

``` r
n_elves <- sum(is.na(calories))

elves <- vector("double", length = n_elves)
```

### Using a for() loop which is very rare in R

``` r
  elf_ID <- 1
  elf_cal <- 0

  for(cal in 1:length(calories)) {
    
    if(is.na(calories[cal])) {
      
      # assign sum
      elves[elf_ID] <- elf_cal
      
      # initialize next elf
      elf_ID <- elf_ID + 1
      elf_cal <- 0
      
    } else {
      # sum up calories
      elf_cal = elf_cal + calories[cal]
    }
  }
```

The number of calories carried by the elf with the most calories.

``` r
print(max(elves))
```

    ## [1] 71924

## Part 2

The number of calories carried by the top three elves.

``` r{}
sum(sort(elves)[(n_elves-2):n_elves])
```

### Splitting the vector into a list and mapping it to sum

Calculate an elf_ID and then split the vector into a list of vectors.
The length of the list is the number of elves and each element is the
calories of the snacks the elf is carrying.

``` r
elf_ID <- cumsum(is.na(calories)) + 1

elf_calories <- split(calories, elf_ID)
```

With that list it is easy to sum the calories

``` r
elf_sum_calories <- map_dbl(elf_calories, sum, na.rm = TRUE)
```

# Part 1

``` r
elf_sum_calories %>% max()
```

    ## [1] 71924

# Part 2

``` r
(sort(elf_sum_calories, decreasing = TRUE) %>% cumsum())[3]
```

    ##      7 
    ## 210406
