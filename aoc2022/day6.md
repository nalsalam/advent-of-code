– Day 6 Tuning Trouble –
================

``` r
library(tidyverse)
```

## Read the Input

``` r
data_buffer <- scan("data/input6a.txt", what = "character") %>%
  str_split("") %>% unlist()
length(data_buffer)
```

    [1] 4095

## -- Part One --

``` r
i = 4
while(length(unique(data_buffer[(i-4+1):i])) < 4) {
  i = i + 1
}
i
```

    [1] 1109

## -- Part Two –

``` r
i = 14
while(length(unique(data_buffer[(i-14+1):i])) < 14) {
  i = i + 1
}
i
```

    [1] 3965
