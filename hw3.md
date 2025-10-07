p8105_hw3_yl2625
================
Yurou Liu
2025-10-07

load in data

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
load("./data/instacart.rda")
insta_ez_to_read = instacart %>% 
  select(order_number, aisle, everything())
```

``` r
# group by same aisles and sum up the order number for the same group 
total_aisle_df = insta_ez_to_read %>% 
  group_by(aisle) %>% 
  summarise(total_orders = sum(order_number)) %>%
  arrange(desc(total_orders))

# count the number of different aisles
num_aisles = n_distinct(total_aisle_df$aisle)

# find out the best seller
best_seller = total_aisle_df %>% 
  # pick out the aisle with the most orders, of there are aisles with the same number of orders, they will be picked out together
  filter(total_orders == max(total_orders))

print(num_aisles)
```

    ## [1] 134

``` r
print(best_seller)
```

    ## # A tibble: 1 × 2
    ##   aisle        total_orders
    ##   <chr>               <int>
    ## 1 fresh fruits      2787084

There are 134 different aisles, and most items are ordered form fresh
fruits, with 2787084 orders in total.
