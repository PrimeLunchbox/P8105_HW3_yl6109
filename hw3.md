p8105_hw3_yl2625
================
Yurou Liu
2025-10-07

## Problem 1

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
library(p8105.datasets)

data("instacart") 
insta_ez_to_read = instacart %>% 
  select(aisle, everything())
```

### 1-1 How many aisles are there, and which aisles are the most items ordered from?

``` r
# group by same aisles and sum up the order number for the same group 
total_aisle_df = insta_ez_to_read %>% 
  group_by(aisle) %>% 
  summarise(total_orders = n(), aisle = first(aisle)) %>%
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
    ##   aisle            total_orders
    ##   <chr>                   <int>
    ## 1 fresh vegetables       150609

According to the printed results, there are 134 different aisles, and
the most items are ordered from fresh vegetables, with 150609 orders in
total.

### 1-2 make a plot

``` r
plot_df = total_aisle_df %>% 
  filter(total_orders > 10000)

library(ggplot2)

plot_df %>% 
  ggplot(aes(y = reorder(aisle, total_orders), x = total_orders)) + 
  geom_col() +
  labs(title = "Aisles with 10000+ Orders", x = "Number of Orders", y = "Aisle") + 
  # add order number labels to the columns.
  geom_text(aes(label = format(total_orders, big.mark = ",")), hjust = -.05, size = 3) + 
  # expand x axis from the right side.
  scale_x_continuous(expand = expansion(mult = c(0, .1))) + 
  theme(plot.title = element_text(hjust = .5))
```

![](hw3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### 1-3 Table: three most popular items in ‘baking ingredients’, ‘dog food care’ and ‘packaged vegetables fruits’ with number of orders.

``` r
df_for_table = insta_ez_to_read %>% 
  select(aisle, product_name) %>% 
  group_by(product_name) %>% 
  summarise(total_orders = n(), 
            aisle = first(aisle)) %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits" )) %>% 
  group_by(aisle) %>% 
  slice_max(total_orders, n = 1) %>% 
  ungroup() %>% 
  select(aisle, everything()) %>% 
  print()
```

    ## # A tibble: 3 × 3
    ##   aisle                      product_name                           total_orders
    ##   <chr>                      <chr>                                         <int>
    ## 1 baking ingredients         Light Brown Sugar                               499
    ## 2 dog food care              Snack Sticks Chicken & Rice Recipe Do…           30
    ## 3 packaged vegetables fruits Organic Baby Spinach                           9784

### 1-4 Table: mean hour of the day at which pink lady apples and coffee ice cream are ordered on each day of the week.

``` r
library(tidyr)

df_for_table2 = insta_ez_to_read %>% 
  select(product_name, order_dow, order_hour_of_day) %>% 
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
  group_by(product_name) %>% 
  mutate(row_id = row_number()) %>% 
  pivot_wider(names_from = product_name, 
              values_from = order_hour_of_day) %>% 
  select(-row_id) %>% 
  arrange(order_dow) %>% 
  group_by(order_dow) %>% 
  summarise(mean_order_hour_apples = mean(`Pink Lady Apples`, na.rm = TRUE), 
         mean_order_hour_icecream = mean(`Coffee Ice Cream`, na.rm = TRUE)) %>% 
  mutate(order_dow = recode(order_dow, 
                            `0` = "Sunday", 
                            `1` = "Monday", 
                            `2` = "Tuesday", 
                            `3` = "Wednesday", 
                            `4` = "Thursday", 
                            `5` = "Friday", 
                            `6` = "Saturday")) %>% 
  print()
```

    ## # A tibble: 7 × 3
    ##   order_dow mean_order_hour_apples mean_order_hour_icecream
    ##   <chr>                      <dbl>                    <dbl>
    ## 1 Sunday                      13.4                     13.8
    ## 2 Monday                      11.4                     14.3
    ## 3 Tuesday                     11.7                     15.4
    ## 4 Wednesday                   14.2                     15.3
    ## 5 Thursday                    11.6                     15.2
    ## 6 Friday                      12.8                     12.3
    ## 7 Saturday                    11.9                     13.8

## Problem 2

``` r
zillow_df = readr::read_csv("./data/zip_nyc.csv") %>% 
  janitor::clean_names()
```

    ## Rows: 149 Columns: 125
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (6): RegionType, StateName, State, City, Metro, CountyName
    ## dbl (119): RegionID, SizeRank, RegionName, 2015-01-31, 2015-02-28, 2015-03-3...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
a_zillow_df = zillow_df %>% 
  pivot_longer(c("x2015_01_31":"x2024_08_31"), 
               names_to = "date", 
               names_prefix = "x")

zip_count = a_zillow_df %>% 
  filter(!is.na(value)) %>%
  group_by(region_name) %>% 
  summarise(count = n())
```

### 2-1-a How many ZIP codes are observed 116 times?

``` r
obs_116_times = zip_count %>% 
  filter(count == 116) %>% 
  print()
```

    ## # A tibble: 48 × 2
    ##    region_name count
    ##          <dbl> <int>
    ##  1       10001   116
    ##  2       10002   116
    ##  3       10003   116
    ##  4       10005   116
    ##  5       10010   116
    ##  6       10012   116
    ##  7       10013   116
    ##  8       10014   116
    ##  9       10017   116
    ## 10       10018   116
    ## # ℹ 38 more rows

According to the printed results, there are 48 zip codes that were
observed 116 times in total.

### 2-1-b How many are observed fewer than 10 times?

``` r
obs_less_than_10 = zip_count %>% 
  filter(count < 10) %>% 
  print()
```

    ## # A tibble: 26 × 2
    ##    region_name count
    ##          <dbl> <int>
    ##  1       10044     9
    ##  2       10162     2
    ##  3       10303     2
    ##  4       10308     3
    ##  5       10453     1
    ##  6       10455     3
    ##  7       10456     4
    ##  8       10459     2
    ##  9       10460     2
    ## 10       10470     1
    ## # ℹ 16 more rows

According to the printed results, there are 26 zip codes that were
observed less than 10 times.

### 2-1-c Why are some ZIP codes are observed rarely and others observed in each month?

According to a_zillow_df, differences in zip code observation times may
due to the various data collection starting time. For example, 11368 was
first observed from 2023-01-31, which may imply that the data collection
in this area began later. Besides, there is also possibility that some
zip codes were merged or split due to administrative changes, which can
lead to the discontinuity and incomplete of these historical data.

### 2-2-a Table: average rental price in wach borough and year.

``` r
queens_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Queens County") %>% 
  group_by(year) %>% 
  summarise(ave_rental_prices = mean(value, na.rm = TRUE))

kings_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Kings County") %>% 
  group_by(year) %>% 
  summarise(ave_rental_prices = mean(value, na.rm = TRUE))

richmond_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Richmond County") %>% 
  group_by(year) %>% 
  summarise(ave_rental_prices = mean(value, na.rm = TRUE))

bronx_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Bronx County") %>% 
  group_by(year) %>% 
  summarise(ave_rental_prices = mean(value, na.rm = TRUE))

ny_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "New York County") %>% 
  group_by(year) %>% 
  summarise(ave_rental_prices = mean(value, na.rm = TRUE))

p22a_df = bind_rows(
  queens_df %>% mutate(county = "Queens"), 
  kings_df %>% mutate(county = "Kings"), 
  richmond_df %>% mutate(county = "Richmond"),
  bronx_df %>% mutate(county = "Bronx"),
  ny_df %>% mutate(county = "New York")) %>% 
  pivot_wider(names_from = county, 
              values_from = ave_rental_prices) %>% 
  print()
```

    ## # A tibble: 10 × 6
    ##    year  Queens Kings Richmond Bronx `New York`
    ##    <chr>  <dbl> <dbl>    <dbl> <dbl>      <dbl>
    ##  1 2015   2215. 2493.     NaN  1760.      3022.
    ##  2 2016   2272. 2520.     NaN  1520.      3039.
    ##  3 2017   2263. 2546.     NaN  1544.      3134.
    ##  4 2018   2292. 2547.     NaN  1639.      3184.
    ##  5 2019   2388. 2631.     NaN  1706.      3310.
    ##  6 2020   2316. 2555.    1978. 1811.      3107.
    ##  7 2021   2211. 2550.    2045. 1858.      3137.
    ##  8 2022   2406. 2868.    2147. 2054.      3778.
    ##  9 2023   2562. 3015.    2333. 2285.      3933.
    ## 10 2024   2694. 3127.    2536. 2497.      4078.

### 2-2-b Comment on the table

Overall speaking, all boroughs show an increase in rental prices from
2015 to 2024. The New York County stays the most expensive throughout
the whole 10 years, followed by Kings, Queens, Richmond and Bronx.
Rental prices in Queens and Kings County decreased from 2019 to 2021,
rental prices in New York County decreased sharply from 2019 to 2020,
probably due to the COVID-19 pandemic. While rental price in Richmond
County increased from 2020 to 2024 and rental price of Bronx County kept
increasing throughout the whole period observed.

### 2-3-a

``` r
queens_23a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Queens County") %>% 
  group_by(region_name, year) %>% 
  summarise(county = "Queens", ave_rental_prices = mean(value, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'region_name'. You can override using the
    ## `.groups` argument.

``` r
kings_23a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Kings County") %>% 
  group_by(region_name, year) %>% 
  summarise(county = "Kings", ave_rental_prices = mean(value, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'region_name'. You can override using the
    ## `.groups` argument.

``` r
richmond_23a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Richmond County") %>% 
  group_by(region_name, year) %>% 
  summarise(county = "Richmond", ave_rental_prices = mean(value, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'region_name'. You can override using the
    ## `.groups` argument.

``` r
bronx_23a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Bronx County") %>% 
  group_by(region_name, year) %>% 
  summarise(county = "Bronx", ave_rental_prices = mean(value, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'region_name'. You can override using the
    ## `.groups` argument.

``` r
ny_23a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "New York County") %>% 
  group_by(region_name, year) %>% 
  summarise(county = "New York", ave_rental_prices = mean(value, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'region_name'. You can override using the
    ## `.groups` argument.

``` r
p23a_df = bind_rows(kings_23a_df, queens_23a_df, richmond_23a_df, bronx_23a_df, ny_23a_df)

p23a_plot = p23a_df %>% 
  filter(!is.na(ave_rental_prices)) %>% 
  ggplot(aes(x = year, 
             y = ave_rental_prices, 
             group = region_name)) + 
  geom_line(color = "#52C0EF") + 
  geom_point(color = "#52C0EF") + 
  geom_text(data = . %>% 
              group_by(region_name) %>% 
              filter(!is.na(ave_rental_prices)) %>% 
              slice(1), 
            aes(label = region_name), 
            hjust = 1.1,
            size = 2.5, 
            color = "black",
            check_overlap = TRUE) + 
  facet_wrap(. ~ county, nrow = 2, scales = "free_y") +
  labs(title = "Average NYC Rental Prices Within ZIP Codes for All Available Years",
       y = "Average Rental Prices", 
       x = "Year") + 
  theme(legen.position = "none", 
        plot.title = element_text(hjust = .5, size = 20))
```

### 2-3-b Comment on the plot

The plot shows the overall changes in the rental prices of regions of
different zip codes in the five counties. All boroughs exhibit upward
trends, particularly during the 2021-2024 post-pandemic recovery period.
There also exists intra-borough disparities. Rental prices in Bronx
County demonstrate steep percentage growth rates in recent years.

### 2-4-a

``` r
queens_24a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Queens County",
         year == "2023")

kings_24a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Kings County",
         year == "2023")

richmond_24a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Richmond County",
         year == "2023")

bronx_24a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "Bronx County",
         year == "2023")

ny_24a_df = a_zillow_df %>% 
  separate(date, c("year", "month","day")) %>% 
  filter(county_name == "New York County",
         year == "2023")

p24a_df = bind_rows(queens_24a_df, kings_24a_df, richmond_24a_df, bronx_24a_df, ny_24a_df)

p24a_plot = p24a_df %>% 
  ggplot(aes(x = month, 
           y = value, 
           group = region_name)) +
  geom_line(color = "#5EE14D") + 
  geom_point(color = "#5EE14D") + 
  geom_text(data = . %>% 
              group_by(region_name) %>% 
              filter(!is.na(value)) %>% 
              slice(1),
            aes(label = region_name), 
            hjust = 1.1, 
            size = 2.5, 
            color = "black", 
            check_overlap = TRUE) + 
  facet_wrap(. ~ county_name, nrow = 2, scales = "free_y") + 
  labs(title = "Average Rental Price Within Each ZIP Code Over Each Month in 2023",
       x = "Month", 
       y = "Rental Prices") + 
  theme(plot.title = element_text(hjust = .5, size = 20))
```

### 2-4-b Comment on the plot

This plot shows variations across boroughs in NYC in 2023. Counties such
as Queens and Kings County show higher prices and larger fluctuations
compared to the more stable rental prices in Bronx and Richmond
Counties. Within the same county, there are also intra-county
disparities in rental prices. Several zip codes exhibit notable
volatility, which suggest dynamics of the market and others maintain
relatively steady trajectories throughout the year.

### 2-5 Combine the previous two plots and export to ‘results’ folder.

``` r
library(patchwork)

combined_plot = p23a_plot / p24a_plot
ggsave("./results/combined_plot.png", 
       plot = combined_plot, 
       width = 20, 
       height = 32, 
       dpi = 300)
```

    ## Warning in plot_theme(plot): The `legen.position` theme element is not defined
    ## in the element hierarchy.

    ## Warning: Removed 330 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

    ## Warning: Removed 333 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

## Problem 3

### 3-1 Data tidying

``` r
accel_df = readr::read_csv("./data/nhanes_accel.csv") %>% 
  janitor::clean_names()
```

    ## Rows: 250 Columns: 1441
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1441): SEQN, min1, min2, min3, min4, min5, min6, min7, min8, min9, min1...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
covar_df = readr::read_csv("./data/nhanes_covar.csv", skip = 4) %>% 
  janitor::clean_names() 
```

    ## Rows: 250 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (5): SEQN, sex, age, BMI, education
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
merged_df <- left_join(accel_df, covar_df, by = "seqn") %>% 
  select(sex, age, bmi, education, everything()) %>% 
  filter(age >= 21, 
         !is.na(sex),
         !is.na(bmi), 
         !is.na(education)) %>% 
  mutate(sex = factor(sex, 
                      levels = c(1, 2), 
                      labels = c("male", "female")), 
         education = factor(education, 
                            levels = c(1, 2, 3), 
                            labels = c("Less than high school", 
                                       "High school equivalent", 
                                       "More than high school"), 
                            ordered = TRUE))
```

### 3-2-a Table: number of men and women in each education category

``` r
p32a_df = merged_df %>% 
  select(sex, education)  %>% 
  group_by(education, sex) %>%
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(names_from = education, 
              values_from = n) %>% 
  print()
```

    ## # A tibble: 2 × 4
    ##   sex    `Less than high school` `High school equivalent` More than high schoo…¹
    ##   <fct>                    <int>                    <int>                  <int>
    ## 1 male                        27                       35                     56
    ## 2 female                      28                       23                     59
    ## # ℹ abbreviated name: ¹​`More than high school`

### 3-2-b Plot: age distributions for men and women in each education category.

``` r
p32b_df = merged_df %>% 
  select(education, sex, age)

p32b_plot = p32b_df %>% 
  ggplot(aes(x = age, 
             fill = sex)) + 
  geom_histogram(color = "black") +
  facet_wrap(. ~ education) + 
  scale_x_continuous(breaks = seq(20, 80, by = 5)) + 
  labs(title = "Age Distributions for Men and Women in Each Education Category", 
       x = "Age", 
       y = "Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))
```

### 3-2-c Comment on the table and plot

Table: As shown in p32a_df, male has a highest percentage in the
education level “high school equivalent”, and female has a highest
percentage in the education level “more than high school”. The
distribution of each gender in each education level is relatively
balanced.

Plot: a large percentage of the data is from users who has an education
level that is more than high school. In the education level which is
high school equivalent, age interval 30-40, 50-60, and 65-70, number of
male is far more than female. In the education level which is less than
high school, there exits blank area in histogram, this may be caused by
the exclusion of rows with na values in demographic data, or also can be
caused by sampling bias.

### 3-3-a Plot: total activity by age, sex and education level.

``` r
p33a_df = merged_df %>% 
  mutate(activity = rowSums(across(min1:min1440)), na.rm = TRUE) %>% 
  select(sex, age, education, activity) 

p33a_plot = p33a_df %>%
  ggplot(aes(x = age, 
             y = activity)) +
  geom_point(size = .8, 
             alpha = .6, 
             aes(color = sex, fill = sex) ) + 
  geom_smooth(aes(color = sex), 
              se = FALSE) + 
  labs(title = "Total Activity by Age, Sex and Education Level",
       x = "Age", 
       y = "Total Activity") + 
  scale_x_continuous(breaks = seq(20, 80, by = 5)) + 
  facet_wrap(. ~ education) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 6.5),
        plot.title = element_text(hjust = .5))
```

### 3-3-b Comment on the plot

According to the p33a_plot, of all three education levels, the total
activity has an decreasing overall trend. There exist fluctuations in
total activity from age 20 to age 80 participant, which may due to
different perspectives in the living style among different ages and
education levels. Under the education level which is mare than high
school, total activity of male of every observed age is higher than
female.

### 3-4-a Plot: Mean hourly activity by hour, sex and education level.

``` r
p34a_df = merged_df %>% 
  pivot_longer(cols = starts_with("min"), 
               names_to = "minute", 
               values_to = "activity") %>% 
  mutate(minute_num = as.numeric(gsub("min", "", minute)), 
         hour = floor((minute_num - 1) / 60)) %>% 
  group_by(seqn, education, sex, hour) %>%
  summarise(hourly_activity_per_person = sum(activity, na.rm = TRUE), 
            .groups = "drop") %>%
  group_by(education, sex, hour) %>% 
  summarise(mean_hourly_activity = mean(hourly_activity_per_person, 
            na.rm = TRUE), 
            .groups = "drop")

p34a_plot = p34a_df %>% 
  ggplot(aes(x = hour, 
             y = mean_hourly_activity,)) + 
  geom_point(aes(fill = sex, 
                 color = sex), 
             size = .8, 
             alpha = .6) + 
  geom_smooth(se = FALSE, 
              aes(color = sex)) + 
  facet_wrap(. ~ education) + 
  labs(x = "Hour", 
       y = "Mean Hourly Activity", 
       title = "Mean Hourly Activity by Hour, Sex and Education Level") + 
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8))
```

### 3-4-b Comment on the plot

According to p34a_plot, in all three education levels and both two
genders, the overall trend of mean hourly activity level in a day first
increases and arrives the peak at around 12pm and then decreases.
Difference in mean hourly activity level among different education
levels is not too much. And it can also be noticed that in the beginning
of the day, mean hourly activity level of male is higher than female.
Under “equivalent to high school” and “more than high school” education
level, the mean hourly activity level of female is higher than male
almost in every hour.
