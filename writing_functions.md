writing_functions
================
Fang Wang
2024-10-23

``` r
library (tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
setwd("/Users/fangwang/Downloads/P8105 Data Science I/Lecture 16/writing_functions")

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp =.6,
  out.width = "90%"
)

theme_set (theme_bw() + theme (
    legend.position =  "bottom"
  ))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_color_discrete = scale_color_viridis_d()
scale_fill_discrete = scale_fill_viridis_d()
```

# Do sth simple: z-score computation (input: 1 vector and output: 1 vector)

``` r
x_vec = rnorm(25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec) # z-score computation
```

    ##  [1] -0.3656285 -0.1046047  0.8058828  0.5168308 -0.2386036 -0.5066667
    ##  [7]  1.5357680 -0.9721071  0.3637039 -0.7294501  1.9513850 -0.1376393
    ## [13] -0.1489187 -0.9336230  0.3863426 -0.7896717 -1.0417356  0.2880910
    ## [19]  0.4275028  1.8913862  0.1438369 -1.2578916 -1.2256734  1.6707614
    ## [25] -1.5292771

# Write a function to compute z-scores

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1] -0.3656285 -0.1046047  0.8058828  0.5168308 -0.2386036 -0.5066667
    ##  [7]  1.5357680 -0.9721071  0.3637039 -0.7294501  1.9513850 -0.1376393
    ## [13] -0.1489187 -0.9336230  0.3863426 -0.7896717 -1.0417356  0.2880910
    ## [19]  0.4275028  1.8913862  0.1438369 -1.2578916 -1.2256734  1.6707614
    ## [25] -1.5292771

# try my function on some other things

``` r
z_scores(3) # can't compute sd or mean from just one number!
```

    ## [1] NA

``` r
z_scores("my name is jeff") # it is not numeric.
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
z_scores(iris)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Warning in Ops.factor(left, right): '-' not meaningful for factors

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ##  [1] -0.7348469 -0.7348469  1.3063945  1.3063945 -0.7348469 -0.7348469
    ##  [7] -0.7348469 -0.7348469 -0.7348469 -0.7348469 -0.7348469 -0.7348469
    ## [13] -0.7348469 -0.7348469  1.3063945 -0.7348469  1.3063945  1.3063945
    ## [19]  1.3063945  1.3063945 -0.7348469 -0.7348469  1.3063945 -0.7348469
    ## [25]  1.3063945

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric") # for z_scores("my name is jeff")  
  } else if (length(x) == 1) {           # for z_scores(3)
    stop("Z scores cannot be computed for length 1 vectors")
  }
  z = mean(x) / sd(x)
  z
}


z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric") # for z_scores("my name is jeff")  
  } else if (length(x) < 5) {           # for z_scores(3)
    stop("you need at least five numbers to compute the z scores")
  }
  z = mean(x) / sd(x)
  z
}
```

# multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  out_df =
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return (out_df)
}

mean_and_sd (x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.59  3.80

# Check stuff using a simulation

``` r
sim_df = 
  tibble(
  x = rnorm(30, mean = 10, sd = 5)
)

# traditional method:
sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.93  5.76

Simulation function to check sample mean and sd:

``` r
sim_mean_sd = function (sample_size, true_mean, true_sd) {
  
  sim_df = 
  tibble(
  x = rnorm(sample_size, true_mean, true_sd)
)

out_df =
  sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
  
return(out_df)
}

sim_mean_sd (sample_size = 30, true_mean = 4, true_sd =12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.156  11.6

``` r
sim_mean_sd (30, 4, 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.12  14.0

``` r
# "sim_mean_sd (300)" # the function doesn't work until we do the following:

sim_mean_sd = function (sample_size = 30, true_mean =10, true_sd = 5) {
  
  sim_df = 
  tibble(
  x = rnorm(sample_size, true_mean, true_sd)
)

out_df =
  sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
  
return(out_df)
}

sim_mean_sd (sample_size = 30, true_mean = 4, true_sd =12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.48  12.0

``` r
sim_mean_sd (300)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.84  4.24

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

# Try to write a function to import the data above

``` r
lotr_import = function (cell_range, movie_title) {
  
movie_df = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = cell_range) |>
  mutate (movie = movie_title) |> 
  janitor::clean_names() |> 
  pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_title)
  
return (movie_df)
}

lotr_df = 
  bind_rows(
lotr_import ("B3:D6", "fellowship_ring"),
lotr_import ("F3:H6", "two_towers"),
lotr_import ("J3:L6", "return_king"),
)
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |> # nth(4) and nth(5) to take No. 4 and No. 5 tables
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

# Let’s write a quick function to scrape review information for other tables on this page. We’ll pass in the HTML data as an argument so we don’t scrape it each time, along with a number and name for the table we want to process.

``` r
nsduh_table_format = function (table_num, table_name) {
  out_table =
   nsduh_html |> 
    html_table() |> 
    nth(table_num) |> 
    slice (-1) |> 
    mutate (name = table_name)
  
  return (out_table)
}

nsduh_table_format (1, "marj")
```

    ## # A tibble: 56 × 17
    ##    State     `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-2014)`
    ##    <chr>     <chr>            <chr>            <chr>          <chr>             
    ##  1 Total U.… 12.90a           13.36            0.002          13.28b            
    ##  2 Northeast 13.88a           14.66            0.005          13.98             
    ##  3 Midwest   12.40b           12.76            0.082          12.45             
    ##  4 South     11.24a           11.64            0.029          12.02             
    ##  5 West      15.27            15.62            0.262          15.53a            
    ##  6 Alabama   9.98             9.60             0.426          9.90              
    ##  7 Alaska    19.60a           21.92            0.010          17.30             
    ##  8 Arizona   13.69            13.12            0.364          15.12             
    ##  9 Arkansas  11.37            11.59            0.678          12.79             
    ## 10 Californ… 14.49            15.25            0.103          15.03             
    ## # ℹ 46 more rows
    ## # ℹ 12 more variables: `12-17(2014-2015)` <chr>, `12-17(P Value)` <chr>,
    ## #   `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>, `18-25(P Value)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `26+(P Value)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>, `18+(P Value)` <chr>,
    ## #   name <chr>

# or we can do some cleaning as follows:

``` r
nsduh_table = function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  return (table)
  
}
```

``` r
nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )
```

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 1.149138

``` r
my_summary(x_vec, IQR)
```

    ## [1] 0.9939844

``` r
my_summary(x_vec, var)
```

    ## [1] 1.320518

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
