Case Study: Bike-sharing
================
Abhinav
2022-09-29

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

# Part 1

## Importing data

``` r
data_q1 <- read.csv("Trips_2019_Q1.csv")
data_q2 <- read.csv("Trips_2019_Q2.csv")
data_q3 <- read.csv("Trips_2019_Q3.csv")
data_q4 <- read.csv("Trips_2019_Q4.csv")
```

## Homogenizing column names

``` r
colnames(data_q2) <- c('trip_id', 'start_time', 'end_time', 'bikeid','tripduration','from_station_id','from_station_name',  'to_station_id', 'to_station_name', 'usertype', 'gender',   'birthyear')
```

## Aggregating into one Dataframe

``` r
trips_data <- rbind(data_q1, data_q2, data_q3, data_q4)
write.csv(trips_data, file = "trips_data.csv")
```

# Part 2

## Setting up the environment

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
trips_data <- read.csv("trips_data.csv")
```

## Verifying Dataframe structure, checking for duplicates and inconsistent formatting

``` r
str(trips_data)
```

    ## 'data.frame':    3818004 obs. of  13 variables:
    ##  $ X                : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ trip_id          : int  21742443 21742444 21742445 21742446 21742447 21742448 21742449 21742450 21742451 21742452 ...
    ##  $ start_time       : chr  "2019-01-01 00:04:37" "2019-01-01 00:08:13" "2019-01-01 00:13:23" "2019-01-01 00:13:45" ...
    ##  $ end_time         : chr  "2019-01-01 00:11:07" "2019-01-01 00:15:34" "2019-01-01 00:27:12" "2019-01-01 00:43:28" ...
    ##  $ bikeid           : int  2167 4386 1524 252 1170 2437 2708 2796 6205 3939 ...
    ##  $ tripduration     : chr  "390.0" "441.0" "829.0" "1,783.0" ...
    ##  $ from_station_id  : int  199 44 15 123 173 98 98 211 150 268 ...
    ##  $ from_station_name: chr  "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
    ##  $ to_station_id    : int  84 624 644 176 35 49 49 142 148 141 ...
    ##  $ to_station_name  : chr  "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
    ##  $ usertype         : chr  "Subscriber" "Subscriber" "Subscriber" "Subscriber" ...
    ##  $ gender           : chr  "Male" "Female" "Female" "Male" ...
    ##  $ birthyear        : int  1989 1990 1994 1993 1994 1983 1984 1990 1995 1996 ...

``` r
get_dupes(trips_data, trip_id)
```

    ## No duplicate combinations found of: trip_id

    ##  [1] trip_id           dupe_count        X                 start_time       
    ##  [5] end_time          bikeid            tripduration      from_station_id  
    ##  [9] from_station_name to_station_id     to_station_name   usertype         
    ## [13] gender            birthyear        
    ## <0 rows> (or 0-length row.names)

``` r
table(trips_data$usertype)
```

    ## 
    ##   Customer Subscriber 
    ##     880637    2937367

``` r
table(trips_data$gender)
```

    ## 
    ##          Female    Male 
    ##  559206  857978 2400820

``` r
trips_data$usertype <- sub("Customer", "Casual", trips_data$usertype)
trips_data$gender <- sub("^$", "NA", trips_data$gender)
trips_data$tripduration <- gsub(",","",trips_data$tripduration)
trips_data$tripduration <- as.numeric(trips_data$tripduration)
```

## Adding new columns for reducing granularity

``` r
trips_data$day_of_week <- wday(trips_data$start_time)
trips_data$month <- month(trips_data$start_time, label = TRUE)
trips_data$ride_length <- hms::as_hms(difftime(trips_data$end_time,
                                          trips_data$start_time))
trips_data$age = 2019 - trips_data$birthyear
trips_data$age_category <- cut(trips_data$age, breaks = c(-Inf,18,30,59,Inf),
                               labels = c("Children","YA","Adults","Seniors"))
```

## Descriptive Analysis

### Breakdown of usertype with gender & age

``` r
tabyl(trips_data, usertype, gender, age_category) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
```

    ## $Seniors
    ##    usertype Female  Male    NA
    ##      Casual   5.5%  3.5%  5.3%
    ##  Subscriber  94.5% 96.5% 94.7%
    ## 
    ## $Adults
    ##    usertype Female  Male    NA
    ##      Casual  10.8%  6.4% 15.7%
    ##  Subscriber  89.2% 93.6% 84.3%
    ## 
    ## $NA_
    ##    usertype Female   Male    NA
    ##      Casual 100.0% 100.0% 98.8%
    ##  Subscriber   0.0%   0.0%  1.2%
    ## 
    ## $YA
    ##    usertype Female  Male    NA
    ##      Casual  19.4% 12.3% 26.4%
    ##  Subscriber  80.6% 87.7% 73.6%
    ## 
    ## $Children
    ##    usertype Female  Male    NA
    ##      Casual  39.8% 37.5% 76.9%
    ##  Subscriber  60.2% 62.5% 23.1%

``` r
ggplot(trips_data) + geom_bar(mapping = aes(x = age_category, fill = usertype)) +
  scale_y_continuous(trans = 'log10')
```

![](Documentation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Trip duration summary across different categories

``` r
trips_data %>% 
  select(usertype, gender, age_category, tripduration) %>% 
  group_by(usertype, gender, age_category) %>%
  summarise(mean_duration = mean(tripduration), 
            median_duration = median(tripduration), 
            minimum = min(tripduration),
            maximum = max(tripduration))
```

    ## `summarise()` has grouped output by 'usertype', 'gender'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 28 × 7
    ## # Groups:   usertype, gender [6]
    ##    usertype gender age_category mean_duration median_duration minimum maximum
    ##    <chr>    <chr>  <fct>                <dbl>           <dbl>   <dbl>   <dbl>
    ##  1 Casual   Female Children             2490.           1706.      69   93370
    ##  2 Casual   Female YA                   2979.           1496       61 6629854
    ##  3 Casual   Female Adults               3533.           1713       61 6165373
    ##  4 Casual   Female Seniors              4137.           2245       97  849010
    ##  5 Casual   Female <NA>                 1949            1949     1949    1949
    ##  6 Casual   Male   Children             3523.           1485       61  855830
    ##  7 Casual   Male   YA                   2602.           1165       61 7420632
    ##  8 Casual   Male   Adults               2770.           1419       61 6951586
    ##  9 Casual   Male   Seniors              3363.           2104.      70  230240
    ## 10 Casual   Male   <NA>                  351             351      351     351
    ## # … with 18 more rows

### Month-wise trends in ridership

``` r
tabyl(trips_data, month, usertype)
```

    ##  month Casual Subscriber
    ##    Jan   4602      98670
    ##    Feb   2638      93548
    ##    Mar  15923     149688
    ##    Apr  47744     217566
    ##    May  81624     285834
    ##    Jun 130218     345177
    ##    Jul 175632     381683
    ##    Aug 186889     403295
    ##    Sep 129173     364046
    ##    Oct  71035     300751
    ##    Nov  18729     158447
    ##    Dec  16430     138662

``` r
ggplot(trips_data) + geom_bar(mapping = aes(x = month, fill = usertype)) + 
  scale_y_continuous(labels = comma)
```

![](Documentation_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
