``` r
setwd("/Users/preciouschukwubem/Library/CloudStorage/OneDrive-AuburnUniversity/Reproducibility In Science")
```

``` r
library(ggplot2)
library(drc) 
```

    ## Loading required package: MASS

    ## 
    ## 'drc' has been loaded.

    ## Please cite R and 'drc' if used for a publication,

    ## for references type 'citation()' and 'citation('drc')'.

    ## 
    ## Attaching package: 'drc'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     gaussian, getInitial

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ lubridate 1.9.5     ✔ tibble    3.3.1
    ## ✔ purrr     1.2.1     ✔ tidyr     1.3.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ dplyr::select() masks MASS::select()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
#install.packages("drc")
library(drc)
```

## 1: 1. Reproducibility (1 pt)

Writing your own functions and iterations ensures reproducibility by
making analyses consistent, reusable, and less error-prone. Instead of
repeating manual calculations, functions and loops automate processes so
that results can be replicated exactly across datasets or by other
researchers.

## 2: 2. Conceptual Explanation (2 pts)

A function in R is created using the function() keyword. It takes inputs
(arguments), performs operations inside curly braces {}, and returns a
result using return() (or automatically returns the last line).
Functions are written in script or R Markdown code chunks and help avoid
repetition.

A for loop in R is used to repeat a block of code for a sequence of
values. The syntax is for (i in sequence) {} where i takes each value in
the sequence. The loop runs the code inside the braces for each
iteration, and results can be printed or stored in objects such as
vectors or dataframes.

## Read in Data

``` r
cities <- read.csv("Cities.csv", header = TRUE)
```

## Function: Haversine Distance (6 pts)

``` r
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  
  # convert to radians
  rad.lat1 <- lat1 * pi/180
  rad.lon1 <- lon1 * pi/180
  rad.lat2 <- lat2 * pi/180
  rad.lon2 <- lon2 * pi/180
  
  # Haversine formula
  delta_lat <- rad.lat2 - rad.lat1
  delta_lon <- rad.lon2 - rad.lon1
  a <- sin(delta_lat / 2)^2 + cos(rad.lat1) * cos(rad.lat2) * sin(delta_lon / 2)^2
  c <- 2 * asin(sqrt(a)) 
  
  # Earth's radius in kilometers
  earth_radius <- 6378137
  
  # Calculate distance
  distance_km <- (earth_radius * c)/1000
  
  return(distance_km)
}
```

## Distance: Auburn → New York (6 pts)

``` r
# Extract coordinates
auburn_lat <- cities$lat[cities$city == "Auburn"]
auburn_lon <- cities$long[cities$city == "Auburn"]

nyc_lat <- cities$lat[cities$city == "New York"]
nyc_lon <- cities$long[cities$city == "New York"]

# Calculate distance
distance_nyc <- haversine_distance(auburn_lat, auburn_lon, nyc_lat, nyc_lon)
distance_nyc
```

    ## [1] 1367.854

## For loop to calculate distances to all cities (4 pts)

``` r
# Auburn coordinates
aub_lat <- cities$lat[cities$city == "Auburn"]
aub_lon <- cities$long[cities$city == "Auburn"]

# For loop to print first 9 distances (as requested)
for (i in 1:nrow(cities)) {
  city_lat <- cities$lat[i]
  city_lon <- cities$long[i]
  dist <- haversine_distance(aub_lat, aub_lon, city_lat, city_lon)
  print(dist)
}
```

    ## [1] 1367.854
    ## [1] 3051.838
    ## [1] 1045.521
    ## [1] 916.4138
    ## [1] 993.0298
    ## [1] 1056.022
    ## [1] 1239.973
    ## [1] 162.5121
    ## [1] 1036.99
    ## [1] 1665.699
    ## [1] 2476.255
    ## [1] 1108.229
    ## [1] 3507.959
    ## [1] 3388.366
    ## [1] 2951.382
    ## [1] 1530.2
    ## [1] 591.1181
    ## [1] 1363.207
    ## [1] 1909.79
    ## [1] 1380.138
    ## [1] 2961.12
    ## [1] 2752.814
    ## [1] 1092.259
    ## [1] 796.7541
    ## [1] 3479.538
    ## [1] 1290.549
    ## [1] 3301.992
    ## [1] 1191.666
    ## [1] 608.2035
    ## [1] 2504.631
    ## [1] 3337.278
    ## [1] 800.1452
    ## [1] 1001.088
    ## [1] 732.5906
    ## [1] 1371.163
    ## [1] 1091.897
    ## [1] 1043.273
    ## [1] 851.3423
    ## [1] 1382.372
    ## [1] 0

## Build Dataframe in Loop (4 pts)

``` r
# Build dataframe of distances using a for loop
distance_df <- data.frame(
  City1 = character(),
  City2 = character(),
  Distance_km = numeric(),
  stringsAsFactors = FALSE
)

aub_lat <- cities$lat[cities$city == "Auburn"]
aub_lon <- cities$long[cities$city == "Auburn"]

for (i in 1:nrow(cities)) {
  if (cities$city[i] != "Auburn") {
    city_lat <- cities$lat[i]
    city_lon <- cities$long[i]
    
    dist <- haversine_distance(aub_lat, aub_lon, city_lat, city_lon)
    
    new_row <- data.frame(
      City1 = cities$city[i],
      City2 = "Auburn",
      Distance_km = dist,
      stringsAsFactors = FALSE
    )
    
    distance_df <- rbind(distance_df, new_row)
  }
}

# Display the full dataframe
distance_df
```

    ##            City1  City2 Distance_km
    ## 1       New York Auburn   1367.8540
    ## 2    Los Angeles Auburn   3051.8382
    ## 3        Chicago Auburn   1045.5213
    ## 4          Miami Auburn    916.4138
    ## 5        Houston Auburn    993.0298
    ## 6         Dallas Auburn   1056.0217
    ## 7   Philadelphia Auburn   1239.9732
    ## 8        Atlanta Auburn    162.5121
    ## 9     Washington Auburn   1036.9900
    ## 10        Boston Auburn   1665.6985
    ## 11       Phoenix Auburn   2476.2552
    ## 12       Detroit Auburn   1108.2288
    ## 13       Seattle Auburn   3507.9589
    ## 14 San Francisco Auburn   3388.3656
    ## 15     San Diego Auburn   2951.3816
    ## 16   Minneapolis Auburn   1530.2000
    ## 17         Tampa Auburn    591.1181
    ## 18      Brooklyn Auburn   1363.2072
    ## 19        Denver Auburn   1909.7897
    ## 20        Queens Auburn   1380.1382
    ## 21     Riverside Auburn   2961.1199
    ## 22     Las Vegas Auburn   2752.8142
    ## 23     Baltimore Auburn   1092.2595
    ## 24     St. Louis Auburn    796.7541
    ## 25      Portland Auburn   3479.5376
    ## 26   San Antonio Auburn   1290.5492
    ## 27    Sacramento Auburn   3301.9923
    ## 28        Austin Auburn   1191.6657
    ## 29       Orlando Auburn    608.2035
    ## 30      San Juan Auburn   2504.6312
    ## 31      San Jose Auburn   3337.2781
    ## 32  Indianapolis Auburn    800.1452
    ## 33    Pittsburgh Auburn   1001.0879
    ## 34    Cincinnati Auburn    732.5906
    ## 35     Manhattan Auburn   1371.1633
    ## 36   Kansas City Auburn   1091.8970
    ## 37     Cleveland Auburn   1043.2727
    ## 38      Columbus Auburn    851.3423
    ## 39         Bronx Auburn   1382.3721
