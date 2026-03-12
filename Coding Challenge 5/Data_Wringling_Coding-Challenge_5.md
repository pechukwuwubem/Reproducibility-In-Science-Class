    setwd("/Users/preciouschukwubem/Library/CloudStorage/OneDrive-AuburnUniversity/Reproducibility In Science")

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Question 1: `1.   3 pts. Download two .csv files from Canvas called DiversityData.csv and Metadata.csv, and read them into R using relative file paths.`

# Read the Diversity Data file

    diversity_data <- read.csv("DiversityData.csv", header = TRUE)

# Read the Metadata file

    metadata <- read.csv("Metadata.csv", header = TRUE)

# View the first few rows

    head(diversity_data)

    ##     Code  shannon invsimpson   simpson richness
    ## 1 S01_13 6.624921   210.7279 0.9952545     3319
    ## 2 S02_16 6.612413   206.8666 0.9951660     3079
    ## 3 S03_19 6.660853   213.0184 0.9953056     3935
    ## 4 S04_22 6.660671   204.6908 0.9951146     3922
    ## 5 S05_25 6.610965   200.2552 0.9950064     3196
    ## 6 S06_28 6.650812   199.3211 0.9949830     3481

    head(metadata)

    ##     Code Crop Time_Point Replicate Water_Imbibed
    ## 1 S01_13 Soil          0         1            na
    ## 2 S02_16 Soil          0         2            na
    ## 3 S03_19 Soil          0         3            na
    ## 4 S04_22 Soil          0         4            na
    ## 5 S05_25 Soil          0         5            na
    ## 6 S06_28 Soil          0         6            na

# Check structure of the datasets

    str(diversity_data)

    ## 'data.frame':    70 obs. of  5 variables:
    ##  $ Code      : chr  "S01_13" "S02_16" "S03_19" "S04_22" ...
    ##  $ shannon   : num  6.62 6.61 6.66 6.66 6.61 ...
    ##  $ invsimpson: num  211 207 213 205 200 ...
    ##  $ simpson   : num  0.995 0.995 0.995 0.995 0.995 ...
    ##  $ richness  : int  3319 3079 3935 3922 3196 3481 3250 3170 3657 3177 ...

    str(metadata)

    ## 'data.frame':    70 obs. of  5 variables:
    ##  $ Code         : chr  "S01_13" "S02_16" "S03_19" "S04_22" ...
    ##  $ Crop         : chr  "Soil" "Soil" "Soil" "Soil" ...
    ##  $ Time_Point   : int  0 0 0 0 0 0 6 6 6 6 ...
    ##  $ Replicate    : int  1 2 3 4 5 6 1 2 3 4 ...
    ##  $ Water_Imbibed: chr  "na" "na" "na" "na" ...

# Question 2 `2.    4 pts. Join the two dataframes together by the common column ‘Code’. Name the resulting dataframe alpha.`

# Join the two dataframes by the common column “Code”

    alpha <- merge(diversity_data, metadata, by = "Code")

    str(alpha)

    ## 'data.frame':    70 obs. of  9 variables:
    ##  $ Code         : chr  "C01_11" "C02_14" "C03_17" "C04_20" ...
    ##  $ shannon      : num  6.62 6.63 6.62 6.63 6.64 ...
    ##  $ invsimpson   : num  221 211 216 216 211 ...
    ##  $ simpson      : num  0.995 0.995 0.995 0.995 0.995 ...
    ##  $ richness     : int  3076 3180 2938 3371 3435 3629 2779 3193 2859 2950 ...
    ##  $ Crop         : chr  "Cotton" "Cotton" "Cotton" "Cotton" ...
    ##  $ Time_Point   : int  0 0 0 0 0 0 12 12 12 12 ...
    ##  $ Replicate    : int  1 2 3 4 5 6 1 2 3 4 ...
    ##  $ Water_Imbibed: chr  "0.0042" "0.0091" "0.0013" "0.0087" ...

# Question 3: `3.   4 pts. Calculate Pielou’s evenness index: Pielou’s evenness is an ecological parameter calculated by the Shannon diversity index (column Shannon) divided by the log of the richness column.`

1.  Using mutate, create a new column to calculate Pielou’s evenness
    index.
2.  Name the resulting dataframe alpha\_even.

<!-- -->

    library(dplyr)
    alpha_even <- mutate(alpha, Pielou_evenness = shannon / log(richness))

    head(alpha_even)

    ##     Code  shannon invsimpson   simpson richness   Crop Time_Point Replicate
    ## 1 C01_11 6.618126   220.6622 0.9954682     3076 Cotton          0         1
    ## 2 C02_14 6.627206   211.0392 0.9952615     3180 Cotton          0         2
    ## 3 C03_17 6.616958   216.0663 0.9953718     2938 Cotton          0         3
    ## 4 C04_20 6.626465   215.9390 0.9953691     3371 Cotton          0         4
    ## 5 C05_23 6.642822   211.0896 0.9952627     3435 Cotton          0         5
    ## 6 C06_26 6.679131   216.3135 0.9953771     3629 Cotton          0         6
    ##   Water_Imbibed Pielou_evenness
    ## 1        0.0042       0.8240330
    ## 2        0.0091       0.8217613
    ## 3        0.0013       0.8286233
    ## 4        0.0087       0.8157692
    ## 5        0.0075       0.8158938
    ## 6        0.0046       0.8148549

    str(alpha_even)

    ## 'data.frame':    70 obs. of  10 variables:
    ##  $ Code           : chr  "C01_11" "C02_14" "C03_17" "C04_20" ...
    ##  $ shannon        : num  6.62 6.63 6.62 6.63 6.64 ...
    ##  $ invsimpson     : num  221 211 216 216 211 ...
    ##  $ simpson        : num  0.995 0.995 0.995 0.995 0.995 ...
    ##  $ richness       : int  3076 3180 2938 3371 3435 3629 2779 3193 2859 2950 ...
    ##  $ Crop           : chr  "Cotton" "Cotton" "Cotton" "Cotton" ...
    ##  $ Time_Point     : int  0 0 0 0 0 0 12 12 12 12 ...
    ##  $ Replicate      : int  1 2 3 4 5 6 1 2 3 4 ...
    ##  $ Water_Imbibed  : chr  "0.0042" "0.0091" "0.0013" "0.0087" ...
    ##  $ Pielou_evenness: num  0.824 0.822 0.829 0.816 0.816 ...

# Question 4: `4.   4. Pts. Using tidyverse language of functions and the pipe, use the summarise function and tell me the mean and standard error evenness grouped by crop over time.`

1.  Start with the alpha\_even dataframe
2.  Group the data: group the data by Crop and Time\_Point.
3.  Summarize the data: Calculate the mean, count, standard deviation,
    and standard error for the even variable within each group.
4.  Name the resulting dataframe alpha\_average

<!-- -->

    library(tidyverse)
    alpha_average <- alpha_even %>%
      group_by(Crop, Time_Point) %>%
      summarise(
        mean_even = mean(Pielou_evenness),
        n = n(),
        sd_even = sd(Pielou_evenness),
        se_even = sd_even / sqrt(n)
      )

    ## `summarise()` has regrouped the output.
    ## ℹ Summaries were computed grouped by Crop and Time_Point.
    ## ℹ Output is grouped by Crop.
    ## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
    ## ℹ Use `summarise(.by = c(Crop, Time_Point))` for per-operation grouping
    ##   (`?dplyr::dplyr_by`) instead.

    head(alpha_average)

    ## # A tibble: 6 × 6
    ## # Groups:   Crop [2]
    ##   Crop   Time_Point mean_even     n sd_even se_even
    ##   <chr>       <int>     <dbl> <int>   <dbl>   <dbl>
    ## 1 Cotton          0     0.820     6 0.00556 0.00227
    ## 2 Cotton          6     0.805     6 0.00920 0.00376
    ## 3 Cotton         12     0.767     6 0.0157  0.00640
    ## 4 Cotton         18     0.755     5 0.0169  0.00755
    ## 5 Soil            0     0.814     6 0.00765 0.00312
    ## 6 Soil            6     0.810     6 0.00587 0.00240

    str(alpha_average)

    ## gropd_df [12 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)
    ##  $ Crop      : chr [1:12] "Cotton" "Cotton" "Cotton" "Cotton" ...
    ##  $ Time_Point: int [1:12] 0 6 12 18 0 6 12 18 0 6 ...
    ##  $ mean_even : num [1:12] 0.82 0.805 0.767 0.755 0.814 ...
    ##  $ n         : int [1:12] 6 6 6 5 6 6 6 5 6 6 ...
    ##  $ sd_even   : num [1:12] 0.00556 0.0092 0.01567 0.01689 0.00765 ...
    ##  $ se_even   : num [1:12] 0.00227 0.00376 0.0064 0.00755 0.00312 ...
    ##  - attr(*, "groups")= tibble [3 × 2] (S3: tbl_df/tbl/data.frame)
    ##   ..$ Crop : chr [1:3] "Cotton" "Soil" "Soybean"
    ##   ..$ .rows: list<int> [1:3] 
    ##   .. ..$ : int [1:4] 1 2 3 4
    ##   .. ..$ : int [1:4] 5 6 7 8
    ##   .. ..$ : int [1:4] 9 10 11 12
    ##   .. ..@ ptype: int(0) 
    ##   ..- attr(*, ".drop")= logi TRUE

\#Question 5:
`5.    4. Pts. Calculate the difference between the soybean column, the soil column, and the difference between the cotton column and the soil column`
a. Start with the alpha\_average dataframe b. Select relevant columns:
select the columns Time\_Point, Crop, and mean.even. c. Reshape the
data: Use the pivot\_wider function to transform the data from long to
wide format, creating new columns for each Crop with values from
mean.even. d. Calculate differences: Create new columns named
diff.cotton.even and diff.soybean.even by calculating the difference
between Soil and Cotton, and Soil and Soybean, respectively. e. Name the
resulting dataframe alpha\_average2

    library(tidyverse)

    alpha_average2 <- alpha_average %>%
      select(Time_Point, Crop, mean_even) %>%
      pivot_wider(names_from = Crop, values_from = mean_even) %>%
      mutate(
        diff.cotton.even = Soil - Cotton,
        diff.soybean.even = Soil - Soybean
      )

    str(alpha_average2)

    ## tibble [4 × 6] (S3: tbl_df/tbl/data.frame)
    ##  $ Time_Point       : int [1:4] 0 6 12 18
    ##  $ Cotton           : num [1:4] 0.82 0.805 0.767 0.755
    ##  $ Soil             : num [1:4] 0.814 0.81 0.798 0.8
    ##  $ Soybean          : num [1:4] 0.822 0.764 0.687 0.716
    ##  $ diff.cotton.even : num [1:4] -0.00602 0.00507 0.03129 0.0449
    ##  $ diff.soybean.even: num [1:4] -0.0074 0.0459 0.1119 0.0833

    head(alpha_average2)

    ## # A tibble: 4 × 6
    ##   Time_Point Cotton  Soil Soybean diff.cotton.even diff.soybean.even
    ##        <int>  <dbl> <dbl>   <dbl>            <dbl>             <dbl>
    ## 1          0  0.820 0.814   0.822         -0.00602          -0.00740
    ## 2          6  0.805 0.810   0.764          0.00507           0.0459 
    ## 3         12  0.767 0.798   0.687          0.0313            0.112  
    ## 4         18  0.755 0.800   0.716          0.0449            0.0833

# Question 6: `6.   4 pts. Connecting it to plots`

1.  Start with the alpha\_average2 dataframe
2.  Select relevant columns: select the columns Time\_Point,
    diff.cotton.even, and diff.soybean.even.
3.  Reshape the data: Use the pivot\_longer function to transform the
    data from wide to long format, creating a new column named diff that
    contains the values from diff.cotton.even and diff.soybean.even.
4.  This might be challenging, so I’ll give you a break. The code is
    below.

pivot\_longer(c(diff.cotton.even, diff.soybean.even), names\_to =
“diff”)

1.  Create the plot: Use ggplot and geom\_line() with ‘Time\_Point’ on
    the x-axis, the column ‘values’ on the y-axis, and different colors
    for each ‘diff’ category. The column named ‘values’ come from the
    pivot\_longer. The resulting plot should look like the one to the
    right.

<!-- -->

    library(tidyverse)

    alpha_average3 <- alpha_average2 %>%
      select(Time_Point, diff.cotton.even, diff.soybean.even) %>%
      pivot_longer(
        c(diff.cotton.even, diff.soybean.even),
        names_to = "diff",
        values_to = "values"
      )

    ggplot(alpha_average3, aes(x = Time_Point, y = values, color = diff)) +
      geom_line() +
      labs(
        x = "Time (hrs)",
        y = "Difference from Soil in Pielou's Evenness"
      ) +
      theme_classic()

![](Data_Wringling_Coding-Challenge_5_files/figure-markdown_strict/unnamed-chunk-15-1.png)
