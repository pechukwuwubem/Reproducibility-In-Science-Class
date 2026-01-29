# ------------------------------------------------------------------------------
# Script: IntroR
# Purpose: Intro to R practise Assignment
# Author: [ Precious Chukwubem]
# Date: January 29, 2026
# ------------------------------------------------------------------------------

# 1. Create a vector named 'z' with the values 1 to 200
z <- 1:200

# 2. Print the mean and standard deviation of z on the console
cat("Mean of z:", mean(z), "\n")
cat("Standard deviation of z:", sd(z), "\n\n")

# 3. Create a logical vector named zlog that is TRUE for z values > 30
zlog <- z > 30

# 4. Make a data frame with z and zlog as columns, named zdf
zdf <- data.frame(z, zlog)

# 5. Change the column names to “zvec” and “zlogic”
colnames(zdf) <- c("zvec", "zlogic")

# 6. Add a new column zsquared = zvec squared
zdf$zsquared <- zdf$zvec ^ 2

# Optional: look at the first few rows (you can comment this out)
# head(zdf)

# 7. Subset the dataframe (two ways) to only include rows where 
#    zsquared > 10 and zsquared < 100

# Way 1: using subset() function
subset_zdf <- subset(zdf, zsquared > 10 & zsquared < 100)

# Way 2: using logical indexing (base R bracket notation)
logical_zdf <- zdf[zdf$zsquared > 10 & zdf$zsquared < 100, ]

# Check that both methods give the same result
# nrow(subset_zdf) == nrow(logical_zdf)   # should be TRUE

cat("Number of rows where 10 < zsquared < 100:", nrow(subset_zdf), "\n\n")

# 8. Subset zdf to only include row 26
row_26 <- zdf[26, ]
cat("Row 26:\n")
print(row_26)

# Alternative using subset():
# subset(zdf, rownames(zdf) == "26")   # less common

# 9. Get only the value in column zsquared in the 180th row
zsquared_180 <- zdf$zsquared[180]
# or: zdf[180, "zsquared"]
# or: zdf[180, 3]

cat("\nValue of zsquared in row 180:", zsquared_180, "\n")

# Optional: show what row 180 looks like
cat("\nFull row 180:\n")
print(zdf[180, ])

read.csv("TipsR.csv", 
         na.strings = ".",          # convert . to NA
         stringsAsFactors = FALSE)  # keep character columns as character
