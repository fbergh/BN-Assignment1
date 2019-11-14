# Import packages
library(psych)          # For describe()

# Load data
ff_orig = read.csv("forestfires.csv", header = TRUE)
head(ff_orig)

# Pre-process data
month_abb_lower = lapply(month.abb,tolower)
weekend = c("sat","sun")
ff = ff_orig
ff$X = NULL; ff$Y = NULL
ff$month = match(ff$month,month_abb_lower)
ff$day = sapply(ff$day, function(day) day %in% weekend)
head(ff)

# Correlation matrix and basic statistics
ff_desc = describe(ff); ff_desc
cor(ff)

# Plot data to see if normally distributed
hist(ff$FFMC)    # Skewed to right; not normal
hist(ff$DMC)     # Skewed to left; normal-like?
hist(ff$DC)      # Skewed to right; not normal
hist(ff$ISI)     # Normal-like?
hist(ff$temp)    # Normal
hist(ff$RH)      # Skewed to left; normal-like?
hist(ff$wind)    # Normal
hist(ff$rain)    # Skewed to left; needs smaller bins for 0-2 range
hist(ff$area)    # Skewed to left; needs smaller bins for range 0-200 range
