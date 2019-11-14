# Import packages
library(psych)          # For describe()

# Function for creating QQ plots
qq_plot = function(data) {
  qqnorm(data)
  qqline(data)
}

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
hist(ff$FFMC, breaks=50)    # Skewed to right; not normal
hist(ff$DMC, breaks=50)     # Skewed to left; normal-like?
hist(ff$DC, breaks=50)      # Skewed to right; not normal
hist(ff$ISI, breaks=50)     # Normal-like?
hist(ff$temp, breaks=50)    # Normal
hist(ff$RH, breaks=50)      # Skewed to left; normal-like?
hist(ff$wind, breaks=50)    # Normal
hist(ff$rain, breaks=50)    # Skewed to left; needs smaller bins for 0-2 range
hist(ff$area, breaks=50)    # Skewed to left; needs smaller bins for 0-200 range
hist(log(ff$rain + 0.0001), breaks=50)
hist(log(ff$area + 0.0001), breaks=50)

# Create QQ plots of all attributes
qq_plot(ff$FFMC)
qq_plot(ff$DMC)
qq_plot(ff$DC)
qq_plot(ff$ISI)
qq_plot(ff$temp)
qq_plot(ff$RH)
qq_plot(ff$wind)
qq_plot(ff$rain)
qq_plot(ff$area)
qq_plot(log(ff$rain + 0.0001))
qq_plot(log(ff$area + 0.0001))
