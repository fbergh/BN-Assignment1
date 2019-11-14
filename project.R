library(psych)

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

