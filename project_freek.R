# Load packages
library(dagitty)
library(bnlearn)
library(psych)          # For describe()

# Load data
ff_orig = read.csv("forestfires.csv", header = TRUE)
head(ff_orig)

# Function for creating QQ plots
qq_plot = function(data) {
    qqnorm(data)
    qqline(data)
}

# Pre-process data
month_abb_lower = lapply(month.abb,tolower)
weekend = c("sat","sun")
ff = ff_orig
ff$X = NULL; ff$Y = NULL; ff$rain = NULL
ff$month = match(ff$month,month_abb_lower)
ff$day = sapply(ff$day, function(day) day %in% weekend)
head(ff)

# Replace month numbers with average temperatures per month of Bragança, a place nearby Monteshino Park
# Retrieved from https://www.climatestotravel.com/climate/portugal/bragança
braganca_min_temp = c(0,1,3,5,8,12,14,14,12,8,4,1)
braganca_max_temp = c(9,11,15,16,20,26,29,29,25,18,13,10)
braganca_avg_temp = (braganca_min_temp + braganca_max_temp)/2
ff$avg_temp = braganca_avg_temp[ff$month]
braganca_avg_wind = c(5,6,6,6,5,5,5,5,4,4,5,4)
ff$avg_wind = as.double(braganca_avg_wind[ff$month])
head(ff)

# Make all columns double
ff$month = as.double(ff$month); ff$day = as.double(ff$day); ff$area = as.double(ff$area); ff$RH = as.double(ff$RH)
head(ff)

qq_plot(ff$area)
# Add small value to zero values in area column such that the log-transform does not give "Inf" values
ff$area[ff$area < 0.5] = ff$area[ff$area < 0.5] + 0.5
ff$area = log(ff$area)
# Visualise QQ-plot of area to observe normality after addition of constant and log-transform
qq_plot(ff$area)

# Create train-test split (80/20)
train_size = floor(0.8 * nrow(ff)); train_size
set.seed(420)   # Seed for reproducible split
train_ind <- sample(seq_len(nrow(ff)), size = train_size)
ff_train <- ff[train_ind, ]
ff_test <- ff[-train_ind, ]

# Describe test and train sets to make sure they are partitioned fairly
describe(ff_train)[c("n","mean","sd","median","min","max","range")]
describe(ff_test)[c("n","mean","sd","median","min","max","range")]

# Create and plot DAG
g = dagitty('
            dag{
                DC [pos="0.141,0.377"]
                DMC [pos="-0.495,0.382"]
                FFMC [pos="-1.160,0.387"]
                ISI [pos="-1.132,0.919"]
                RH [pos="-0.511,-0.482"]
                area [pos="-0.464,1.349"]
                day [pos="0.149,0.943"]
                avg_temp [pos="-1.510,-0.902"]
                temp [pos="-1.168,-0.482"]
                wind [pos="-1.804,-0.445"]
                avg_wind [pos="-2,-1"]
                DC -> area
                DMC -> area
                day -> area
                ISI -> area
                FFMC -> ISI
                wind -> ISI
                temp -> FFMC
                wind -> FFMC
                RH -> FFMC
                avg_temp -> FFMC
                temp -> DMC
                RH -> DMC
                avg_temp -> DMC
                temp -> DC
                avg_temp -> DC
                temp -> RH
                avg_temp -> temp
                avg_wind -> wind
            }
            ')

plot(g)
impliedConditionalIndependencies(g)

# Locally test network and show results with an absolute significant correlation greater than 0.1 (sorted on correlation magnitude)
lt_out = localTests(g,ff)
corr_lt_out = subset(lt_out, p.value<0.05 & abs(estimate)>0.1); corr_lt_out[order(abs(corr_lt_out$estimate)),]


# Fit network to train data
net <- model2network(toString(g,"bnlearn"))
fit <- bn.fit( net, as.data.frame(ff_train) ); fit

# Predict area given the test data and compute absolute error
preds = predict(fit, node="area", data=ff_test)
abs_error = abs(ff_test$area - preds); abs_error

# Show ground truth and predictions
ff_test$area; preds
plot(preds, ff_test$area)
cor.test(preds, ff_test$area)