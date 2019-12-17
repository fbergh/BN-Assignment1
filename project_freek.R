# Load packages
library(dagitty)
library(bnlearn)
library(psych)          # For describe()
library(grDevices)      # For outlier detection

# Load data
ff_orig = read.csv("forestfires.csv", header = TRUE)
head(ff_orig)

# Function for creating QQ plots
qq_plot = function(data,ann=FALSE) {
    qqnorm(data,ann=ann)
    qqline(data)
}

rmse = function(true, pred) {
    rmse = sqrt(mean((true-pred)^2))
    rmse
}

# Function for removing outliers based on inter-quantile range
get_outlier_indeces = function(x, k=1.5, na.rm = TRUE, ...) {
  qnt = quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H = k * IQR(x, na.rm = na.rm)
  indcs = c(which(x < (qnt[1] - H)), which(x > (qnt[2] + H)))
  indcs
}

# Pre-process data
month_abb_lower = lapply(month.abb,tolower)
weekend = c("sat","sun")
ff = ff_orig
# Remove X, Y, and rain variables
ff$X = NULL; ff$Y = NULL; ff$rain = NULL
# Convert month variable to numbers and day variable to boolean of weekend
ff$month = match(ff$month,month_abb_lower)
ff$weekend = as.double(sapply(ff$day, function(day) day %in% weekend))
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
ff$month = NULL; ff$day = NULL; ff$area = as.double(ff$area); ff$RH = as.double(ff$RH)
head(ff)

# Outlier removal
# Inspect plots and remove clear measuring errors
png(filename="img/FFMC-qq.png");qq_plot(ff$FFMC);title(main="Quantile-quantile plot of FFMC attribute",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
dev.off()
png(filename="img/FFMC-hist.png");hist(ff$FFMC, breaks=50, ann=FALSE);title(main="Histogram of FFMC attribute",xlab="FFMC",ylab="Frequency")
dev.off()
png(filename="img/ISI-qq.png");qq_plot(ff$ISI);title(main="Quantile-quantile plot of ISI attribute",xlab="Theoretical Quantiles",ylab="Sample Quantiles")
dev.off()
png(filename="img/ISI-hist.png");hist(ff$ISI, breaks=50, ann=FALSE);title(main="Histogram of ISI attribute",xlab="ISI",ylab="Frequency")
dev.off()
nrow(ff); ff = ff[-which(ff$FFMC < 60 | ff$ISI > 30),]; nrow(ff)
qq_plot(ff$FFMC)
hist(ff$FFMC, breaks=50)
qq_plot(ff$ISI)
hist(ff$ISI, breaks=50)

# Do log(area+1) transform of area similar to paper of Cortez & Morais (2007)
qq_plot(ff$area)
ff$area = log(ff$area + 1.0)
qq_plot(ff$area) # Visualise QQ-plot of area to observe normality after addition of constant and log-transform
# Do log(ISI+1) because predictions show an exponential curve and log-transform improves prediction
qq_plot(ff$ISI)
ff$ISI = log(ff$ISI + 1.0)
qq_plot(ff$ISI)

# Get statistics of dataframe
describe(ff)

# Loop over FFMC, DMC, DC, ISI, temp, RH, wind, and area columns (index 1 to 8) and accumulate outlier indeces
# outlier_idcs = c()
# for(col in 1:8) {
#     indcs = get_outlier_indeces(ff[,col],k=3.0)
#     print(paste(paste("Number of outliers for ", paste(names(ff)[col], ":")), length(indcs)))
#     print(indcs)
#     outlier_idcs = c(outlier_idcs, indcs)
# }
# outlier_idcs = outlier_idcs[!duplicated(outlier_idcs)]; length(outlier_idcs)
# length(ff[,1]);ff = ff[-outlier_idcs,];length(ff[,1])

# Create and plot DAG
g = dagitty('
            dag{
                DC [pos="0.141,0.377"]
                DMC [pos="-0.495,0.382"]
                FFMC [pos="-1.160,0.387"]
                ISI [pos="-1.132,0.919"]
                RH [pos="-0.511,-0.482"]
                area [pos="-0.464,1.349"]
                weekend [pos="0.149,0.943"]
                avg_temp [pos="-1,-0.902"]
                temp [pos="-1.168,-0.482"]
                wind [pos="-1.804,-0.445"]
                avg_wind [pos="-2,-1"]
                DC -> area
                DMC -> area
                weekend -> area
                ISI -> area
                FFMC -> ISI
                wind -> ISI
                wind -> FFMC
                RH -> FFMC
                avg_temp -> FFMC
                temp -> FFMC
                RH -> DMC
                avg_temp -> DMC 
                temp -> DMC
                avg_temp -> DC
                temp -> DC
                temp -> RH
                avg_temp -> RH
                avg_temp -> temp
                avg_temp <-> avg_wind
                avg_wind -> wind
                avg_wind -> DC
            }
            ')
plot(g)

# Locally test network and show results with an absolute significant correlation greater than 0.1 (sorted on correlation magnitude)
lt_out = localTests(g,ff)
corr_lt_out = subset(lt_out, p.value<0.05 & abs(estimate)>0.1); corr_lt_out[order(abs(corr_lt_out$estimate)),]

# Fit network to train data
net <- model2network(toString(g,"bnlearn"))
fit <- bn.fit( net, as.data.frame(ff) ); fit

# To en/disable saving plots comment out/in the lines with "png()" and "dev.off()"
# Show ground truth as a function of predictions for area, FFMC, DMC, DC, and ISI
preds_area = predict(fit, node="area", data=ff)
png(filename="img/area-preds-plot.png")
plot(preds_area, ff$area, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Ground truth of area plotted as a function of the predictions", xlab="Predictions of area", ylab="Ground truth of area")
dev.off()

preds_FFMC = predict(fit, node="FFMC", data=ff[c("wind","temp","avg_temp","RH")])
png(filename="img/FFMC-preds-plot.png")
plot(preds_FFMC, ff$FFMC, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Ground truth of FFMC plotted as a function of the predictions", xlab="Predictions of FFMC", ylab="Ground truth of FFMC")
dev.off()

preds_DMC = predict(fit, node="DMC", data=ff[c("avg_temp","temp","RH")])
png(filename="img/DMC-preds-plot.png")
plot(preds_DMC, ff$DMC, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Ground truth of DMC plotted as a function of the predictions", xlab="Predictions of DMC", ylab="Ground truth of DMC")
dev.off()

preds_DC = predict(fit, node="DC", data=ff[c("avg_temp","avg_wind","temp")])
png(filename="img/DC-preds-plot.png")
plot(preds_DC, ff$DC, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Ground truth of DC plotted as a function of the predictions", xlab="Predictions of DC", ylab="Ground truth of DC")
dev.off()

preds_ISI = predict(fit, node="ISI", data=ff[c("wind","FFMC")])
png(filename="img/ISI-preds-plot.png")
plot(preds_ISI, ff$ISI, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Ground truth of ISI plotted as a function of the predictions", xlab="Predictions of ISI", ylab="Ground truth of ISI")
dev.off()

# Calculate root mean squared errors
rmse_area = rmse(ff$area, preds_area); mad_area; mean(ff$area); sd(ff$area)
rmse_FFMC = rmse(preds_FFMC, ff$FFMC); rmse_FFMC; mean(ff$FFMC); sd(ff$FFMC)
rmse_DMC = rmse(preds_DMC, ff$DMC); rmse_DMC; mean(ff$DMC); sd(ff$DMC)
rmse_DC = rmse(preds_DC, ff$DC); rmse_DC; mean(ff$DC); sd(ff$DC)
rmse_ISI = rmse(preds_ISI, ff$ISI); rmse_ISI; mean(ff$ISI); sd(ff$ISI)

# Calculate correlation coefficients
cor.test(preds_area, ff$area)
cor.test(preds_FFMC,ff$FFMC)
cor.test(preds_DMC,ff$DMC)
cor.test(preds_DC,ff$DC)
cor.test(preds_ISI, ff$ISI)