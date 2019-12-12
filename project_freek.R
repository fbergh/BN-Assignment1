# Load packages
library(dagitty)
library(bnlearn)
library(psych)          # For describe()
library(grDevices)      # For outlier detection

# Load data
ff_orig = read.csv("forestfires.csv", header = TRUE)
head(ff_orig)

# Function for creating QQ plots
qq_plot = function(data) {
    qqnorm(data)
    qqline(data)
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

qq_plot(ff$area)
# Do log(area+1) transform of area similar to paper
ff$area = log(ff$area + 1.0)
# Visualise QQ-plot of area to observe normality after addition of constant and log-transform
qq_plot(ff$area)
dev.off()
describe(ff)

# Loop over FFMC, DMC, DC, ISI, temp, RH, wind, and area columns (index 1 to 8) and accumulate outlier indeces
outlier_idcs = c()
for(col in 1:8) {
    indcs = get_outlier_indeces(ff[,col])
    print(names(ff)[col]); print(length(indcs))
    outlier_idcs = c(outlier_idcs, indcs)
}
outlier_idcs = outlier_idcs[!duplicated(outlier_idcs)]; length(outlier_idcs)
length(ff[,1]);ff = ff[-outlier_idcs,];length(ff[,1])

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

# Predict area given the test data and compute absolute error
preds_area = predict(fit, node="area", data=ff)

# To en/disable saving plots comment out/in the lines with "png()" and "dev.off()"
# Show ground truth and predictions
# png(filename="img/area-preds-plot.png")
plot(preds_area, ff$area, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Predictions of area plotted as a function of the ground truth"); title(xlab="Ground Truth of area"); title(ylab="Predictions of area")
# dev.off()
mad_area = mean(abs(ff$area - preds_area)); mad_area; mean(ff$area); sd(ff$area)
cor.test(preds_area, ff$area)

# Predict FFMC, DMC, DC, and ISI and plot them
preds_FFMC = predict(fit, node="FFMC", data=ff[c("wind","temp","avg_temp","RH")])
# png(filename="img/FFMC-preds-plot.png")
plot(preds_FFMC, ff$FFMC, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Predictions of FFMC plotted as a function of the ground truth"); title(xlab="Ground Truth of FFMC"); title(ylab="Predictions of FFMC")
# dev.off()
mad_FFMC = mean(abs(preds_FFMC-ff$FFMC)); mad_FFMC; mean(ff$FFMC); sd(ff$FFMC)
cor.test(preds_FFMC,ff$FFMC)

preds_DMC = predict(fit, node="DMC", data=ff[c("avg_temp","temp","RH")])
# png(filename="img/DMC-preds-plot.png")
plot(preds_DMC, ff$DMC, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Predictions of DMC plotted as a function of the ground truth"); title(xlab="Ground Truth of DMC"); title(ylab="Predictions of DMC")
# dev.off()
mad_DMC = mean(abs(preds_DMC-ff$DMC)); mad_DMC; mean(ff$DMC); sd(ff$DMC)
cor.test(preds_DMC,ff$DMC)

preds_DC = predict(fit, node="DC", data=ff[c("avg_temp","avg_wind","temp")])
# png(filename="img/DC-preds-plot.png")
plot(preds_DC, ff$DC, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Predictions of DC plotted as a function of the ground truth"); title(xlab="Ground Truth of DC"); title(ylab="Predictions of DC")
# dev.off()
mad_DC = mean(abs(preds_DC-ff$DC)); mad_DC; mean(ff$DC); sd(ff$DC)
cor.test(preds_DC,ff$DC)

preds_ISI = predict(fit, node="ISI", data=ff[c("wind","FFMC")])
# png(filename="img/ISI-preds-plot.png")
plot(preds_ISI, ff$ISI, ann=FALSE); abline(coef = c(0,1), c="red"); title(main="Predictions of ISI plotted as a function of the ground truth"); title(xlab="Ground Truth of ISI"); title(ylab="Predictions of ISI")
# dev.off()
mad_ISI = mean(abs(preds_ISI-ff$ISI)); mad_ISI; mean(ff$ISI); sd(ff$ISI)
cor.test(preds_ISI, ff$ISI)
