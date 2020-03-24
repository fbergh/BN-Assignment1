library(lavaan)
M <- lavCor(ff)
fit <- sem( toString(g_final_final,"lavaan"), sample.cov=M, sample.nobs=nrow(ff) )
fg <- lavaanToGraph(fit, digits=2) 
cg <- coordinates(g_final_final)
coordinates(fg) <- cg
plot(fg, show.coefficients=TRUE)
