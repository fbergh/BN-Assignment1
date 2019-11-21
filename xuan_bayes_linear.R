library(dagitty)
library(bnlearn)
g <- dagitty("dag{
             month -> wind
             month -> temp
             wind -> FFMC
             wind -> ISI
             temp -> RH
             temp -> DMC
             temp -> DC
             temp -> FFMC
             RH -> FFMC
             
             RH -> DMC
             
             FFMC -> ISI
             DMC -> area
             DC -> area
             ISI -> area
        
}")
# can't run.....
# plot(graphLayout(g))
impliedConditionalIndependencies(g)
localTests(g,ff)
# add 5 more edges
g2 <- dagitty("dag{
             month -> wind
             month -> temp
             month -> DC
             month -> DMC
             wind -> FFMC
             wind -> ISI
             wind -> temp
             temp -> RH
             temp -> ISI
             temp -> DMC
             temp -> DC
             temp -> FFMC
             RH -> FFMC
          
             RH -> DMC
             FFMC -> DMC
             FFMC -> ISI
             DMC -> area
             DMC -> DC
             DC -> area
             ISI -> area
        
}")
# fitting to the network

net <- model2network(toString(g2,"bnlearn"))
fit <- bn.fit(net, as.data.frame(scale(ff)))
fit
# with the original scale
ff$month <- as.double(ff$month)
ff$RH <- as.double(ff$RH)
fit <- bn.fit(net,ff)
fit
# prediction
# predict(fit, node = "area", data = data.frame(DC = as.double(1:1000)), DMC = as.double(1:300), ISI = as.double(1:60))
