#############################################################
## R Code for Dissertation: Mixed Logit Mode Specification ##
#############################################################

## Multinomial Logit 1

library(mlogit)
library(data.table)

logit.formula.multi1 <- mFormula(IsChosenAlternative ~  Price + TravelTime)
logit.model.multi1 <- mlogit(formula=logit.formula.multi1, data=all.data.long.ss, reflevel="Car/van driver")

######## Empirical Mixing Distribution ########

summary(logit.model.multi1)

coefficient.names <- colnames(t(data.frame(logit.model.multi1$coefficients)))
coefficient.length <- length(coefficient.names)
coefficient.frame <- as.data.frame(setNames(replicate(coefficient.length, numeric(0)), coefficient.names))

coefficient.frame <- data.table(coefficient.frame)
plot(density(coefficient.frame[,`Bus:(intercept)`]))

for (i in unique(all.data.long.ss$TripID)) {
        logit.empirical <- mlogit(formula=logit.formula.multi1, data=all.data.long.ss[with(all.data.long.ss, TripID!=i),])   
        coefficient.frame <- rbind(coefficient.frame, t(logit.empirical$coefficients))
        }

library(xtable)
xtable(logit.model.multi1$coefficients)
plot(choice.mlogit[,1])
## Multinomial Logit 2

## TEST OUT STARGASER
## HAVE MANY MODEL SPECIFICATIONS TO TEST

logit.formula.multi2 <- mFormula(IsChosenAlternative ~ Price + TravelTime | TripPurpose )
logit.model.multi2 <- mlogit(formula=logit.formula.multi2, data=all.data.long.ss, reflevel="Car/van driver", tol=0.1)

logit.formula.multi3 <- mFormula(IsChosenAlternative ~ Price + TravelTime | Sex + Minority + IncomeQuintiles + TripPurpose )
logit.model.multi3 <- mlogit(formula=logit.formula.multi2, data=all.data.long.ss, reflevel="Car/van driver", tol=0.1)

## Mixed Logit

logit.formula.mixed <- mFormula(IsChosenAlternative ~ Price + TravelTime )

choice.mlogit <- mlogit(formula=logit.formula.mixed,
                        data=all.data.long.ss,
                        panel=FALSE, 
                        rpar= c(`Bicycle:(intercept)`="t",
                                `Bus:(intercept)`="t", 
                                `Car/van passenger:(intercept)`="t", 
                                `Rail:(intercept)`="t", 
                                `Taxi:(intercept)`="t",
                                `Walk:(intercept)`="t" , 
                                Price="t", 
                                TravelTime="t"
                                #TripPurpose="t"
                                ),
                        R=100,
                        #halton=NA,
                        print.level=1
                        )
