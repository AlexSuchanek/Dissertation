#############################################################
## R Code for Dissertation: Mixed Logit Mode Specification ##
#############################################################

## Multinomial Logit 1

library(mlogit)
library(data.table)
logit.formula.multi1 <- mFormula(IsChosenAlternative ~  Price + TravelTime| TripPurpose)

logit.model.multi1 <- mlogit(formula=logit.formula.multi1, data=all.data.long.ss, reflevel="Car/van driver")

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


logit.formula.multi2 <- mFormula(IsChosenAlternative ~ Price + TravelTime | Sex )

logit.model.multi2 <- mlogit(formula=logit.formula.multi2, data=all.data.long.ss)

summary(logit.model.multi2)
View(model.matrix(logit.formula.multi2, all.data.long.ss))

lrtest(logit.model.multi1,logit.model.multi1)

logit.model.multi1$probabilities

## Mixed Logit

logit.formula.mixed <- mFormula(IsChosenAlternative ~ Price + TravelTime | Sex)

choice.mlogit <- mlogit(formula=logit.formula.mixed,
                        data=all.data.long.ss,
                        panel=FALSE, 
                        rpar= c(`Bus:(intercept)`="t",  Price="t", TravelTime="t"),
                        R=100,
                        #halton=NA,
                        print.level=1
                        )

table(sample.data$IncomeQuintiles, sample.data$TripPurpose)

summary(sample.data$Price)

plot(sample.data$TravelTime, sample.data$Price)

a <- choice.mlogit$probabilities

summary(choice.mlogit)

a <- rpar(choice.mlogit)

# replace price and logtravel time with averages if data not  there

# View(head(model.matrix(logit.formula, sample.data)))

library(npRmpi)

mpi.spawn.Rslaves(nslaves=3)
mpi.bcast.cmd(np.mpi.initialize(), caller.execute=TRUE)
mpi.bcast.Robj2slave(all.data.long.ss)

## issues with missing values

mpi.bcast.cmd(bw <- npconmode(Alternative ~ Price + TravelTime, data=all.data.long.ss), caller.execute=TRUE)
