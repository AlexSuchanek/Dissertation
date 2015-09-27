load(paste0(getwd(),"/Data.Rda", collapse=NULL))
cr.vector.1 <- vector()
ccr.vector.4 <- vector()
library(npRmpi)
library(data.table)
mpi.spawn.Rslaves(nslaves=3)
mpi.bcast.cmd(np.mpi.initialize(), caller.execute=TRUE)
all.data.long.ss2 <- all.data.long.ss
all.data.long.ss2 <- data.table(all.data.long.ss2)
all.data.long.ss2 <- all.data.long.ss2[IsChosenAlternative==1]
#all.data.long.ss$Price <- -(all.data.long.ss$Price)
for (i in 1:500) {
    sub.sample.rows <- sample(nrow(all.data.long.ss2), floor(nrow(all.data.long.ss2)*0.75))
    all.data.long.train <- all.data.long.ss2[sub.sample.rows,]
    all.data.long.test <- all.data.long.ss2[!(sub.sample.rows),]
    mpi.bcast.Robj2slave(all.data.long.train)
    mpi.bcast.cmd(library(data.table), caller.execute=TRUE)
    mpi.bcast.cmd(np.bandwidth <- npcdensbw(
        xdat=all.data.long.train[,.(Price, TravelTime, TripPurpose, Sex, Minority, IncomeQuintiles)],
        ydat=all.data.long.train[,Alternative],
        cxkertype="epanechnikov", 
        bwscaling=TRUE), caller.execute=TRUE)
    mpi.bcast.Robj2slave(all.data.long.test)
    mpi.bcast.Robj2slave(all.data.long.train)
    mpi.bcast.cmd(np.model <- npconmode(
        bws=np.bandwidth,    
        #             txdat=all.data.long.train[,.(Price, TravelTime, TripPurpose, Sex, Minority, IncomeQuintiles)],
        #             tydat=all.data.long.train[,Alternative],
        txdat=all.data.long.test[,.(Price, TravelTime, TripPurpose, Sex, Minority, IncomeQuintiles)],
        tydat=all.data.long.test[,Alternative]), caller.execute=TRUE)
    ccr.vector.4[i] <- np.model$CCR.overall
    rm(sub.sample.rows)
    print(i)
}

ccr.vector.3 <- c(ccr.vector.3, ccr.vector.4)

class(all.data.long.ss$IncomeQuintiles)

## 
library(npRmpi)
mpi.spawn.Rslaves(nslaves=3)
mpi.bcast.cmd(np.mpi.initialize(), caller.execute=TRUE)
all.data.long.ss$IncomeQuintiles <- factor(all.data.long.ss$IncomeQuintiles, ordered=TRUE)
all.data.long.np <- all.data.long.ss[all.data.long.ss$IsChosenAlternative==1,]
all.data.long.np <- data.table(all.data.long.np)
mpi.bcast.Robj2slave(all.data.long.np)
mpi.bcast.cmd(library(data.table), caller.execute=TRUE)
mpi.bcast.cmd(np.bandwidth.insample <- npcdensbw(formula=Alternative~Price+TravelTime+TripPurpose+Sex+Minority+IncomeQuintiles,
    data=all.data.long.np,
    cxkertype="epanechnikov", 
    bwscaling=TRUE), caller.execute=TRUE)
mpi.bcast.cmd(np.model.insample <- npcdens(
    bws=np.bandwidth.insample,    
    data=all.data.long.np), caller.execute=TRUE)
mpi.bcast.cmd(np.model.insample.con <- npconmode(
    bws=np.bandwidth.insample,    
    data=all.data.long.np), caller.execute=TRUE)

np.model.insample$condens
all.data.long.ss$IncomeQuintiles <- factor(all.data.long.ss$IncomeQuintiles)   
np.model$CCR.overall
######## Empirical Mixing Distribution MNL 1 ########

logit.formula.multi3 <- mFormula(IsChosenAlternative ~ Price | TripPurpose + Sex + Minority + IncomeQuintiles | TravelTime )
logit.model.multi3 <- mlogit(formula=logit.formula.multi3, data=all.data.long.ss, reflevel="Car")

######## Empirical Mixing Distribution MNL 2########

coefficient.names <- colnames(t(data.frame(logit.model.multi3$coefficients)))
coefficient.length <- length(coefficient.names)
coefficient.frame <- as.data.frame(setNames(replicate(coefficient.length, numeric(0)), coefficient.names))
coefficient.frame <- data.table(coefficient.frame)
for (i in unique(all.data.long.ss$IndividualID)) {
    logit.empirical <- mlogit(formula=logit.formula.multi3, data=all.data.long.ss[with(all.data.long.ss, IndividualID!=i),], reflevel="Car")   
    coefficient.frame2 <- rbind(coefficient.frame2, data.frame(t(logit.empirical$coefficients[1:length(logit.model.multi3$coefficients)])))
    print(i)
}



######## Empirical Mixing Distribution MNL 2########
library(data.table)
library(utils)
library(plyr)
coefficient.names3 <- colnames(t(data.frame(logit.model.multi3$coefficients)))
coefficient.length3 <- length(coefficient.names3)
coefficient.frame3 <- as.data.frame(setNames(replicate(coefficient.length3, numeric(0)), coefficient.names3))
coefficient.frame3 <- data.table(coefficient.frame3)

# log.socket <- make.socket(host="localhost", port=4000, server=FALSE)
# 
# Log <- function(text, ...) {
#     msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
#     cat(msg)
#     write.socket(log.socket, msg)
# }
# Log("Processing block %d of %d", i, n.blocks)

# library(foreach)
# library(doMC)
# library(mlogit)
# registerDoMC(4)
pb <- txtProgressBar(min = min(all.data.long.ss$TripID), max = max(all.data.long.ss$TripID), style = 3)
for (i in unique(all.data.long.ss$TripID))  {
    logit.empirical <- mlogit(formula=logit.formula.multi3, data=all.data.long.ss[with(all.data.long.ss, TripID!=i),], reflevel="Car")   
    coefficient.frame3 <- rbind(coefficient.frame3, data.frame(t(logit.empirical$coefficients[1:length(logit.model.multi3$coefficients)])), fill=TRUE)
    setTxtProgressBar(pb, i)
}
plot(density(coefficient.frame3$Bicycle..intercept., from=-2.9, to = -2.85, bw=bw3))
average.coef <- as.vector(logit.model.multi3$coefficients[1:68])
coefficient.frame3 <- data.frame(t(apply(coefficient.frame3, 1, function(z)  z - average.coef)))


bw3 <- 0.9*min(sd(coefficient.frame3$Bicycle..intercept.), abs(max(coefficient.frame3$Bicycle..intercept.) - min(coefficient.frame3$Bicycle..intercept.))/1.5)/(length(coefficient.frame3$Bicycle..intercept.)^0.2)

#ccr.vector.1 <- c(ccr.vector.1, ccr.vector.2)


save.image("Data.Rda")
