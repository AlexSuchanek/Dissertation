setwd("~/Dropbox/Economics Masters/Dissertation")
load(paste0(getwd(),"/Data.Rda", collapse=NULL))

# View(head(model.matrix(logit.formula, sample.data)))


mpi.spawn.Rslaves(nslaves=3)
mpi.bcast.cmd(np.mpi.initialize(), caller.execute=TRUE)
mpi.bcast.Robj2slave(all.data.long.train)
mpi.bcast.Robj2slave(all.data.long.test)   


for (i in 1:2) {

        mpi.bcast.Robj2slave(all.data.long.train)
        mpi.bcast.Robj2slave(all.data.long.test)   
        mpi.bcast.cmd(bw <- npcdensbw(xdat=all.data.long.train[,.(Price, TravelTime, TripPurpose, Sex, DrivingLicense, AgeCategory)],
                                      ydat=all.data.long.train[, Alternative],
                                      cxkertype="epanechnikov"), caller.execute=TRUE)    
        x <-npconmode(bws=bw,
   
        CCR.values[i] <- x$CCR.overall
        }

library(npRmpi)
