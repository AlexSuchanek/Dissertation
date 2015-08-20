ccr.vector <- vector()

library(npRmpi)
mpi.spawn.Rslaves(nslaves=3)
mpi.bcast.cmd(np.mpi.initialize(), caller.execute=TRUE)
mpi.bcast.cmd(library(data.table), caller.execute=TRUE)
for (i in 1:2) {
all.data.long.ss2 <- data.table(all.data.long.ss)
all.data.long.ss2 <- all.data.long.ss[IsChosenAlternative==1][sample(1:nrow(all.data.long.ss), 2000, replace=FALSE)]
sample.rows <- sample(nrow(all.data.long.ss2), floor(nrow(all.data.long.ss2)*0.75))
all.data.long.train <- all.data.long.ss2[sample.rows,]
all.data.long.test <- all.data.long.ss2[-sample.rows,]

mpi.bcast.Robj2slave(all.data.long.train)
mpi.bcast.cmd(x <- npcdensbw(formula=Alternative~Price+TravelTime+TripPurpose+Sex+DrivingLicense+AgeCategory,
                             data=all.data.long.train), caller.execute=TRUE)  

mpi.bcast.Robj2slave(all.data.long.test)
mpi.bcast.cmd(np.model <- npconmode(bws=x, newdata=all.data.long.test), caller.execute=TRUE)
ccr.vector[i] <- np.model$CCR.overall
}
# mpi.close.Rslaves()
# mpi.bcast.cmd(mpi.quit(), caller.execute=TRUE)
# summary(x)
