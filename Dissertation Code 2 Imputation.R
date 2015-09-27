#######################################################################
## R Code for Dissertation: Imputation and Setup for the Logit Model ##
#######################################################################


############# Things to do ##############

# Fix the multicollinear imputation regressions DONE
# Keep the predicted output required to reduce the size of the workspace
# Remove aliased variables to improve predictions - removal won't diminish predictive power

# List of individuals who made no journeys
# individuals.no.journeys <- outersect(unique(individual.household$IndividualID), unique(all.data$IndividualID))

library(clusterSEs)
library(data.table)
library(ggplot2)
library(car)

# i can move all of this stuff over to the base merge file for cleanup purposes
# as it is just renaming variables and turning them into factor class types

# Creating the real choice set

all.data.long[,FeasibleAlternative:=1]
all.data.long[IsChosenAlternative==0 & Alternative=="Bicycle" & BicycleOwner==0, FeasibleAlternative:=0]
all.data.long[IsChosenAlternative==0 & Alternative=="Car" & (DrivingLicense ==0 | NumCarVan==0 | is.na(VehicleID)==TRUE), FeasibleAlternative:=0]

################
## Imputation ##
################

rmse <- function (model) {
        return(sqrt(sum(resid(model)^2)/length(resid(model))))  
}

clusterSENB <- function (mod, dat, cluster) {
    form <- mod$formula
    variables <- all.vars(form)
    clust.name <- all.vars(cluster)
    used.idx <- which(rownames(dat) %in% rownames(mod$model))
    dat <- dat[used.idx, ]
    clust <- as.vector(unlist(dat[[clust.name]]))
    G <- length(unique(clust))
    ind.variables <- names(coefficients(mod))
    cl <- function(dat, fm, cluster) {
        M <- length(unique(cluster))
        N <- length(cluster)
        K <- fm$rank
        dfc <- (M/(M - 1))
        uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                      sum))
        vcovCL <- dfc * sandwich(fm, meat. = crossprod(uj)/N)
        coeftest(fm, vcovCL)
    }
    se.clust <- cl(dat, mod, clust)[ind.variables, 2]
    beta.mod <- coefficients(mod)[ind.variables]
    w <- beta.mod/se.clust
}

## Some PSUIDFactors do not exist for the long data set for each regression

##############
## Bus Time ##  
##############
# 
# length(all.data$TripPurpose)
# length(na.omit(all.data$BusRel))
# length(na.omit(all.data$BusStopWalkDist))
# length(na.omit(all.data$BusDifficulties))
# length(na.omit(all.data$WalkDifficulties))

bus.time.reg <- lm(formula=LogTravelTime~TripPurpose+Month+DayType+DayOfWeek+LogDistance+LogDistance2+BusRel+
                    BusStopWalkDist+BusDifficulties+WalkDifficulties+Sex+JourneyTime+BusFreq+PSUIDFactor, 
                    family=gaussian, data=all.data[Mode=="Bus"])


imputereg1 <- npregbw(formula=LogTravelTime~TripPurpose+Month+DayType+DayOfWeek+LogDistance+LogDistance2+BusRel+
            BusStopWalkDist+BusDifficulties+WalkDifficulties+Sex+JourneyTime+BusFreq+PSUIDFactor, 
        data=all.data[Mode=="Bus"], ckert="epanechnikov")

rmse.bus.time <- rmse(bus.time.reg)

# all.data[Mode=="Bus",PredictedTime:=Time]
# all.data[Mode=="Bus" & is.na(PredictedBusTime)==FALSE,PredictedTime:=exp(PredictedBusTime)*exp(0.5*rmse.bus.time^2)]
# all.data[Mode=="Bus",MPH:=6*Distance/PredictedBusTime]
# all.data[Mode=="Bus" & MPH>60, PredictedBusTime:=6*Distance/60]
# all.data[,MPH:=NULL]

plot(density(exp(fitted.values(bus.time.reg))*exp(0.5*rmse.bus.time^2)))
lines(density(all.data[Mode=="Bus",Time],to=200))

# cor(fitted.values(bus.time.reg), model.frame(bus.time.reg)$LogTravelTime)
# fitted.values.bus.time <- data.frame(exp(fitted.values(bus.time.reg))*exp(0.5*rmse.bus.time^2))

# View(fitted.values.bus.time)
# 
# ctf <- data.frame(exp(fitted.values(bus.time.reg))*exp(0.5*rmse.bus.time^2))
# btf <- data.frame(exp(fitted.values(car.time.reg))*exp(0.5*car.time.rmse^2))
# colnames(ctf) <- "Travel Time (Minutes)"
# colnames(btf) <- "Travel Time (Minutes)"
# 
# library(ggplot2)
# 
# ggplot() + aes(x=`Travel Time (Minutes)`) + labs(title="Predicted Travel Time Distribution") + ylab("Density") + geom_density(data=ctf, linetype=4) + geom_density(data=btf)
# 
# ggplot(data=all.data) + aes(x=Time) + geom_density(data=all.data[Mode=="Bus"]) + geom_density(data=all.data[Mode=="Car"])

# Some of the factors are not present in the top 

all.data.long[PSUIDFactor %in% model.frame(bus.time.reg)$PSUIDFactor & Alternative=="Bus" & IsChosenAlternative==0,
              LogTravelTime:=predict.glm(object=bus.time.reg, newdata=all.data.long[PSUIDFactor %in% model.frame(bus.time.reg)$PSUIDFactor & Alternative=="Bus" & IsChosenAlternative==0,])]

# bus.time.reg2 <- lm(formula=LogTravelTime~TripPurpose+Month+DayType+DayOfWeek+LogDistance+LogDistance2+BusRel+
#                        BusStopWalkDist+BusDifficulties+WalkDifficulties+Sex+JourneyTime+BusFreq+Region, 
#                    family=gaussian, data=all.data[Mode=="Bus"])
# 
# 
# all.data.long[!(PSUIDFactor %in% model.frame(bus.time.reg)$PSUIDFactor) & Alternative=="Bus" & IsChosenAlternative==0,
#               LogTravelTime:=predict.glm(object=bus.time.reg2, newdata=all.data.long[!(PSUIDFactor %in% model.frame(bus.time.reg)$PSUIDFactor) & Alternative=="Bus" & IsChosenAlternative==0,])]
# 
# rm(bus.time.reg2)

# all.data.long[PSUIDFactor %in% model.frame(bus.time.reg)$PSUIDFactor & Alternative=="Bus" & IsChosenAlternative==0,]
# Check for multicollinearity between variables
# library(car)
# vif(bus.time.reg)
# na.omit(all.data[Mode=="Bus", .(LogTravelTime,TripPurpose,Month,DayType,DayOfWeek,LogDistance,JtPO,JtChem,JtGP,JtGroc,BusStopWalkDist,Sex,JourneyTime,BusFreq,BusRel,BusDifficulties,PSUIDFactor,WalkDifficulties, IndividualID)])
# bus.time.cse <- cluster.bs.glm(mod=bus.time.reg, dat=all.data[Mode=="Bus"], cluster=~IndividualID)

###############
## Bus Price ##  
###############

bus.price.reg <- glm(formula=Price~Month+DayOfWeek+BusFreq+JourneyTime+LogDistance+LogDistance2+AgeCategory+WorkStatus+PSUIDFactor, 
                    data=all.data[Mode=="Bus" & Concession==0 & TicketType %in% 0:4],family=gaussian)

# Adding predicted values to the long form matrix

all.data.long[PSUIDFactor %in% model.frame(bus.price.reg)$PSUIDFactor & Alternative=="Bus" & IsChosenAlternative==0,
              Price:=predict.glm(object=bus.price.reg, 
                                       newdata=all.data.long[PSUIDFactor %in% model.frame(bus.price.reg)$PSUIDFactor & Alternative=="Bus" & IsChosenAlternative==0,])]

# Some issue with PSUID factor not appearing in the regression as it has been NAed out, could rectify this by fixing missing values
# unique(all.data.long[PSUIDFactor %in% model.frame(bus.price.reg)$PSUIDFactor, PSUID])
# unique(all.data[ ,PSUID])
# bus.price.cse <- cluster.bs.glm(mod=bus.time.reg,dat=all.data[Mode=="Bus" & TicketType %in% c("Annual Bus", "Weekly Bus", "Monthly Bus", "Annual Bus & Rail", "Weekly Bus & Rail", "Monthly Bus & Rail") & Concession==0], cluster=~IndividualID, report=T)
# summary(na.omit(all.data[Mode=="Bus" & TicketType %in% 0:4 & Concession==0,)]))
# all.data[Mode=="Bus" & TicketType %in% 0:4 & is.na(StageCost)==TRUE] 
# View(all.data[Mode=="Bus" & TicketType %in% 0:4, .(StageCost,Month,DayType,TravDay,BusFreq,BusRel,JourneyTime,LogDistance,LogDistance2,AgeCategory,TicketType,WorkStatus,Concession)])

rmse.bus.price <- rmse(bus.price.reg)

# all.data.bus <- cbind(all.data[Mode=="Bus" & TicketType %in% 0:4 & Concession==0,.(IndividualID, TripID, StageID)][as.numeric(names(bus.price.reg$fitted.values))], fitted.values(bus.price.reg))
# names(all.data.bus)[4] <- "PredictedBusPrice"
# all.data <- merge(all.data, all.data.bus, by=c("IndividualID", "StageID", "TripID"), all.x=TRUE)

#all.data[is.na(PredictedBusPrice)==FALSE, Price:=PredictedBusPrice ]
# all.data[PredictedBusPrice<0,PredictedBusPrice:=0]
# all.data[Mode=="Bus" & Concession=="1", PredictedBusPrice:=0]
# all.data[Mode=="Bus" & as.numeric(TicketType)>=4, PredictedBusPrice:=0]

plot(density(all.data[Mode=="Bus" & Concession==0 & as.numeric(TicketType)<4 & Price>0 ,na.omit(Price)]))
# lines(density(exp(bus.time.reg.fitted), kernel="epanechnikov"))

# bus.price.cse <- cluster.bs.glm(mod=bus.price.reg, dat=all.data[Mode=="Bus"], cluster=~IndividualID, boot.reps=1)
# If price is negative set to zero not many cases less than 1%

# all.data.long[Alternative=="Bus" & IsChosenAlternative==0 & Price<=0,Price:=0]

# bus.price.reg2 <- glm(formula=Price~Month+DayOfWeek+BusFreq+JourneyTime+LogDistance+LogDistance2+AgeCategory+WorkStatus+Region, 
#                      data=all.data[Mode=="Bus" & Concession==0 & TicketType %in% 0:4],family=gaussian)
# 
# all.data.long[!(PSUIDFactor %in% model.frame(bus.price.reg)$PSUIDFactor) & Alternative=="Bus" & IsChosenAlternative==0,
#               Price:=predict.glm(object=bus.price.reg2, 
#                                  newdata=all.data.long[!(PSUIDFactor %in% model.frame(bus.price.reg)$PSUIDFactor) & Alternative=="Bus" & IsChosenAlternative==0,])]
# 
# rm(bus.price.reg2)

###############
## Walk Time ##
###############

walk.time.reg <- glm(formula=LogTravelTime~TripPurpose+LogDistance+AgeCategory*LogDistance
                     +Sex*LogDistance+AgeCategory*LogDistance2+Sex*LogDistance2+WalkDifficulties+PSUIDFactor, family=gaussian, data=all.data[Mode=="Walk"])

walk.time.rmse <-rmse(walk.time.reg)

reg.data.walk <- cbind(all.data[Mode=="Walk",.(IndividualID, TripID, StageID)][as.numeric(names(walk.time.reg$fitted.values))], fitted.values(walk.time.reg))
names(reg.data.walk)[4] <- "PredictedWalkTime"

# all.data <- merge(all.data, reg.data.walk, by=c("IndividualID", "StageID", "TripID"), all.x=TRUE)
# all.data[is.na(PredictedWalkTime)==FALSE,Time:=exp(PredictedWalkTime)*exp(0.5*walk.time.rmse^2)]
# 
# all.data[Mode=="Walk", MPH:=6*Distance/Time]
# all.data[MPH>25, Time:=6*Distance/25]
# all.data[,MPH:=NULL]

plot(density(exp(fitted.values(walk.time.reg))*exp(0.5*walk.time.rmse^2), kernel="epanechnikov", bw=4))
lines(density(all.data[Mode=="Walk",Time],bw=4))

# walk.time.cse <- cluster.bs.glm(mod=walk.time.reg, dat=all.data[Mode=="Walk"], cluster=~IndividualID, report=T, boot.reps=1)

## use the row names provided 
## tomorrow, use lars code to check predicted walk times

all.data.long[PSUIDFactor %in% model.frame(walk.time.reg)$PSUIDFactor & Alternative=="Walk" & IsChosenAlternative==0,
              LogTravelTime:=predict.glm(object=walk.time.reg, 
                                       newdata=all.data.long[PSUIDFactor %in% model.frame(walk.time.reg)$PSUIDFactor &  Alternative=="Walk" & IsChosenAlternative==0,])]


# walk.time.reg2 <- glm(formula=LogTravelTime~TripPurpose+LogDistance+AgeCategory*LogDistance
#                      +Sex*LogDistance+AgeCategory*LogDistance2+Sex*LogDistance2+WalkDifficulties+Region, family=gaussian, data=all.data[Mode=="Walk"])
# 
# all.data.long[!(PSUIDFactor %in% model.frame(walk.time.reg)$PSUIDFactor) & Alternative=="Walk" & IsChosenAlternative==0,
#               LogTravelTime:=predict.glm(object=walk.time.reg2, 
#                                          newdata=all.data.long[!(PSUIDFactor %in% model.frame(walk.time.reg)$PSUIDFactor) &  Alternative=="Walk" & IsChosenAlternative==0,])]
# 
# rm(walk.time.reg2)
################
## Cycle Time ##
################

bicycle.time.reg <- glm(formula=LogTravelTime~TripPurpose+Month+DayOfWeek+JourneyTime+PavementCond+CycleLanes+AgeCategory+Sex
                        +PSUIDFactor+AgeCategory*LogDistance+Sex*LogDistance+AgeCategory*LogDistance2+Sex*LogDistance2
                        , family=gaussian, data=all.data[Mode=="Bicycle",])

rmse.bicycle.time <- rmse(bicycle.time.reg)

# reg.data.walk <- cbind(all.data[Mode=="Bicycle",.(IndividualID, StageID, TripID)][as.numeric(names(bicycle.time.reg$fitted.values))], fitted(bicycle.time.reg))
# names(reg.data.walk)[4] <- "PredictedBicycleTime"
# all.data <- merge(all.data, reg.data.walk, by=c("IndividualID", "StageID", "TripID"), all.x=TRUE)
# all.data[,PredictedBicycleTime:=exp(PredictedBicycleTime)*exp(0.5*rmse.bicycle.time^2)]
# all.data[(6*Distance)/Time>35,PredictedBicycleTimeL:=(6*Distance/35)]

plot(density(all.data[Mode=="Bicycle",Time], to=200))
lines(density(exp(fitted.values(bicycle.time.reg))*exp(0.5*rmse.bicycle.time^2)))

all.data.long[PSUIDFactor %in% model.frame(bicycle.time.reg)$PSUIDFactor & Alternative=="Bicycle" & IsChosenAlternative==0, 
              LogTravelTime:=predict.glm(bicycle.time.reg, newdata=all.data.long[PSUIDFactor %in% model.frame(bicycle.time.reg)$PSUIDFactor & Alternative=="Bicycle" & IsChosenAlternative==0,])]

# bicycle.time.reg2 <- glm(formula=LogTravelTime~TripPurpose+Month+DayOfWeek+JourneyTime+PavementCond+CycleLanes+AgeCategory+Sex
#                         +Region+AgeCategory*LogDistance+Sex*LogDistance+AgeCategory*LogDistance2+Sex*LogDistance2
#                         , family=gaussian, data=all.data[Mode=="Bicycle",])
# 
# 
# all.data.long[!(PSUIDFactor %in% model.frame(bicycle.time.reg)$PSUIDFactor) & Alternative=="Bicycle" & IsChosenAlternative==0, 
#               LogTravelTime:=predict.glm(bicycle.time.reg2, newdata=all.data.long[!(PSUIDFactor %in% model.frame(bicycle.time.reg)$PSUIDFactor) & Alternative=="Bicycle" & IsChosenAlternative==0,])]
# 
# rm(bicycle.time.reg2)

# bicycle.time.cse <- cluster.bs.glm(mod=bicycle.time.reg, dat=all.data[Mode=="Bicycle"], cluster=~IndividualID, report=TRUE)

##################
## Driving time ##
##################

car.time.reg <- lm(formula=LogTravelTime~TripPurpose*LogDistance+Month*LogDistance+DayOfWeek+DayType+JourneyTime*LogDistance+
                      JourneyTime*LogDistance2+DisabledDriver+PSUIDFactor*LogDistance, data=all.data[Mode=="Car"])

car.time.rmse <- rmse(car.time.reg)
reg.car.data <- cbind(all.data[Mode=="Car",.(IndividualID, StageID, TripID)][as.numeric(names(car.time.reg$fitted.values))], fitted(car.time.reg))
names(reg.data.walk)[4] <- "PredictedCarDriverTime"
all.data <- merge(all.data, reg.data.walk, by=c("IndividualID", "StageID", "TripID"), all.x=TRUE)

## This is for cleaning up the imputations

# all.data[Mode=="Car", TimeTest:=Time]
# all.data[is.na(PredictedCarDriverTime)==FALSE,TimeTest:=exp(PredictedCarDriverTime)*exp(0.5*car.time.rmse^2)]
# all.data[6*Distance/Time>70, TimeTest:=6*Distance/70]

plot(density(exp(fitted.values(car.time.reg))*exp(0.5*car.time.rmse^2),bw=2 ,to=100))
lines(density(all.data[Mode=="Car",Time]))

# there are two factors in disabled driver 3 and 4 which mean than the person doesnt drive any more so they will never choose to driver as a mode, also an issue with aliased coefficients

all.data.long[Alternative=="Car" & DisabledDriver %in% c(0,1,2) & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(car.time.reg)$PSUIDFactor , 
              LogTravelTime:=predict.glm(car.time.reg, newdata=all.data.long[Alternative=="Car" & DisabledDriver %in% c(0,1,2) & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(car.time.reg)$PSUIDFactor,])]

# car.time.cse <- cluster.bs.glm(mod=car.time.reg, data=all.data.car, cluster=~IndividualID, report=TRUE)

###################
## Driving Price ##
###################

## Predicted litres/100km

all.data.long[Alternative=="Car" & is.na(VehicleID)==FALSE, FuelConsumption:=predict.lm(size.efficiency.reg, newdata=all.data.long[Alternative=="Car" & is.na(VehicleID)==FALSE,]) ]
all.data.long[Alternative=="Car" & FuelType=="Petrol", Price:=exp(LogDistance)*1.609334/100*FuelConsumption*(PetrolPrice)]
all.data.long[Alternative=="Car" & FuelType=="Diesel", Price:=exp(LogDistance)*1.609334/100*FuelConsumption*(DieselPrice)]

plot(density(all.data.long[FuelType=="Petrol" & Alternative == "Car", Price/100], to=10))
lines(density(all.data.long[FuelType=="Diesel" & Alternative == "Car", Price/100], to=10))
## we know full consumption per l/100km
## we know the cost per litre

# Add to data.long as well.
# 
# fuel.pred <-  all.data[Mode=="Car" & is.na(Price)==FALSE, .(Price, VehicleID, TripID)]
# setnames(fuel.pred, "Price", "JourneyPrice")
# all.data.long <- merge(all.data.long, fuel.pred, by=c("VehicleID", "TripID"), all.x=TRUE)
# all.data.long[Alternative=="Car" & IsChosenAlternative==1, Price:=JourneyPrice]
# all.data.long[,JourneyPrice:=NULL]
# 
# all.data.long[Alternative=="Car" & is.na(VehicleID)==FALSE & IsChosenAlternative==0, Price:=predict.lm(size.efficiency.reg, newdata=all.data.long[Alternative=="Car" & is.na(VehicleID)==FALSE & IsChosenAlternative==0,])]

########################
## Car Passenger Time ##
########################

## can justify bandwidth smoothing due to people recording travel time in a non-continous manner

carp.time.reg <- glm(formula=LogTravelTime~TripPurpose*LogDistance+Month*LogDistance+DayOfWeek+DayType+JourneyTime*LogDistance+
                       JourneyTime*LogDistance2+Region*LogDistance+Region*LogDistance2+PSUIDFactor, dat=all.data[Mode=="Passenger"], family=gaussian)

rmse.carp <- rmse(carp.time.reg)

plot(density(exp(fitted.values(carp.time.reg))*exp(0.5*rmse.carp^2),bw=4, to=200))
lines(density(all.data[Mode=="Passenger" & Time>0,Time], to=200, bw=4))

## Rank deficient 

all.data.long[Alternative=="Passenger" & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(carp.time.reg)$PSUIDFactor, 
              LogTravelTime:=predict.glm(carp.time.reg, newdata=all.data.long[PSUIDFactor %in% model.frame(carp.time.reg)$PSUIDFactor & Alternative=="Passenger" & IsChosenAlternative==0,])]

# carp.time.reg2 <- glm(formula=LogTravelTime~TripPurpose*LogDistance+Month*LogDistance+DayOfWeek+DayType+JourneyTime*LogDistance+
#                          JourneyTime*LogDistance2+Region*LogDistance+Region*LogDistance2+Region, dat=all.data[Mode=="Passenger"], family=gaussian)
# 
# all.data.long[Alternative=="Passenger" & IsChosenAlternative==0 & !(PSUIDFactor %in% model.frame(carp.time.reg)$PSUIDFactor), 
#               LogTravelTime:=predict.glm(carp.time.reg2, newdata=all.data.long[!(PSUIDFactor %in% model.frame(carp.time.reg)$PSUIDFactor) & Alternative=="Passenger" & IsChosenAlternative==0,])]
# 
# rm(carp.time.reg2)

# carp.time.cse <- cluster.bs.glm(mod=carp.time.reg, data=all.data.carp, cluster=~IndividualID, report=TRUE)

######################
## Rail Travel Time ##
######################

rail.time.reg <- glm(formula=LogTravelTime~TripPurpose+Month+DayOfWeek+DayType+JourneyTime*LogDistance+Region*LogDistance
                     +WalkTimeRail+BusTimeRail+RailReliability+PSUIDFactor, data=all.data[Mode=="Rail",], family=gaussian)

rmse.rail.time <- rmse(rail.time.reg)

plot(density(all.data[Mode=="Rail", Time]))
lines(density(exp(fitted.values(rail.time.reg))*exp(0.5*rmse.rail.time^2)))

all.data.long[Alternative=="Rail" & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(rail.time.reg)$PSUIDFactor, 
              LogTravelTime:=predict.glm(rail.time.reg, 
                                         newdata=all.data.long[Alternative=="Rail" 
                                                  & IsChosenAlternative==0 
                                                  & PSUIDFactor %in% model.frame(rail.time.reg)$PSUIDFactor,,])]




# 
# rail.time.reg2 <- glm(formula=LogTravelTime~TripPurpose+Month+DayOfWeek+DayType+JourneyTime*LogDistance+Region*LogDistance
#                      +WalkTimeRail+BusTimeRail+RailReliability+Region, data=all.data[Mode=="Rail",], family=gaussian)
# 
# all.data.long[Alternative=="Rail" & IsChosenAlternative==0 & !(PSUIDFactor %in% model.frame(rail.time.reg)$PSUIDFactor), 
#               LogTravelTime:=predict.glm(rail.time.reg2, 
#                                          newdata=all.data.long[Alternative=="Rail" 
#                                                                & IsChosenAlternative==0 
#                                                                & !(PSUIDFactor %in% model.frame(rail.time.reg)$PSUIDFactor),,])]
# 
# 
# rm(rail.time.reg2)
###############
## Rail Cost ##
###############

rail.price.reg <- glm(formula=Price~Month+DayOfWeek+JourneyTime*LogDistance+JourneyTime*LogDistance2
                      +RailReliability+RailFreq+AgeCategory+PSUIDFactor, 
                      family=gaussian, data=all.data[Mode=="Rail",])

table(all.data.long$RailFreq)

rmse.rail.price <- rmse(rail.price.reg)
# table(all.data$RailFreq)

plot(density(all.data[Mode=="Rail" & is.na(Price)==FALSE, Price]))
lines(density(fitted.values(rail.price.reg)))

all.data.long[Alternative=="Rail" 
              & IsChosenAlternative==0 
              & PSUIDFactor %in% model.frame(rail.price.reg)$PSUIDFactor, 
              Price:=predict.glm(rail.price.reg, 
                                 newdata=all.data.long[Alternative=="Rail" 
                                   & IsChosenAlternative==0 
                                   & PSUIDFactor %in% model.frame(rail.price.reg)$PSUIDFactor,])]

# rail.price.reg2 <- glm(formula=Price~Month+DayOfWeek+JourneyTime*LogDistance+JourneyTime*LogDistance2
#                        +RailReliability+RailFreq+AgeCategory+Region, 
#                        family=gaussian, data=all.data[Mode=="Rail",])
# 
# all.data.long[Alternative=="Rail" 
#               & IsChosenAlternative==0 
#               & !(PSUIDFactor %in% model.frame(rail.price.reg)$PSUIDFactor), 
#               Price:=predict.glm(rail.price.reg2, 
#                                  newdata=all.data.long[Alternative=="Rail" 
#                                                        & IsChosenAlternative==0 
#                                                        & !(PSUIDFactor %in% model.frame(rail.price.reg)$PSUIDFactor),])]

######################
## Taxi Travel Time ##
######################

taxi.time.reg <- glm(formula=LogTravelTime~TripPurpose*LogDistance+Month*LogDistance+DayOfWeek+JourneyTime*LogDistance
                     +JourneyTime*LogDistance2+Region*LogDistance+Region*LogDistance2+PSUIDFactor, data=all.data[Mode=="Taxi",], family=gaussian)


rmse.tax.time <- rmse(taxi.time.reg)

plot(density(exp(fitted.values(taxi.time.reg))*exp(0.5*rmse.tax.time^2)))
lines(density(all.data[Mode=="Taxi", Time]))

all.data.long[Alternative=="Taxi" & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(taxi.time.reg)$PSUIDFactor, 
              LogTravelTime:=predict.glm(taxi.time.reg, 
                newdata=all.data.long[Alternative=="Taxi" & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(taxi.time.reg)$PSUIDFactor ,])]

################
## Taxi Price ##
################

taxi.price.reg <- glm(formula=Price~Month+DayOfWeek+JourneyTime*LogDistance+
                          JourneyTime*LogDistance2+Region*LogDistance+Region*LogDistance2+PSUIDFactor,
                      data=all.data[Mode=="Taxi" & Price>0,])

rmse.taxi.price <- rmse(taxi.price.reg)

plot(density(fitted.values(taxi.price.reg)))
lines(density(all.data[Mode=="Taxi" & Price>0, Price]))

all.data.long[Alternative=="Taxi" & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(taxi.price.reg)$PSUIDFactor,   
              Price:=predict.glm(taxi.price.reg, 
              newdata=all.data.long[Alternative=="Taxi" & IsChosenAlternative==0 & PSUIDFactor %in% model.frame(taxi.price.reg)$PSUIDFactor,])]

## Set the price of these alternatives to zero

all.data.long[Alternative %in% c("Walk", "Bicycle", "Passenger"), Price:=0]

## Keep the feasible set of alternatives 

all.data.long <- all.data.long[FeasibleAlternative==1]

## Keep the residuals for densiuty plots

bus.price.reg.fitted <- fitted.values(bus.time.reg)
bus.time.reg.fitted <- fitted.values(bus.time.reg)
walk.time.reg.fitted <- fitted.values(walk.time.reg)
bicycle.time.reg.fitted <- fitted.values(bicycle.time.reg)
car.time.reg.fitted <- fitted.values(car.time.reg)
carp.time.reg.fitted <- fitted.values(carp.time.reg)
rail.price.reg.fitted <- fitted.values(rail.price.reg)
rail.time.reg.fitted <- fitted.values(rail.time.reg)
taxi.price.reg.fitted <- fitted.values(taxi.price.reg)
taxi.time.reg.fitted <- fitted.values(taxi.time.reg)

## Clear up the workspace before saving a new image ##

bus.price.reg.frame <- model.frame(bus.time.reg)
bus.time.reg.frame <- model.frame(bus.time.reg)
walk.time.reg.frame <- model.frame(walk.time.reg)
bicycle.time.reg.frame <- model.frame(bicycle.time.reg)
car.time.reg.frame <- model.frame(car.time.reg)
carp.time.reg.frame <- model.frame(carp.time.reg)
rail.price.reg.frame <- model.frame(rail.price.reg)
rail.time.reg.frame <- model.frame(rail.time.reg)
taxi.price.reg.frame <- model.frame(taxi.price.reg)
taxi.time.reg.frame <- model.frame(taxi.time.reg)

## Clean up the work space

rm(carp.time.reg, 
   car.time.reg, 
   bus.price.reg, 
   bus.time.reg, 
   rail.price.reg, 
   rail.time.reg, 
   taxi.price.reg, 
   taxi.time.reg, 
   walk.time.reg, 
   bicycle.time.reg)

rm(reg.car.data, reg.data.walk)

## Saving a copy of the workspace image ##
## 

## could possibly move this all to the .Rnw file


# all.data[PSUID==2012000002 & IndividualID==2012000015 & HouseholdID==2012000007 ,.(Mode,Price, TicketType, TicketTripCost)]
# vehicle.data[HouseholdID==2012000007,]


# rm(a, all.data.bus, diesel.data, diesel.data.melt, modes, petrol.data, petrol.data.melt, car.time.reg, carp.time.reg, bus.time.reg, bus.price.reg)
# rm(reg.car.data, reg.data.walk)
# rm(fuel.pred, regions.merge, vehicle.data, vehicle.efficiency.data)
# rm(walk.time.reg)

# Convert car driving price from pence to pounds to get it in line with the other prices

all.data.long[Price<=0, Price:=0]
all.data.long[Alternative=="Car", Price:=Price/100]
all.data.long[Alternative=="Car", Price:=Price + exp(LogDistance)*VehFixedCosts]
# all.data.long <- all.data.long[TripID %in% all.data.long[IsChosenAlternative==1,TripID],]

# Test of replacing with means brekaing down on category - this is the best for the moment until the replace PSUID code works
# 
# all.data.long[is.na(LogTravelTime)==FALSE, LTTDecile:=findInterval(LogTravelTime, quantile(LogTravelTime, probs=seq(0.1,1,0.1)))]
# all.data.long[is.na(Price)==FALSE, PriceDecile:=findInterval(Price, quantile(Price, probs=seq(0.1,1,0.1)))]
# all.data.long[,MeanTravelTime:=mean(LogTravelTime, na.rm=TRUE), by=.(Alternative, Region, TripPurpose, AgeCategory, DayOfWeek)]
# all.data.long[is.na(LogTravelTime)==TRUE, LogTravelTime:=MeanTravelTime]
# all.data.long[,MeanPrice:=mean(Price), by=.(Alternative, Region, TripPurpose, AgeCategory, DayOfWeek)]
# all.data.long[is.na(Price)==TRUE, Price:=MeanPrice]

#### Creating the sample #### 

# this removes all trip IDs that do not have price and travel time data for each alternative.

incomplete.sets <- all.data.long[is.na(Price)==TRUE | is.na(LogTravelTime)==TRUE, length(is.na(LogTravelTime)==TRUE), by=TripID]
all.data.long <- all.data.long[!(TripID %in% incomplete.sets$TripID)]


# all.data.long.ss <- unique(all.data.long[!(TripID %in% all.data.long.cc$TripID), TripID])
# rm(all.data.long.cc)


# Taking a subsample of the complete set to train the model

# Removing unneccesary columns

## This converts the dataset into the format required

## length(unique(all.data.longss$TripID))

all.data.long[,IsChosenAlternative:=as.logical(IsChosenAlternative)]
all.data.long[,TravelTime:=exp(LogTravelTime)]
all.data.long[,Distance:=exp(LogDistance)]

setnames(all.data.long, "Social class of individuals", "SocialClass")
setnames(all.data.long, "Household Income Quintiles - Diary Sample 2012", "IncomeQuintiles")
setnames(all.data.long, "Mobility difficulties summary", "Mobility")

all.data.long[,SocialClass:=factor(SocialClass)]
all.data.long[,IncomeQuintiles:=factor(IncomeQuintiles)]
all.data.long[,Mobility:=factor(Mobility)]
all.data.long[,Alternative:=factor(Alternative)]

# Select 5000 Individuals
all.data.long.ss <- all.data.long[!(AgeCategory %in% c("5-10", "11-16", "Pensioner")) & Region %in% c(12,13),.(IndividualID, TripID, IncomeQuintiles, TripPurpose, AgeCategory, Sex, TravelTime, Price, IsChosenAlternative, DrivingLicense, BicycleOwner, Minority, HHoldNumPeople, Alternative, Region)] 


all.data.long.ss[,IID:=c(1:length(unique(IndividualID))), by=IndividualID]

# all.data.long.ss <- data.table(all.data.long.ss)
# sampleindividuals <- sample(unique(all.data.long.ss$IndividualID), size=432)
# all.data.long.ss <- all.data.long.ss[IndividualID %in% sampleindividuals,]

# sampletrips <- sample(unique(all.data.long.ss$TripID), size=5000)
# all.data.long.ss <- all.data.long.ss[TripID %in% sampletrips,]
# rm(sampleindividuals, sampletrips)

library(mlogit)

all.data.long.ss <- mlogit.data(data=all.data.long.ss, 
                                choice="IsChosenAlternative", 
                                shape="long", 
                                alt.var = "Alternative",
                                #alt.levels=c("Bus", "Walk", "Bicyle", "Car", "Passenger", "Rail", "Taxi"), 
                                id.var = "IndividualID",
                                chid.var="TripID")


all.data.long.ss2 <- mlogit.data(data=all.data.long.ss, 
                                 choice="IsChosenAlternative", 
                                 shape="long", 
                                 alt.var = "Alternative",
                                 #alt.levels=c("Bus", "Walk", "Bicyle", "Car", "Passenger", "Rail", "Taxi"), 
                                 id.var = "IndividualID",
                                 chid.var="TripID")

all.data.long.ss$IndividualID <- as.numeric(all.data.long.ss$IndividualID)

save.image("Data.Rda")




