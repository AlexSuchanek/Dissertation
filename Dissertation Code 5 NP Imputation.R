
## Bus Travel Time

bus.time.np.reg.bw  <- npregbw(formula=LogTravelTime~TripPurpose+Month+DayType+DayOfWeek+LogDistance+LogDistance2+BusRel+
                               BusStopWalkDist+BusDifficulties+WalkDifficulties+Sex+JourneyTime+BusFreq+PSUIDFactor, 
                               data=all.data[Mode=="Bus"], ckertype="epanechnikov")

bus.time.np.reg.insample <-  npreg(bws=bus.time.np.reg.bw)

bus.time.np.reg.outofsample <- npreg(bws=bus.time.np.reg.bw, newdata=all.data.long[Alternative=="Bus" & IsChosenAlternative==0], gradients = TRUE)

## Bus Price

bus.price.np.reg.bw <- npregbw(formula=Price~Month+DayOfWeek+BusFreq+JourneyTime+LogDistance+LogDistance2+AgeCategory+WorkStatus+PSUIDFactor, 
                     data=all.data[Mode=="Bus" & Concession==0 & TicketType %in% 0:4], ckertype="epanechnikov")

bus.price.np.reg.insample <-  npreg(bws=bus.price.np.reg.bw)

bus.price.np.reg.outofsample <- npreg(bws=bus.price.np.reg.bw, newdata=all.data.long[Alternative=="Bus" & IsChosenAlternative==0], gradients = TRUE)

## Walk Travel Time

walk.time.np.reg.bw <- npregbw(formula=LogTravelTime~TripPurpose+LogDistance+ AgeCategory+ LogDistance2+Sex+WalkDifficulties+PSUIDFactor, data=all.data[Mode=="Walk"], ckertype="epanechnikov")

walk.time.np.reg.insample <-  npreg(bws=walk.time.np.reg.bw)

walk.time.np.reg.outofsample <- npreg(bws=walk.time.np.reg.bw, newdata=all.data.long[Alternative=="Walk" & IsChosenAlternative==0], gradients = TRUE)

## Cycle Time

bicycle.time.np.reg.bw <- npregbw(formula=LogTravelTime~TripPurpose+Month+DayOfWeek+JourneyTime+PavementCond+CycleLanes+AgeCategory+Sex
                        +PSUIDFactor+AgeCategory+LogDistance+Sex+LogDistance2, data=all.data[Mode=="Bicycle",], ckertype="epanechnikov")

bicycle.time.np.reg.insample <-  npreg(bws=bicycle.time.np.reg.bw)

bicycle.time.np.reg.outofsample <- npreg(bws=bicycle.time.np.reg.bw, newdata=all.data.long[Alternative=="Bicycle" & IsChosenAlternative==0], gradients = TRUE)

## Driving Time

car.time.np.reg.bw <- npregbw(formula=LogTravelTime~TripPurpose+Month+LogDistance+DayOfWeek+DayType+JourneyTime+LogDistance2+DisabledDriver+PSUIDFactor, data=all.data[Mode=="Car"], ckertype="epanechnikov")

car.time.np.reg.insample <- npreg(bws=car.time.np.reg.bw)

car.time.np.reg.outofsample <- npreg(bws=car.time.np.reg.bw, newdata=all.data.long[Alternative=="Car" & IsChosenAlternative==0], gradients = TRUE)

## Passenger Time

carp.time.np.reg.bw <- npregbw(formula=LogTravelTime~TripPurpose+LogDistance+Month+DayOfWeek+DayType+JourneyTime+
                            LogDistance2+Region+PSUIDFactor, dat=all.data[Mode=="Passenger"], ckertype="epanechnikov")

carp.time.np.reg.insample <- npreg(bws=carp.time.np.reg.bw)

carp.time.np.reg.outofsample <- npreg(bws=carp.time.np.reg.bw, newdata=all.data.long[Alternative=="Passenger" & IsChosenAlternative==0], gradients = TRUE)


## Rail Time 

rail.time.np.reg.bw <- npregbw(formula=LogTravelTime~TripPurpose+Month+DayOfWeek+DayType+JourneyTime+LogDistance+Region
                     +WalkTimeRail+BusTimeRail+RailReliability+PSUIDFactor, data=all.data[Mode=="Rail",], ckertype="epanechnikov")

rail.time.np.reg.insample <- npreg(bws=rail.time.np.reg.bw)

rail.time.np.reg.outofsample<- npreg(bws=rail.time.np.reg.bw, newdata=all.data.long[Alternative=="Rail" & IsChosenAlternative==0], gradients = TRUE)

## Rail Price

rail.price.np.reg.bw <- npregbw(formula=Price~Month+DayOfWeek+JourneyTime+LogDistance+LogDistance2
                      +RailReliability+RailFreq+AgeCategory+PSUIDFactor, data=all.data[Mode=="Rail",], ckertype="epanechnikov")

rail.price.np.reg.insample <- npreg(bws=rail.price.np.reg.bw)

rail.price.np.reg.outofsample <- npreg(bws=rail.price.np.reg.bw, newdata=all.data.long[Alternative=="Rail" & IsChosenAlternative==0], gradients = TRUE)

## Taxi Travel Time

taxi.time.np.reg.bw <- npregbw(formula=LogTravelTime~TripPurpose+LogDistance+Month+DayOfWeek+JourneyTime+LogDistance2+PSUIDFactor, data=all.data[Mode=="Taxi",], ckertype="epanechnikov")

taxi.time.np.reg.insample <- npreg(bws=taxi.time.np.reg.bw)

taxi.time.np.reg.outofsample <- npreg(bws=taxi.time.np.reg.bw, newdata=all.data.long[Alternative=="Taxi" & IsChosenAlternative==0], gradients = TRUE)

## Taxi Price 

taxi.price.np.reg.bw <- npregbw(formula=Price~Month+DayOfWeek+JourneyTime+LogDistance+LogDistance2+PSUIDFactor, data=all.data[Mode=="Taxi",],  ckertype="epanechnikov" )

taxi.price.np.reg.insample <- npreg(bws=taxi.price.np.reg.bw)

taxi.price.np.reg.outofsample <- npreg(bws=taxi.price.np.reg.bw, newdata=all.data.long[Alternative=="Taxi" & IsChosenAlternative==0], gradients = TRUE)

plot(density(taxi.price.np.reg.outofsample$mean))
lines(density(all.data.long[Alternative=="Taxi" & IsChosenAlternative==0, Price]))

