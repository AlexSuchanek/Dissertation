#############################################################
## R Code for Dissertation: Mixed Logit Mode Specification ##
#############################################################

all.data.logitsample <- all.data.long.ss[all.data.long.ss$TripID %in% all.data.long.ss2$TripID,]

## Mixed Logit

summary(logit.model.mixed1)

range.coef <- abs(summary(coefficient.frame3$Price)[6]-summary(coefficient.frame3$Price)[1]/1.5)
sd.coefsd <- sd(coefficient.frame3$Price)
b2 <- 0.9*min(range.coef, sd.coefsd)/(nrow(coefficient.frame3)^0.2)


logit.model.mixed1$coefficients[c(7,75)]
plot(density(rnorm(n=100000, mean=logit.model.mixed1$coefficients[7], sd=logit.model.mixed1$coefficients[75])))

lines(density(coefficient.frame3$Price, bw=b2))


# `Bicycle:Sex1`="n",               
# `Bus:Sex1`="n",                    
# `Passenger:Sex1`="n",              
# `Rail:Sex1`="n",                
# `Taxi:Sex1`="n",                   
# `Walk:Sex1`="n",                  
# `Bicycle:Minority1`="n",          
# `Bus:Minority1`="n",               
# `Passenger:Minority1`="n",       
# `Rail:Minority1`="n",             
# `Taxi:Minority1`="n",               
# `Walk:Minority1`="n",             
# `Bicycle:IncomeQuintiles2`="n",  
# `Bus:IncomeQuintiles2`="n",      
# `Passenger:IncomeQuintiles2`="n",  
# `Rail:IncomeQuintiles2`="n",        
# `Taxi:IncomeQuintiles2`="n",      
# `Walk:IncomeQuintiles2`="n",     
# `Bicycle:IncomeQuintiles3`="n",  
# `Bus:IncomeQuintiles3`="n",        
# `Passenger:IncomeQuintiles3`="n",
# `Rail:IncomeQuintiles3`="n",       
# `Taxi:IncomeQuintiles3`="n",     
# `Walk:IncomeQuintiles3`="n",       
# `Bicycle:IncomeQuintiles4`="n",  
# `Bus:IncomeQuintiles4`="n",      
# `Passenger:IncomeQuintiles4`="n",  
# `Rail:IncomeQuintiles4`="n",    
# `Taxi:IncomeQuintiles4`="n",        
# `Walk:IncomeQuintiles4`="n",     
# `Bicycle:IncomeQuintiles5`="n",   
# `Bus:IncomeQuintiles5`="n",      
# `Passenger:IncomeQuintiles5`="n",    
# `Rail:IncomeQuintiles5`="n",       
# `Taxi:IncomeQuintiles5`="n",    
# `Walk:IncomeQuintiles5`="n",     
# `Bicycle:TripPurposeEV`="n",      
# `Bus:TripPurposeEV`="n",         
# `Passenger:TripPurposeEV`="n",     
# `Rail:TripPurposeEV`="n",        
# `Taxi:TripPurposeEV`="n",        
# `Walk:TripPurposeEV`="n",        
# `Bicycle:TripPurposeH`="n",       
# `Bus:TripPurposeH`="n",           
# `Passenger:TripPurposeH`="n",   
# `Rail:TripPurposeH`="n",         
# `Taxi:TripPurposeH`="n",          
# `Walk:TripPurposeH`="n",          
# `Bicycle:TripPurposeSP`="n",    
# `Bus:TripPurposeSP`="n",          
# `Passenger:TripPurposeSP`="n",   
# `Rail:TripPurposeSP`="n",       
# `Taxi:TripPurposeSP`="n",        
# `Walk:TripPurposeSP`="n",       
# #`Car:TravelTime`="n",            
# `Bicycle:TravelTime`="n",     
# `Bus:TravelTime`="n",           
# `Passenger:TravelTime`="n",    
# `Rail:TravelTime`="n",        
# `Taxi:TravelTime`="n",    
# `Walk:TravelTime`="n")

## Predicting with models ## 

CCRvector.MNL1<- vector()
for (i in 1:500) {
    sub.sample.trips <- sample(unique(all.data.long.ss$TripID), length(unique(all.data.long.ss$TripID))*0.75)
    all.data.long.train <- all.data.long.ss[all.data.long.ss$TripID %in% sub.sample.trips,]
    all.data.long.test <- all.data.long.ss[!(all.data.long.ss$TripID %in% sub.sample.trips),]
    logit.formula <- logit.formula.multi1
    logit.model <- mlogit(formula=logit.formula, data=all.data.long.train, reflevel="Car")
    fitted.prob <- predict(logit.model, newdata=all.data.long.test)
    # Actual
    all.data.long.test$IsChosenAlternative <- as.numeric(all.data.long.test$IsChosenAlternative)
    all.data.long.test <- all.data.long.test[all.data.long.test$IsChosenAlternative==1,c(1,8,14)]
    # Predicted
    y <-t(apply(fitted.prob, 1, function(z) {
        1*(z==max(z))
    }))
    names(all.data.long.test)
    ones <- which(y==1, arr.ind=TRUE)
    y[ones] <- colnames(y)[ones[,2]]
    z <-which(y=="0", arr.ind=TRUE)
    y[z] <- NA
    y <- data.table(y)
    y[is.na(Car), Car:=Bicycle]
    y[is.na(Car), Car:=Bus]
    y[is.na(Car), Car:=Passenger]
    y[is.na(Car), Car:=Rail]
    y[is.na(Car), Car:=Taxi]
    y[is.na(Car), Car:=Walk]
    y[,ChosenAlternative:=Car]
    y <- cbind(all.data.long.test, y[,ChosenAlternative])
    y <- data.table(y)
    
    setnames(y, "y[, ChosenAlternative]", "PredictedAlternative")
    y[,PredictedAlternative:=factor(PredictedAlternative, levels=c("Bicycle", "Bus", "Car", "Passenger", "Rail", "Taxi", "Walk"))]
    ConM <-as.matrix(table(y[,.(Alternative, PredictedAlternative)]))
    CCR <- sum(diag(ConM))/sum(ConM)
    CCRvector.MNL1[i] <-CCR
    print(i)
}

CCRvector.MNL3 <- vector()
for (i in 1:500) {
        sub.sample.trips <- sample(unique(all.data.long.ss$TripID), length(unique(all.data.long.ss$TripID))*0.75)
        all.data.long.train <- all.data.long.ss[all.data.long.ss$TripID %in% sub.sample.trips,]
        all.data.long.test <- all.data.long.ss[!(all.data.long.ss$TripID %in% sub.sample.trips),]
        logit.formula <- logit.formula.multi3
        logit.model <- mlogit(formula=logit.formula, data=all.data.long.train, reflevel="Car")
        fitted.prob <- predict(logit.model, newdata=all.data.long.test)
        # Actual
        all.data.long.test$IsChosenAlternative <- as.numeric(all.data.long.test$IsChosenAlternative)
        all.data.long.test <- all.data.long.test[all.data.long.test$IsChosenAlternative==1,c(1,8,14)]
        # Predicted
        y <-t(apply(fitted.prob, 1, function(z) {
            1*(z==max(z))
        }))
        names(all.data.long.test)
        ones <- which(y==1, arr.ind=TRUE)
        y[ones] <- colnames(y)[ones[,2]]
        z <-which(y=="0", arr.ind=TRUE)
        y[z] <- NA
        y <- data.table(y)
        y[is.na(Car), Car:=Bicycle]
        y[is.na(Car), Car:=Bus]
        y[is.na(Car), Car:=Passenger]
        y[is.na(Car), Car:=Rail]
        y[is.na(Car), Car:=Taxi]
        y[is.na(Car), Car:=Walk]
        y[,ChosenAlternative:=Car]
        y <- cbind(all.data.long.test, y[,ChosenAlternative])
        y <- data.table(y)
       
        setnames(y, "y[, ChosenAlternative]", "PredictedAlternative")
        y[,PredictedAlternative:=factor(PredictedAlternative, levels=c("Bicycle", "Bus", "Car", "Passenger", "Rail", "Taxi", "Walk"))]
        ConM <-as.matrix(table(y[,.(Alternative, PredictedAlternative)]))
        CCR <- sum(diag(ConM))/sum(ConM)
        CCRvector.MNL3[i] <-CCR
        print(i)
}


library(data.table)
all.data.long.ss$Price <- -(all.data.long.ss$Price)
CCRvector.Mix2 <- vector()
for (i in 70:500) {
    set.seed(i)
    sub.sample.trips <- sample(unique(all.data.long.ss$TripID), length(unique(all.data.long.ss$TripID))*0.75)
    all.data.long.train <- all.data.long.ss[all.data.long.ss$TripID %in% sub.sample.trips,]
    all.data.long.test <- all.data.long.ss[!(all.data.long.ss$TripID %in% sub.sample.trips),]
    logit.formula <- logit.formula.mixed3
    logit.model <- mlogit(formula=logit.formula,
                          data=all.data.long.train,
                          panel=FALSE, 
                          rpar= c( `Passenger:(intercept)`="n", `Walk:(intercept)`="n", `Rail:TravelTime`="n",
                                   `Walk:TravelTime`="n")
                          ,halton=NA,
                           reflevel = "Car"
                          ,print.level=1)
    fitted.prob <- predict(logit.model, newdata=all.data.long.test)
    fitted.prob[is.na(fitted.prob)] <- 0
    # Actual
    all.data.long.test$IsChosenAlternative <- as.numeric(all.data.long.test$IsChosenAlternative)
    all.data.long.test <- all.data.long.test[all.data.long.test$IsChosenAlternative==1,c(1,8,14)]
    # Predicted
    y <-t(apply(fitted.prob, 1, function(z) {
        1*(z==max(z))
    }))
    ones <- which(y==1, arr.ind=TRUE)
    y[ones] <- colnames(y)[ones[,2]]
    z <-which(y=="0", arr.ind=TRUE)
    y[z] <- NA
    y <- data.table(y)
    y[is.na(Car), Car:=Bicycle]
    y[is.na(Car), Car:=Bus]
    y[is.na(Car), Car:=Passenger]
    y[is.na(Car), Car:=Rail]
    y[is.na(Car), Car:=Taxi]
    y[is.na(Car), Car:=Walk]
    y[,ChosenAlternative:=Car]
    y <- cbind(all.data.long.test, y[,ChosenAlternative])
    y <- data.table(y)
    setnames(y, "y[, ChosenAlternative]", "PredictedAlternative")
    y[,PredictedAlternative:=factor(PredictedAlternative, levels=c("Bicycle", "Bus", "Car", "Passenger", "Rail", "Taxi", "Walk"))]
    ConM <-as.matrix(table(y[,.(Alternative, PredictedAlternative)]))
    CCR <- sum(diag(ConM))/sum(ConM)
    CCRvector.Mix2[i] <-CCR
    print(i)
    rm(all.data.long.test, all.data.long.train, sub.sample.trips, logit.model, fitted.prob, y)
}
CCRvector.Mix2 <- CCRvector.Mix1

boxplot(CCRvector.Mix1)