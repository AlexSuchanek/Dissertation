#########################################################
## R Code for Dissertation - Data cleaning and Merging ##
#########################################################

## join with psu data as it also includes breakdown between metropolitan and non-metropolitan

rm(list=ls()) ## Clearing Memory

require(reshape2)
require(np)
require(xtable)
require(readxl)
require(data.table)
require(Hmisc)
library(data.table)
library(readxl)
library(stringr)
library(devtools)
library(Hmisc)

#install.packages("devtools")
#install_github("Rdatatable/data.table", build_vignettes=FALSE)
# setwd("C:/Users/Alexander Suchanek/Dropbox/Economics Masters/Dissertation")

#################################
## Setting up global functions ##
#################################

# Function to return all sheets from an excel file

read_excel_allsheets <- function(filename) {
        sheets <- readxl::excel_sheets(filename)
        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet=X))
        names(x) <- sheets
        x
}

lookups.list <- read_excel_allsheets("nts_lookup_tables_banded_variables_ukds.xls")

# Combine all of the lookups names and descriptions into one data frame

lookups.list.names <- data.frame()
for (i in 1:length(names(lookups.list))) {
        lookups.list.names <- rbind(lookups.list.names, data.frame(na.omit(lookups.list[[i]][,c(1,2)])))
}
  
rownames(lookups.list.names) <- 1:nrow(lookups.list.names) ## Reindexing the rows

# Codes Values

lookups.list.values <- data.frame()
for (i in 1:length(names(lookups.list))) {
        lookups.list.values <- rbind(lookups.list.values, data.frame(lookups.list[[i]][, c(1,2,3,4)]))
        
}

# Replacing the NAs with the column IDs

for (i in 1:nrow(lookups.list.values)) {
        lookups.list.values[i,1][is.na(lookups.list.values[i, 1])] <- lookups.list.values[i-1, 1]
        lookups.list.values[i,2][is.na(lookups.list.values[i, 2])] <- lookups.list.values[i-1, 2]   
}

# Adding codes instead of IDs - This runs through the table and replaces the data with code descriptors

replace.descriptors <- function(df) {
        for (i in 1:ncol(df)) {
                a <- colnames(df)[i] == lookups.list.values$Description
                a <- lookups.list.values[which(a, arr.ind=TRUE),]
                if (nrow(a) != 0) { ## skiping the columns that can't be replaced
                        rownames(a) <- 1:nrow(a)
                        for (j in 1:nrow(a)) {
                                df[df[[i]]== a[j,2],i] <- a[j, "Values.Description"]    
                        }
                } 
        }
        return(df)       
}

# household.data <- replace.descriptors(household.data)
# individual.data <- replace.descriptors(individual.data)
# trip.data <- replace.descriptors(trip.data)
# stage.data < replace.descriptors(stage.data)
# day.data <- replace.descriptors(day.data)

remove.columns <- function(dataframe) {
        uniquelength <- sapply(dataframe, function(x) length(unique(x)))
        dataframe <- dataframe[, uniquelength>1]
}

# Function to replace column names with descriptions where possible - 100% matching is not possible
# due to column name differences - this will have to change as it is more efficient to generate dummy 
# variables based on numerics rather than strings.

replace.colnames <- function (df) {
        cr <- data.frame(names(df), stringsAsFactors=FALSE)
        names(cr) <- names(lookups.list.names)[1]
        library(plyr)
        r <- join(x=cr,y=lookups.list.names,  by="Variable.name", type="left")
        r$Description[is.na(r$Description)] <- as.character(r$Variable.name[is.na(r$Description)])
        return(names(df) <- r$Description)
}

########################################################
## Merging the household and individual data together ##
########################################################

psu.data <- read.table("psu.tab", header=TRUE, sep="\t", colClasses="numeric")
names(psu.data) <- replace.colnames(psu.data)
psu.data <- data.table(psu.data)
psu.data <- psu.data[SurveyYear==2012]
psu.data <- psu.data[,c(2,3,6):=NULL, with=FALSE]

individual.data <- read.table("individual.tab", header=TRUE, sep="\t", colClasses="numeric")
#individual.data.h <- read.table("individual.tab", header=FALSE, stringsAsFactors=FALSE, sep="\t", nrows=1)
#names(individual.data) <- unlist(individual.data.h)
names(individual.data) <- replace.colnames(individual.data)
names(individual.data) <- str_trim(names(individual.data), side="both")
individual.data <- individual.data[individual.data$SurveyYear==2012,]

individual.data <- remove.columns(individual.data)
individual.data <- data.table(individual.data, key=c("PSUID", "HouseholdID", "IndividualID"))
setorder(individual.data, PSUID, HouseholdID, IndividualID)
individual.data[,IID:=1:nrow(individual.data)]

# Removing the multi-coded columns that do not provide any useful information 

individual.data[, 98:415:=NULL, with=FALSE]

#rm(individual.data.h)

household.data <- read.table("household.tab", header=TRUE, sep="\t")
household.data <- household.data[household.data$SurveyYear==2012,]  ## Subsetting the data to get the most recent year
colnames(household.data) <- replace.colnames(household.data)
names(household.data) <- str_trim(names(household.data), side="both")

##household.data <- remove.columns(household.data) ##2012 removes some columns that were not answered

household.data <- data.table(household.data, key=c("PSUID", "HouseholdID"))
setorder(household.data, PSUID, HouseholdID)

# Removing uneccesary columns

household.data[,c(91:106, 119:142, 163:253):=NULL, with=FALSE]
household.data[,HHID:=1:nrow(household.data)]
household.data[,c("W1","W2", "W3", "How would you have bought these goods otherwise"):=NULL]

# Merge individual and household data

individual.household.data <- merge(individual.data, household.data, by=c("PSUID","HouseholdID"))

rm(individual.data, household.data)

# Merge with day data as well - later on with the stage data

day.data <- read.table("day.tab", header=TRUE, sep="\t")
day.data <- day.data[day.data$SurveyYear==2012,]  ## Subsetting the data to get the most recent year
colnames(day.data) <- replace.colnames(day.data)
names(day.data) <- str_trim(names(day.data), side="both")
day.data <- remove.columns(day.data)
day.data <- data.table(day.data, key=c("PSUID", "HouseholdID", "IndividualID", "PersNo"))
day.data[,c("Day of week trip took place - weekday, Saturday and Sunday split",
            "Day of week trip took place - weekday and weekend split",
            "TravelYear"):=NULL] # Removing uneccesary columns

# Note TravDay is different to TravelWeekDay_B01ID 
# Merge the trip and day data together
# Drop All Children and Pensioners and reshape age variable

# This is a test so far, just look at adults, so less distortions for journey types prices ect

individual.household.data <- individual.household.data[`Age of person - banded age - Band D - All ages - 9 categories`!=1]
individual.household.data[`Age of person - banded age - Band D - All ages - 9 categories`==2, AgeCategory:=1]
individual.household.data[`Age of person - banded age - Band D - All ages - 9 categories`==3, AgeCategory:=2]
individual.household.data[`Age of person - banded age - Band D - All ages - 9 categories`%in% 4:8,AgeCategory:=3]
individual.household.data[`Age of person - banded age - Band D - All ages - 9 categories`==9, AgeCategory:=4]
age.labels <- c("5-10", "11-16", "Adult", "Pensioner")
age.levels <- c(1,2,3,4)
individual.household.data[,AgeCategory:=factor(x=AgeCategory, levels=age.levels, labels=age.labels, ordered=FALSE)]
rm(age.levels, age.labels)

# individual.household.data <- individual.household.data[AgeCategory=="Adult",]

# Merge with PSU data and drop London + Scotland

individual.household.data <- merge(individual.household.data, psu.data, by="PSUID")
individual.household.data <- individual.household.data[!(`Statistical Region - regional / Metropolitan area breakdown` %in% c(8,15))]

month.list <- lookups.list.values[lookups.list.values$Variable.name=="TWEMonth_B01ID",3:4]
colnames(month.list) <- c("Travel Week End - Month - coded month", "TravelWeekEndMonthName")
month.list <- data.table(month.list)
individual.household.data <- merge(individual.household.data, month.list, by="Travel Week End - Month - coded month")
setorder(individual.household.data, IID)

# Fully cooperating households

individual.household.data <- individual.household.data[OutCom_B02ID==1]

# Bike and car access 

individual.household.data[, BicycleAccess:=0]
individual.household.data[`Own or use a bicycle`<3, BicycleAccess:=1]
individual.household.data[,BicycleAccess:=as.factor(BicycleAccess)]

individual.household.data[,CarAccess:=0]
individual.household.data[`Access to car`<4,CarAccess:=1]
individual.household.data[,CarAccess:=as.factor(CarAccess)]

rm(month.list, psu.data)

###############################
## Create Ticket Information ##
###############################

# Want to have non concessionary tickets only otherwise prices will be distorted 

ticket.data <- read.table("ticket.tab", header=TRUE, sep="\t")
ticket.data <- ticket.data[ticket.data$SurveyYear==2012,]  ## Subsetting the data to get the most recent year
ticket.data <- remove.columns(ticket.data)
ticket.data <- data.table(ticket.data)
colnames(ticket.data) <- replace.colnames(ticket.data)
ticket.data[, NCSeasonTicket:=0]
ticket.data[`Season ticket or travel pass held by individual` %in% c(1:6), NCSeasonTicket:=1]
ticket.data[`Main mode ticket has been used on` %in% c(2,6,8,9,10), NCSeasonTicket:=0]
table(ticket.data$`Modes of transport ticket can be used on`)
ticket.data[NCSeasonTicket==1 & `Modes of transport ticket can be used on` %in% c(1,3),  RailOnly:=1]
ticket.data[NCSeasonTicket==1 & `Modes of transport ticket can be used on`==4, BusOnly:=1]
ticket.data[NCSeasonTicket==1 & `Modes of transport ticket can be used on`==7, RailBus:=1]
ticket.data[NCSeasonTicket==1 & `Modes of transport ticket can be used on` %in% c(5,11), Other:=1]

# Ticket Transport Type Factor

ticket.data[, TicketType:=numeric(.N)]
ticket.data[NCSeasonTicket==0, TicketType:=0 ]
ticket.data[RailOnly==1 & `Length of time ticket lasts for`==1, TicketType:=1]
ticket.data[RailOnly==1 & `Length of time ticket lasts for` %in% 2:4, TicketType:=2]
ticket.data[RailOnly==1 & `Length of time ticket lasts for` %in% 5:7, TicketType:=3]
ticket.data[BusOnly==1 & `Length of time ticket lasts for`==1, TicketType:=4]
ticket.data[BusOnly==1 & `Length of time ticket lasts for` %in% c(2,3,4), TicketType:=5]
ticket.data[BusOnly==1 & `Length of time ticket lasts for` %in% c(5,6), TicketType:=6]
ticket.data[RailBus==1 & `Length of time ticket lasts for`==1, TicketType:=7]
ticket.data[RailBus==1 & `Length of time ticket lasts for` %in% 2:4, TicketType:=8]
ticket.data[RailBus==1 & `Length of time ticket lasts for` %in% 5:7, TicketType:=9]

ticket.type.levels <- c(0:9)
# ticket.type.labels <- c("No bus or rail ticket", "Weekly Rail", "Monthly Rail", "Annual Rail", "Weekly Bus", "Monthly Bus", "Annual Bus",
#                         "Weekly Rail & Bus", "Monthly Rail & Bus", "Annual Rail & Bus")
ticket.data[,TicketType:=factor(x=TicketType, levels=ticket.type.levels)]

# London Specific - call it oyster for the time being

# ticket.data[`SeasonTicket`==1 & `Modes of transport ticket can be used on`==9  & `Length of time ticket lasts for`==1, TicketType:= "Weekly Oyster"]
# ticket.data[`SeasonTicket`==1 & `Modes of transport ticket can be used on`==9  & `Length of time ticket lasts for` %in% c(2,3,4), TicketType:= "Monthly Oyster"]
# ticket.data[`SeasonTicket`==1 & `Modes of transport ticket can be used on`==9  & `Length of time ticket lasts for` %in% c(5,6), TicketType:= "Annual Oyster"]

## Keep a premerged copy of the individual.household.data key columns
## need to merge the travel time and stage cost columns 

# write.csv(x=individual.household.data[, c(1:9), with=FALSE], file="individualhousehold.csv")

#################################
## Merging Trip and Stage Data ## 
#################################

trip.stage.key <- c("PSUID", "HouseholdID", "IndividualID", "TripID", "DayID", "SurveyYear", "PersNo", "TravDay", "JourSeq")
trip.data <- read.table("trip.tab", header=FALSE, colClasses="numeric", sep="\t", skip=1000000)
trip.data.h <- read.table("trip.tab", header=FALSE, stringsAsFactors=FALSE, sep="\t", nrows=1)
names(trip.data) <- unlist(trip.data.h)
names(trip.data) <- replace.colnames(trip.data)
names(trip.data) <- str_trim(names(trip.data), side="both")
trip.data <- remove.columns(trip.data)
trip.data <- data.table(trip.data, key=trip.stage.key)
trip.data <- trip.data[SurveyYear==2012]
setorder(trip.data, PSUID, HouseholdID, IndividualID, TravDay, JourSeq)
trip.data[,JID:=1:nrow(trip.data)] # generate new ID column ## d1 is Travel day and j3 is jourseq
rm(trip.data.h)

# Removing uneeded columns

trip.data[,49:63:=NULL, with=FALSE]

# Stage Data

stage.data <- read.table("stage.tab", header=FALSE, colClasses="numeric", sep="\t", skip=1000000)
stage.data.h <- read.table("stage.tab", header=FALSE, stringsAsFactors=FALSE, sep="\t", nrows=1)
names(stage.data) <- unlist(stage.data.h)
names(stage.data) <- replace.colnames(stage.data)
names(stage.data) <- str_trim(names(stage.data), side="both")
stage.data <- remove.columns(stage.data)
stage.data <- data.table(stage.data, key=trip.stage.key)
stage.data <- stage.data[SurveyYear==2012]
setorder(stage.data, PSUID, HouseholdID, IndividualID, TravDay, JourSeq, StageSeq)
rm(stage.data.h)

# Remove uneeded columns

stage.data[,48:59:=NULL, with=FALSE]

# Merge trip and stage data

trip.stage.data <- merge(trip.data, stage.data)
rm(trip.data, stage.data)

#####################################################

trip.stage.data <- trip.stage.data[`Whether main stage of trip`==1 & 
                                   `Stage mode of travel - publication table breakdown - 13 categories` %in% c(1:4,8,11,12)] # Keeping the main stage of the trip
trip.stage.data <- trip.stage.data[`Main mode of travel - publication table breakdown - 13 categories` %in% c(1:4,8,11,12)]

# Create Trip Purpose Factor this combines both personal trips and escort trips together

#####################################################

trip.stage.data[`Trip purpose - full list - 23 categories`<=3 & `Trip purpose to - 23 categories`!=23, TripPurpose:='Commuting or Education']
trip.stage.data[`Trip purpose - full list - 23 categories` %in% 4:8 & `Trip purpose to - 23 categories`!=23, TripPurpose:='Shopping or Personal Business']
trip.stage.data[`Trip purpose - full list - 23 categories` %in% 9:18 & `Trip purpose to - 23 categories`!=23, TripPurpose:='Entertainment or Visit Friends']
trip.stage.data[`Trip purpose - full list - 23 categories` %in% c(19,20,21) & `Trip purpose to - 23 categories`!=23, TripPurpose:='Commuting or Education'] # Escort
trip.stage.data[`Trip purpose - full list - 23 categories`==22 & `Trip purpose to - 23 categories`!=23, TripPurpose:='Shopping or Personal Business'] # Escort
trip.stage.data[`Trip purpose - full list - 23 categories`==23 & `Trip purpose to - 23 categories`!=23, TripPurpose:='Entertainment or Visit Friends'] # Escort
trip.stage.data[`Trip purpose to - 23 categories`==23, TripPurpose:='Return Home']
trip.stage.data[, TripPurpose:=as.factor(TripPurpose)]

# Journey Location

trip.stage.data[`Trip purpose from - 23 categories`<=3, Location:='Commuting or Education']
trip.stage.data[`Trip purpose from - 23 categories` %in% 2:8, Location:='Shopping or Personal Business']
trip.stage.data[`Trip purpose from - 23 categories` %in% 9:16, Location:='Entertainment or Visit Friends']
trip.stage.data[`Trip purpose from - 23 categories` %in% 17:20, Location:='Commuting or Education'] # Escort
trip.stage.data[`Trip purpose from - 23 categories`==21, Location:='Shopping or Personal Business'] # Escort
trip.stage.data[`Trip purpose from - 23 categories`==22, Location:='Entertainment or Visit Friends'] # Escort
trip.stage.data[`Trip purpose from - 23 categories`==23, Location:='Travelling Home']
trip.stage.data[, TripPurpose:=as.factor(TripPurpose)] 

# Journey Time

trip.stage.data[`Trip start time band - 24 hourly bands`<=7, JourneyTime:=1]
trip.stage.data[`Trip start time band - 24 hourly bands`==8, JourneyTime:=2]
trip.stage.data[`Trip start time band - 24 hourly bands`==9, JourneyTime:=3]
trip.stage.data[`Trip start time band - 24 hourly bands`==10, JourneyTime:=4]
trip.stage.data[`Trip start time band - 24 hourly bands`==11, JourneyTime:=5]
trip.stage.data[`Trip start time band - 24 hourly bands`==12, JourneyTime:=6]
trip.stage.data[`Trip start time band - 24 hourly bands`==13, JourneyTime:=7]
trip.stage.data[`Trip start time band - 24 hourly bands`==14, JourneyTime:=8]
trip.stage.data[`Trip start time band - 24 hourly bands`==15, JourneyTime:=9]
trip.stage.data[`Trip start time band - 24 hourly bands`==16, JourneyTime:=10]
trip.stage.data[`Trip start time band - 24 hourly bands`==17, JourneyTime:=11]
trip.stage.data[`Trip start time band - 24 hourly bands`==18, JourneyTime:=12]
trip.stage.data[`Trip start time band - 24 hourly bands`==19, JourneyTime:=13]
trip.stage.data[`Trip start time band - 24 hourly bands`==20, JourneyTime:=14]
trip.stage.data[`Trip start time band - 24 hourly bands`==21, JourneyTime:=15]
trip.stage.data[`Trip start time band - 24 hourly bands` %in% 22:24, JourneyTime:=16]

# Turn time into an ordered factor

time.levels <- 1:16
time.labels <- c("Before 7:00","7:00","8:00","9:00","10:00","11:00","12:00"
                 ,"13:00","14:00","15:00","16:00","17:00","18:00","19:00"
                 ,"20:00","After 20:00")

trip.stage.data[,JourneyTime:=factor(JourneyTime, levels=time.levels, labels=time.labels)]

# Car Descriptions

trip.stage.data <- trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories` %in% c(1:4,8,11,12)]
trip.stage.data <- trip.stage.data[`Main mode of travel - publication table breakdown - 13 categories` %in% c(1:4,8,11,12)]

trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==8,Mode:="Bus"]
trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==1,Mode:="Walk"]
trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==2,Mode:="Bicycle"]
trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==3,Mode:="Car/van driver"]
trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==4,Mode:="Car/van passenger"]
trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==11,Mode:="Rail"]
trip.stage.data[`Stage mode of travel - publication table breakdown - 13 categories`==12,Mode:="Taxi"]
trip.stage.data[,Mode:=factor(Mode)]

trip.stage.data <- merge(trip.stage.data, day.data, by=c("PSUID", "HouseholdID", "IndividualID", "DayID", "PersNo", "TravDay"))
rm(day.data)

# Merge individual.household and trip.stage

all.data <- merge(individual.household.data, trip.stage.data, by=c("PSUID", "HouseholdID", "IndividualID", "PersNo"))

all.data.key = c("PSUID", "HouseholdID", "IndividualID", "TravDay", "JourSeq")
setkeyv(all.data, all.data.key)

# Data view utils::View(trip.stage.data)

# Merge all.data and ticket.data may have to question this later leave this out for now so we know what we are doing with it

## all.data <- merge(all.data, ticket.data, by=c("PSUID", "HouseholdID", "IndividualID", "PersNo", "IndTicketID"), all.x=TRUE)

# Initialise the columns for the car location at the start and end of the journey

all.data[, CarLocationStart:=character(nrow(all.data))] 
all.data[, CarLocationEnd:=character(nrow(all.data))]
all.data[, BicycleLocationStart:=character(nrow(all.data))]
all.data[, BicycleLocationEnd:=character(nrow(all.data))]

for (i in min(all.data$TravDay):max(all.data$TravDay)) {
        for (j in 1:max(all.data$JourSeq)) {
                if (j==1) {
                        all.data[TravDay==i & JourSeq==j, CarLocationStart:=Location]
                        all.data[TravDay==i & JourSeq==j, CarLocationEnd:=Location]
                        all.data[TravDay==i & JourSeq==j, BicycleLocationStart:=Location]
                        all.data[TravDay==i & JourSeq==j, BicycleLocationEnd:=Location]
                } 
                all.data[TravDay==i & JourSeq==j & `Stage mode of travel - publication table breakdown - 13 categories`==3, CarLocationEnd:=TripPurpose]  
        all.data[TravDay==i & JourSeq==j & `Stage mode of travel - publication table breakdown - 13 categories`==2, BicycleLocationEnd:=TripPurpose]  
                if (j>1) {
                        all.data[TravDay==i & JourSeq==j,CarLocationStart:=shift(CarLocationEnd, 1L, type="lag")]
                        all.data[TravDay==i & JourSeq==j,BicycleLocationStart:=shift(BicycleLocationEnd, 1L, type="lag")]
                }  
        }
}

# I am removing all car/van passenger journies where multiple people from the same household
# took the same journey. True car van passenger journies are the remaining ones

setnames(all.data, "Day of week trip took place", "DayOfWeek")
all.data[Mode %in% c("Car/van driver", "Car/van passenger"), NoPeopleTrip:=length(IndividualID), by=.(HouseholdID, DayOfWeek, TripStart, TripEnd)]
all.data <- all.data[!(Mode=="Car/van passenger" & NoPeopleTrip>1)]


# Have Car/Bicycle Dummy Variables - idea behind this is if one has a car or a bike at their current location - will use it on the next stage
# either commuting home or whatever

all.data[, HaveCar:=0]
all.data[CarLocationStart==Location, HaveCar:=1]
all.data[, HaveBicycle:=0]
all.data[BicycleLocationStart==Location, HaveCar:=1]

# Create Location Data
# rm(individual.household.data, trip.stage.data)

# location.data <- data.table(all.data[,c("PSUID",
#                                         "HouseholdID", 
#                                         "IndividualID",
#                                         "TripID",
#                                         "Location", 
#                                         "JourneyTime", 
#                                         "CarLocationStart", 
#                                         "BicycleLocationStart")])

# write.csv(location.data, file="location.csv")
# write.csv(all.data, file="all.csv")

# Merge ticket data

all.data <- merge(all.data, ticket.data, by=c("PSUID", "IndividualID", "HouseholdID", "IndTicketID", "PersNo"), all.x=TRUE)

# General cleaning of the workspace

rm(individual.household.data, trip.stage.data, ticket.data, all.data.key, lookups.list, ticket.type.levels, time.labels, time.levels, trip.stage.key)

##############################
## Cleaning and factorising ## 
##############################

# Changing column names for readability
setnames(all.data, "TWEMonth", "Month" )
setnames(all.data, "Type of day trip took place on (2008 onwards)", "DayType")
setnames(all.data, "Walk time from household to nearest bus stop - minutes - banded time", "BusStopWalkDist")
setnames(all.data, "Sex of person", "Sex")
setnames(all.data, "Getbus_B01ID", "BusFreq")
setnames(all.data, "Rate the reliability of local buses", "BusRel")
setnames(all.data, "Difficulties when travelling by foot", "WalkDifficulties")
setnames(all.data, "Disability that causes difficulties in using local bus", "BusDifficulties")
setnames(all.data, "Working status of individual - Summary - 6 categories", "WorkStatus")
setnames(all.data, "Journey time from household to nearest GP - minutes - banded time", "JtGP")
setnames(all.data, "Journey time from household to nearest chemist - minutes - banded time", "JtChem")
setnames(all.data, "Journey time from household to nearest hospital - minutes - banded time", "JtHosp")
setnames(all.data, "Journey time from household to nearest shopping centre - minutes - banded time", "JtShop")
setnames(all.data, "Journey time from household to nearest grocer - minutes - banded time", "JtGroc")
setnames(all.data, "Journey time from household to nearest post office - minutes - banded time", "JtPO")
setnames(all.data, "Journey time from household to nearest primary school - minutes - banded time", "JtPS")
setnames(all.data, "Journey time from household to nearest secondary school - minutes - banded time", "JtSS")
setnames(all.data, "Journey time from household to nearest college - minutes - banded time", "JtC")
setnames(all.data, "Legal marital status", "MaritalStatus")
setnames(all.data, "How would you rate the condition of the pavement where you live", "PavementCond")
setnames(all.data, "How would you rate the provision of the cycle lanes in your area", "CycleLanes")
setnames(all.data, "Statistical Region - regional / Metropolitan area breakdown", "Region")
setnames(all.data, "TripDisIncSW", "Distance")
setnames(all.data, "TripTotalTime", "Time")
setnames(all.data, "StageCost", "Price")
setnames(all.data, "Disabled drivers", "DisabledDriver")
setnames(all.data, "Walk time from household to nearest railway station - minutes - banded time", "WalkTimeRail")
setnames(all.data, "Bus time from household to railway station - minutes - banded time", "BusTimeRail")
setnames(all.data, "DescTa_B01ID", "RailFreq")
setnames(all.data, "Rate the frequency of trains / LU / light rail / metro / tram stop", "RailFreqRate")
setnames(all.data, "Rate the reliability of trains / LU / light rail / metro / tram stop", "RailReliability")
setnames(all.data, "Ethnic Group for time series purposes - 2011 bandings - 2 categories", "Minority")
setnames(all.data, "Type of driving licence held - Summary banding - 3 categories", "DrivingLicense")
setnames(all.data, "Own or use a bicycle", "BicycleOwner")
setnames(all.data, "2011 Census Rural-Urban Classification - England and Wales - 3 categories", "UrbanRural")

##################
## Vehicle Data ##
##################

lookups.list.values <- data.table(lookups.list.values)
regions.merge <- lookups.list.values[Variable.name=="PSUStatsReg_B01ID",.(New.Value, Values.Description)]
setnames(regions.merge, c("New.Value", "Values.Description"), c("Region", "RegionName"))
all.data <- merge(all.data, regions.merge, by="Region", all.x=TRUE)
all.data[,Month:=as.factor(Month)]

petrol.data <- data.table(read.csv("petrolprices2012.csv", strip.white = TRUE))
diesel.data <- data.table(read.csv("dieselprices2012.csv", strip.white = TRUE))

## This imputation will be more difficult than the others as it requires information on the
## can use data on fuel prices
## the metric variables are in litres/100km rather than mpg

vehicle.efficiency.data <- data.table(read.csv("fuelefficiency2012.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1"))
setnames(vehicle.efficiency.data, "Engine.Capacity", "EngineCapacity")
setnames(vehicle.efficiency.data, "Fuel.Type", "FuelType")

# Dropping the non petrol and non diesel cars as they are marginal cases - the trip cost for
# these alternatives will be different - questions such as charging, do they use fuel all the way ect -- just too
# complicated to work out.

vehicle.efficiency.data <- vehicle.efficiency.data[FuelType %in% c("Petrol", "Diesel") ,]
size.efficiency.reg <- lm(formula=Metric.Combined~EngineCapacity+as.factor(FuelType)-1, data=vehicle.efficiency.data)

## Add monthly region petrol and diesel prices 
## This requires some changing of the data to match up the regions between the two dataset, although the match is pretty good
## the only difference is that there is no metropolian/non split in the fuel price data from the AA

## Petrol Data - could potentially turn this into a function later ##

petrol.data <- rbind(petrol.data, petrol.data[rep(4:7),]) 
petrol.data[Region=="North",Region:="Northern"] 
petrol.data[,Region:=as.character(Region)]
petrol.data <-petrol.data[Region!="Northern Ireland",]
setorder(petrol.data,Region)
new.region.names <- sort(lookups.list.values[Variable.name=="PSUStatsReg_B01ID" & !(New.Value %in% c(-8,-9)) ,Values.Description])
petrol.data[,Region:=new.region.names]
names(petrol.data)[1:13] <- c("RegionName", 12:1)
petrol.data.melt <- melt(petrol.data, id=1, measure.vars = 2:13, variable.name="Month", value.name="PetrolPrice", variable.factor=TRUE)
all.data <- merge(all.data, petrol.data.melt, by=c("Month", "RegionName"), all.x=TRUE)

## Do the same thing for diesel

diesel.data <- rbind(diesel.data, diesel.data[rep(4:7),]) 
diesel.data[Region=="North",Region:="Northern"] # this is done to get the regions in the right order - this is the easiest method 
diesel.data[,Region:=as.character(Region)]
diesel.data <-diesel.data[Region!="Northern Ireland",] # removing northern ireland as we dont have the data
setorder(diesel.data,Region)
new.region.names <- sort(lookups.list.values[Variable.name=="PSUStatsReg_B01ID" & !(New.Value %in% c(-8,-9)) ,Values.Description])
diesel.data[,Region:=new.region.names]
names(diesel.data)[1:13] <- c("RegionName", 12:1)
diesel.data.melt <- melt(diesel.data, id=1, measure.vars = 2:13, variable.name="Month", value.name="DieselPrice", variable.factor=TRUE)
all.data <- merge(all.data, diesel.data.melt, by=c("Month", "RegionName"), all.x=TRUE)

# Creating log time and distance variables

all.data[BusTimeRail==-8, BusTimeRail:=NA]

all.data[,LogDistance:=log(Distance)]
all.data[,LogTravelTime:=log(Time)]
all.data[,LogDistance2:=LogDistance*LogDistance]

## Recoding -8, -9, -10 as NA or other where appropriate ##


# all.data[,c(199:217,324:325,333:336,348:380),with=FALSE]


## Recoding to NAs journey time to location variables ## update - no longer using this so this is no depreciated ##

# for (k in 154:162) {
#     set(x=all.data, i=which(all.data[[k]] %in% c(-8,-9,-10)), j=k, value=NA)
# }

# Bicycle Ownership

all.data[BicycleOwner==-8, BicycleOwner:=NA]
all.data[BicycleOwner %in% 2:3, BicycleOwner:=1]
all.data[BicycleOwner==4, BicycleOwner:=0]
all.data[,BicycleOwner:=factor(BicycleOwner)]

# Ethnicity

all.data[Minority==-8, Minority:=NA]
all.data[,Minority:=Minority-1]
all.data[,Minority:=as.factor(Minority)]

# Driving License

all.data[DrivingLicense %in% c(-9,2,3), DrivingLicense:=0]
all.data[DrivingLicense==-8, DrivingLincense:=NA]
all.data[,DrivingLicense:=factor(DrivingLicense)]

rm(diesel.data.melt, diesel.data, petrol.data.melt, petrol.data, regions.merge, vehicle.data, vehicle.efficiency.data)

all.data[WalkDifficulties==-9, WalkDifficulties:=1]
all.data[WalkDifficulties==-8, WalkDifficulties:=NA]
all.data[BusDifficulties==-9, BusDifficulties:=1]
all.data[BusDifficulties==-8, BusDifficulties:=NA]
all.data[BusDifficulties==5, BusDifficulties:=4]   # This factor is not run so need to amalgamate for predict to work
all.data[BusRel==-9, BusRel:=NA] 
all.data[,Sex:=Sex-1]

# all.data[,BusRel:=as.factor(BusRel)]
# all.data[is.na(BusRel)==TRUE, BusRel:=8]
# all.data[,BusRel:=as.factor]

# Factor for workstatus for those in full time education

all.data[AgeCategory %in% c("5-10", "11-16"), WorkStatus:=5]

#Transform Day Type into a Normal/Holiday binary variable to avoid multicollinearity issues

all.data[DayType %in% c(1,3,4), DayType:=0]
all.data[DayType %in% c(2), DayType:=1]

# Ticket factors

all.data[,Concession:=0]
all.data[`Season ticket or travel pass held by individual` %in% 7:11, Concession:=1]
all.data[,Concession:=factor(Concession)]

all.data[,TicketType:=as.numeric(TicketType)]
all.data[is.na(TicketType)==TRUE,TicketType:=0]
all.data[,TicketType:=factor(TicketType)]

# Disabled Drivers - 0 factor for not disabled driver

all.data[DisabledDriver==-9, DisabledDriver:=0]

# Pavement condition and cycle lanes

all.data[PavementCond %in% c(-8,7,8), PavementCond:=NA]
all.data[CycleLanes %in% c(8,-8), CycleLanes:=NA]

# Turning the numeric columns into factors

all.data[,Sex:=as.factor(Sex)]
all.data[,BusStopWalkDist:=factor(BusStopWalkDist,ordered=TRUE)]
all.data[,WalkDifficulties:=factor(WalkDifficulties)]
all.data[,BusFreq:=factor(BusFreq, levels=c(1,2,3,4,5), ordered=TRUE)]
all.data[,BusDifficulties:=factor(BusDifficulties)]
all.data[,WorkStatus:=as.factor(WorkStatus)]
all.data[,BusRel:=factor(BusRel)]
all.data[,DayType:=factor(DayType)]
all.data[,DayOfWeek:=factor(DayOfWeek)]
all.data[,JtGP:=factor(JtGP, ordered=TRUE)]
all.data[,JtChem:=factor(JtChem, ordered=TRUE)]
all.data[,JtHosp:=factor(JtHosp, ordered=TRUE)]
all.data[,JtShop:=factor(JtShop, ordered=TRUE)]
all.data[,JtGroc:=factor(JtGroc, ordered=TRUE)]
all.data[,JtPO:=factor(JtPO, ordered=TRUE)]
all.data[,JtPS:=factor(JtPS, ordered=TRUE)]
all.data[,JtSS:=factor(JtSS, ordered=TRUE)]
all.data[,JtC:=factor(JtC, ordered=TRUE)]
all.data[,PavementCond:=factor(PavementCond)]
all.data[,CycleLanes:=factor(CycleLanes)]
all.data[,DisabledDriver:=factor(DisabledDriver)]
all.data[,Region:=factor(Region)]
all.data[,PSUIDFactor:=factor(PSUID)]
all.data[,NCSeasonTicket:=factor(NCSeasonTicket)]
all.data[,WalkTimeRail:=factor(WalkTimeRail)]
all.data[,BusTimeRail:=factor(BusTimeRail)]
all.data[,RailFreqL:=factor(RailFreq)]

all.data[RailFreqRate %in% c(-9,8,7), RailFreqRate:=NA]
all.data[,RailFreqRate:=factor(RailFreqRate)]

all.data[RailReliability==-9, RailReliability:=8]
all.data[,RailReliability:=factor(RailReliability)]

all.data[is.na(RailOnly), RailOnly:=0]
all.data[is.na(BusOnly), BusOnly:=0]
all.data[is.na(RailBus), RailBus:=0]

## Remove columns with only one element value

discard <- vector()
for (i in seq_along(names(all.data))) {
  discard[i] <-nrow(na.omit(unique(all.data[,i, with=FALSE])))
}

## Extra variables

##########################
## Modal Choice Dataset ##
##########################

## Creating the long table for modal choice
## One merge to get individual factors, another merge to get the trip level factors
## some issues with the all data long not merging for some reason

rm(all.data.long)

all.individual <- unique(all.data[,.(PSUID, PSUIDFactor, IndividualID, HouseholdID, BusDifficulties, WalkDifficulties, BusStopWalkDist, BusFreq, BusRel, 
                                     AgeCategory, Sex, WorkStatus, `Household Income Quintiles - Diary Sample 2012`, MaritalStatus, `Social class of individuals`, 
                                     `Mobility difficulties summary`, Region, PavementCond, CycleLanes, RailReliability, RailFreqRate, RailFreq, DisabledDriver, 
                                     WalkTimeRail, BusTimeRail, DrivingLicense, Minority, BicycleOwner, PetrolPrice, DieselPrice, NumCarVan, HHoldNumPeople, UrbanRural)])

# Get primary vehicle for household
## Using the vehicle data to help predict car price data

all.data[,VehicleID.x:=NULL]
setnames(all.data,"VehicleID.y", "VehicleID")
vehicle.data <- data.table(read.csv("vehicle.tab", sep="\t", header=TRUE))
vehicle.data <- vehicle.data[SurveyYear==2012]
names(vehicle.data) <- replace.colnames(vehicle.data)
vehicle.data[,PSUID:=NULL]
vehicle.data[,IndividualID:=NULL]
vehicle.data <- vehicle.data[VehNo==1, .(HouseholdID, VehicleID, EngineCap, VehPropTypeTS_B01ID)] 
setnames(vehicle.data, "VehPropTypeTS_B01ID", "FuelType")
vehicle.data[,FuelType:=factor(FuelType, levels=c(1,2), labels=c("Petrol", "Diesel"))]

all.trip <- all.data[,.(Mode, StageID, TripID, JourneyTime, DayOfWeek, DayType, Month, LogDistance, LogTravelTime, 
                        LogDistance2, TripPurpose, Price, TicketType)]

modes <- data.table(c("Bus", "Walk", "Bicycle", "Car/van driver", "Car/van passenger", "Rail","Taxi"))
names(modes) <- "Mode"

## This does not return the right number of rows - investigate further.
## Need to merge the vehicles again as entries are missing for many



# utils::View(all.data[Mode=="Car/van driver"])

x <- all.data[,.(IndividualID, HouseholdID, TripID, PSUID, StageID)]

all.data.long <- setkey(x[,c(k=1,.SD)],k)[modes[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

all.data.long <- merge(all.data.long, all.individual, by=c("PSUID", "IndividualID", "HouseholdID"))

all.data.long <- merge(all.data.long, all.trip, by=c("TripID", "StageID"), all.x=TRUE)

all.data.long <- merge(all.data.long, vehicle.data, by="HouseholdID", all.x=TRUE)


# View(all.data.long)

setnames(all.data.long, "Mode.x", "Alternative")
setnames(all.data.long, "Mode.y", "ChosenAlternative")
setnames(all.data.long, "EngineCap", "EngineCapacity")
all.data.long[,IsChosenAlternative:=0]
all.data.long[Alternative==ChosenAlternative, IsChosenAlternative:=1]
all.data.long[,ChosenAlternative:=NULL]
all.data.long[IsChosenAlternative==0,LogTravelTime:=NA]
all.data.long[IsChosenAlternative==0,Price:=NA]

rm(all.individual, all.trip, x, modes, i, j, k, new.region.names)

# Save an image of the work enviroment, for quick access later.

save.image("Data.Rda")



