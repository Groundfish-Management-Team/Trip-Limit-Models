####################################################################################################################################################################
####################################################################################################################################################################

#PROJECT:  TRIP LIMIT MODEL (INCIDENTAL HALIBUT)
#R ANALYSTS:  CHRISTIAN HEATH
#OTHER ANALYST: PATRICK MIRICK AND JESSI DOERPINGHAUS

#WHAT DOES THIS SCRIPT DO?
# (1) MODEL NEW TRIP LIMITS FOR INCIDENTAL PACIFIC HALIBUT ON SABLFISH TRIPS
#     (A) TWO ALTERNATE TRIP LIMITS TO LOOK AT FOR 2020

#FIRST STEPS:
# (1) GATHER DATA FROM PACFIN ANSWERS (OR PACFIN QUERY BUILDER TOOL) - COMPREHENSIVE FISH TICKETS - COLUMN NAMES AS FOLLOWED:
#         LANDING_YEAR |	LANDING_MONTH |	LANDING_DATE |	VESSEL_NUM |	FISH_TICKET_ID |	FTID |	AGENCY_CODE |	PARTICIPATION_GROUP_CODE | PORT_CODE | PORT_NAME
#         CATCH_AREA_CODE | PACFIN_CATCH_AREA_CODE | COUNCIL_CODE |	NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME |	PACFIN_SPECIES_COMMON_NAME | DAHL_GROUNDFISH_CODE
#         GMT_SABLEFISH_CODE | PACFIN_GROUP_GEAR_CODE | PACFIN_GEAR_DESCRIPTION | REMOVAL_TYPE_CODE | CONDITION_CODE | CONDITION_NAME | DISPOSITION_NAME
#         ROUND_WEIGHT_LBS (SUM) | EXVESSEL_REVENUE (SUM) | LANDED_WEIGHT_LBS (SUM)
# (2) FILTERED WITHIN ANSWERS FOR THE FOLLOWING:
#         PARTICIPATION_GROUP_CODE does not contain A
#         COUNCIL_CODE is equal to / is in P
#         LANDING_YEAR is between 2012 and 2019
#         NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME contains any PACIFIC HALIBUT; SABLEFISH
# (4) EXPORT TO CSV TO PULL INTO R

####################################################################################################################################################################
####################################################################################################################################################################

##### SECTION 1: SET-UP #####
setwd("C:/Users/heathch/Desktop/R Analysis/Data Input/Groundfish/Commercial_GMT/Trip_Limit_Model/Incidental_Halibut")  # Change the directory path at your convenience
.libPaths("C:/Users/heathch/Desktop/R Analysis/rpackages")
options(scipen = 999)

### LOAD PACKAGES ###
library(easypackages)
libraries ("dplyr", "plyr","data.table", "ggplot2", "xlsx", "ROracle", "dtplyr", "tidyr", "lubridate", "zoo", "Rmisc", "reshape", "reshape2", "dbplyr", "lattice")

#LOAD COMMON TICKET FILE (PAT WILL SEND LINK FROM PACFIN)
Trip <- read.csv("Inc_Hal_Trip_Limit.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

### TO CHECK WHAT ALL YOUR FIELDS ARE TO MAKE SURE YOU DIDN'T FORGET ANYTHING FROM YOUR PACFIN EXTRACT ###
names(Trip)  
colnames(Trip)[1] <- c('LANDING_YEAR')
colnames(Trip)[24:26] <- c('ROUND_WEIGHT_LBS','EXVESSEL_REVENUE','LANDED_WEIGHT_LBS')

####################################################################################################################################################################
##### SECTION 2: FILTER / CLEAN-UP #####

### FILTER FOR WASHINGTON AND SABLEFISH FISHERY WITH HALIBUT INCIDENTAL ###
Trip <- subset(Trip, AGENCY_CODE == "W")
Trip <- subset(Trip, GMT_SABLEFISH_CODE == "PRI" | GMT_SABLEFISH_CODE == "LEN")
Trip <- subset(Trip, REMOVAL_TYPE_CODE != "E")

### FILTER FOR TRIPS NORTH OF POINT CHEHALIS AND COASTAL (P. HALIBUT ONLY) ###
PH <- subset(Trip, PACFIN_SPECIES_COMMON_NAME == "PACIFIC HALIBUT")
PH <- subset(PH, CATCH_AREA_CODE != "60A2" & CATCH_AREA_CODE != "61")
Trip <- subset(Trip, PACFIN_SPECIES_COMMON_NAME != "PACIFIC HALIBUT")
Trip <- rbind(Trip, PH)

### AGGREGATE BY SUM / SPREAD BY SPECIES ###
Inc_Hal <- aggregate(LANDED_WEIGHT_LBS ~ LANDING_YEAR + LANDING_MONTH + LANDING_DATE + VESSEL_NUM + PACFIN_SPECIES_COMMON_NAME, data = Trip, FUN = sum)
Inc_Hal <- spread(Inc_Hal, PACFIN_SPECIES_COMMON_NAME, LANDED_WEIGHT_LBS)
colnames(Inc_Hal)[5:6] <- c('PACIFIC_HALIBUT','SABLEFISH')

### BRING IN NET WEIGHT FOR P. HALIBUT ###
Inc_Hal[is.na(Inc_Hal)] <- 0
Inc_Hal$PACIFIC_HALIBUT_NET <- Inc_Hal$PACIFIC_HALIBUT *0.88

### LOOK AT ONLY MONTHS APRIL THROUGH OCTOBER ###
Inc_Hal <- subset(Inc_Hal, LANDING_MONTH > 3 & LANDING_MONTH < 11)

####################################################################################################################################################################
##### SECTION 3: GGPLOT P. HALIBUT - REFERENCE POINTS #####

### BY YEAR ###
#Hal_12 <- subset(Inc_Hal, LANDING_YEAR == 2012)
#Hal_13 <- subset(Inc_Hal, LANDING_YEAR == 2013)
#Hal_14 <- subset(Inc_Hal, LANDING_YEAR == 2014)
Hal_15 <- subset(Inc_Hal, LANDING_YEAR == 2015)
Hal_16 <- subset(Inc_Hal, LANDING_YEAR == 2016)
Hal_17 <- subset(Inc_Hal, LANDING_YEAR == 2017)
Hal_18 <- subset(Inc_Hal, LANDING_YEAR == 2018)
Hal_19 <- subset(Inc_Hal, LANDING_YEAR == 2019)
Hal_20 <- subset(Inc_Hal, LANDING_YEAR == 2020)
Hal_15_20 <- subset(Inc_Hal, LANDING_YEAR > 2015)
#H_12 <- ggplot(data=Hal_12, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("2012")
#H_13 <- ggplot(data=Hal_13, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("2013")
#H_14 <- ggplot(data=Hal_14, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("2014")
H_15 <- ggplot(data=Hal_15, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2015") + xlab("Month") + ylab("Net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))
H_16 <- ggplot(data=Hal_16, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2016") + xlab("Month") + ylab("Net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))
H_17 <- ggplot(data=Hal_17, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2017") + xlab("Month") + ylab("Net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))
H_18 <- ggplot(data=Hal_18, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2018") + xlab("Month") + ylab("Net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))
H_19 <- ggplot(data=Hal_19, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2019") + xlab("Month") + ylab("Net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))
H_20 <- ggplot(data=Hal_20, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2020") + xlab("Month") + ylab("Net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))
H_15_20 <- ggplot(data=Inc_Hal, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2015-2020") + xlab("Month") + ylab("Average net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))

### LIMIT TO COMPLETE YEARS POST 2015 (CLEAR CHANGE IN FISHING BEHAVIOR OR POSSIBLY FISHING REGULATIONS STARTING IN 2016) ###
Hal <- subset(Inc_Hal, LANDING_YEAR > 2015 & LANDING_YEAR < 2020)
Hal <- ddply (Hal, c("LANDING_MONTH"), summarise, PACIFIC_HALIBUT_NET = sum(PACIFIC_HALIBUT_NET, na.rm=T)/5)
H <- ggplot(data=Hal, aes(x=LANDING_MONTH, y=PACIFIC_HALIBUT_NET))+geom_bar(stat="identity")+ggtitle("P. Halibut Landed 2015-2019") + xlab("Month") + ylab("Average net wt. lbs") + scale_x_continuous(breaks = seq(4, 10, by = 1))

### PLOT ###
dev.new()
multiplot(H_15, H_16, H_17, H_18, H_19, H_20, H, cols = 3)

### YEAR / MONTH BREAKDOWN ###
Hal_Year <- ddply(Hal, c("LANDING_YEAR"), summarise, PACIFIC_HALIBUT_NET = sum(PACIFIC_HALIBUT_NET))
Hal_Year
Hal_Month <- ddply(Hal, c("LANDING_MONTH"), summarise, PACIFIC_HALIBUT_NET = sum(PACIFIC_HALIBUT_NET))
Hal_Month

####################################################################################################################################################################
##### TRIP LIMIT MODELING #####

### USE 2019 AS A REFERENCE YEAR  - STRIP OUT JAN - AUG (ACTUAL 2020 DATA) ###
Inc_Hal19 <- subset(Inc_Hal, LANDING_YEAR == 2019)
Inc_Hal19 <- subset(Inc_Hal19, LANDING_MONTH > 8)

### HALIBUT PER SABLEFISH ON A SINGLE TRIP ###
Inc_Hal19 <- subset(Inc_Hal19, PACIFIC_HALIBUT > 0)
Inc_Hal19 <- subset(Inc_Hal19, SABLEFISH > 0)

### SET TRIP LIMITS BASED ON SABLEFISH LANDED (BY THE THOUSANDS) ROUND UP ONLY AT 900 LBS ###
Inc_Hal19$SF_1K <- round(Inc_Hal19$SABLEFISH, -3)
Inc_Hal19$SF <- ifelse(Inc_Hal19$SF_1K > Inc_Hal19$SABLEFISH + 100, Inc_Hal19$SF_1K - 1000, Inc_Hal19$SF_1K)
Inc_Hal19$SF <- ifelse(Inc_Hal19$SF == 0, Inc_Hal19$SABLEFISH, Inc_Hal19$SF)

####################################################################################################################################################################
##### TRIP LIMIT MODELING ALTERNATIVES #####

### STATUS QUO TRIP LIMIT 200 lb. DRESSED P. HALIBUT PER 1,000 lb. DRESSED SABLEFISH ###
Inc_Hal19$SQTL_2019 <- Inc_Hal19$SF / 5.0  + 30                               #200 per 1,000
Inc_Hal19$SQTL_2020 <- Inc_Hal19$SF / 5.0  + 30                               #200 per 1,000

### SET YOUR ALTERNATE TRIP LIMITS ###
Inc_Hal19$ALT_SQTL <- Inc_Hal19$SF / 4.0 + 30                                #250 per 1,000

### NEED TO SET CONTROL RULES THAT MANIPULATE LBS PER THE TRIP LIMITS ###
### HERE IS WHAT I USE, BUT CAN CHANGE IF WANTED....
# (1)If status quo lbs. were higher than new trip limit, then cap them off at new limit (works mainly for lower trip limits)
# (2)If status quo lbs. were within 80% of current limit, then assumed to be a target that would catch the new higher limit
# (3)If status quo lbs. were lower than 80% of current limit, then assumed to be incidental catches so left them alone with higher limits 

### NEW CALCULATION ###
Inc_Hal19[ ,"SQTL"] <- ifelse (Inc_Hal19$SF < 1000, Inc_Hal19$PACIFIC_HALIBUT, ifelse ((Inc_Hal19$PACIFIC_HALIBUT / Inc_Hal19$SQTL_2019 >= .80), Inc_Hal19$SQTL_2019, Inc_Hal19$PACIFIC_HALIBUT))
Inc_Hal19[ ,"ALT1"] <- ifelse (Inc_Hal19$SF < 1000, Inc_Hal19$PACIFIC_HALIBUT, ifelse ((Inc_Hal19$PACIFIC_HALIBUT / Inc_Hal19$SQTL_2019 >= .80), Inc_Hal19$ALT_SQTL, Inc_Hal19$PACIFIC_HALIBUT))

### SUM NEW TRIP LIMIT PROJECTIONS - ONLY FOR MONTH SEPTEMBER-OCTOBER ###
HALIBUT_Sept <- ddply (subset(Inc_Hal19, LANDING_MONTH == 9), c("LANDING_YEAR"), summarise, SQTL_200LBS = sum(SQTL, na.rm=T), SQTL_200LBS = sum(SQTL, na.rm=T)) 
HALIBUT_Oct <- ddply (subset(Inc_Hal19, LANDING_MONTH == 10), c("LANDING_YEAR"), summarise, SQTL_200LBS = sum(SQTL, na.rm=T), ALT1_250LBS = sum(ALT1, na.rm=T)) 
HALIBUT <- HALIBUT_Sept + HALIBUT_Oct

####################################################################################################################################################################
##### BRING IN ACTUAL 2020 DATA (APRIL - AUGUST) #####

### LOOK AT THE MEASURABLE USED FOR PROJECTIONS ###
Current_Month <- ddply (Hal_20, c("LANDING_MONTH"), summarise, SQTL = sum(PACIFIC_HALIBUT, na.rm=T))
Current_Month
Current_Year <- ddply (Hal_20, c("LANDING_MONTH"), summarise, SQTL = sum(PACIFIC_HALIBUT, na.rm=T))
Current_Year

### SUM THE TRIP LIMITS FOR 2020 ###
HALIBUT_II <- ddply (subset(Hal_20, LANDING_MONTH < 9), c("LANDING_YEAR"), summarise, SQTL = sum(PACIFIC_HALIBUT, na.rm=T))
HALIBUT_II

### COMBINE THE TWO SETS OF DATA (HALIBUT & HALIBUT_II)
HALIBUT <- HALIBUT[c(2:3)] + HALIBUT_II$SQTL
HALIBUT

### CONVERT TO NET POUNDS (PACIFIC HALIBUT IS MANAGED BY NET POUNDS) ###
NET_LBS <- HALIBUT[c(1:2)]*0.88
NET_LBS

####################################################################################################################################################################
##### TRIP LIMIT MODEL RESULTS - STILL WAITING FOR THE MONTH OF MAY TO FINISH UP TO BE INCLUDED IN THE NUMBERS BELOW - EXPECT ANOTHER COUPLE THOUSAND OR SO #####

### THROUGH AUGUST DATA UPLOADED FROM PACFIN ON 9/9/20 ###
# SQTL_200LBS = 64,783
# ALT1_250LBS = 69,315


