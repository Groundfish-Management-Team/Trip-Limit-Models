####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: YELLOWTAIL TRIP LIMIT MODEL
#OUTPUT: html_notebook
#AUTHOR: "GMT"

####################################################################################################################################################################
####################################################################################################################################################################

# SECTION 1: SET-UP
setwd("//Fwnew12/Data/Fishery Management/GMT_Models/Trip Limit Model")  # Change the directory path at your convenience
options(scipen = 999)
library(easypackages)
libraries ("dplyr", "plyr","data.table", "ggplot2", "dtplyr", "tidyr", "lubridate" ,"dbplyr", "odbc","DescTools")

#LOAD COMMON TICKET FILE FROM PACFIN
TIX<- read.csv("Yellowtail/2024/YTRK_N_ Landings.csv") 


#CREATE LE OA FIELD
TIX$TYPE <- NA
TIX$TYPE [TIX$DAHL_GROUNDFISH_CODE %in% c("5", "7", "9")] <- "LE"
TIX$TYPE [TIX$DAHL_GROUNDFISH_CODE %in% c("6", "8", "10")] <- "OA"
TIX$SECTOR [TIX$DAHL_GROUNDFISH_CODE %in% c("5", "6")] <- "NS"
TIX$SECTOR [TIX$DAHL_GROUNDFISH_CODE %in% c("7", "8", "9", "10")] <- "NON-NS"

#CREATE MAJOR AREAS FOR THE MAJOR NORTH OR SOUTH OF 4010 TRIP LIMIT AREAS
TIX$AREA <-NA
TIX$AREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE == "1C"] <- "N4010"
TIX$AREA [TIX$AGENCY_CODE %in% c("O","W")] <- "N4010"
TIX$AREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE != "1C"] <- "S4010"

#COMPLETE CURRENT YEAR (2019) BY RATIO OF POUNDS LANDED PER DECEMBER DAY
# DEC_DAYS <- subset(TIX, PACFIN_YEAR == 2019 & LANDING_MONTH == 12)
# DEC_DAYS$LANDING_DAY <- as.numeric(DEC_DAYS$LANDING_DAY)
# DEC_DAYS <- max(DEC_DAYS$LANDING_DAY)
# TIX$ROUND_WEIGHT_LBS <- ifelse(TIX$PACFIN_YEAR == 2019 & TIX$LANDING_MONTH == 12, TIX$ROUND_WEIGHT_LBS*31/DEC_DAYS, TIX$ROUND_WEIGHT_LBS)



#SET YOUR TRIP LIMIT SPECIES...NEED A LIST? USE: unique(TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME)
TIX$SPPGROUP <- NA
TIX$SPPGROUP [TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("YELLOWTAIL ROCKFISH")] <- "YT"

# SECTION 2: CLEAN-UP, FILTER, SET TRIP LIMITS
#aggregate by boat and other stuff
TIX2 <- ddply(TIX, c("VESSEL_NUM", "LANDING_MONTH", "AREA", "TYPE", "LANDING_YEAR", "SPPGROUP"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
TIX2 <- na.omit(TIX2)

#FILTERS FOR DIFFERENT TRIP LIMIT MODELING SPECIES / SPECIES GROUPS
TIX3 <- subset(TIX2, LANDING_YEAR == 2024 & LANDING_MONTH < 6 & AREA == "N4010" & SPPGROUP == "YT")
#TIX6<- rbind(LANDING_MONTH==1, LANDING_MONTH==2, LANDING_MONTH==3,LANDING_MONTH==4, LANDING_MONTH==5, LANDING_MONTH==5, LANDING_MONTH==5, LANDING_MONTH==4, LANDING_MONTH==3, LANDING_MONTH==2, LANDING_MONTH==1)



#SET LE TRIP LIMITS (MONTHLY AND ALWAYS MAKE SURE HIGHER THAN OA)
LESQ = data.table(LANDING_MONTH=(1:12), TL_SQ=c(3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000))
LEA1 = data.table(LANDING_MONTH=(1:12), TL_A1=c(4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500))
LEALL <- merge(LESQ,LEA1,by="LANDING_MONTH", all=TRUE)
LEALL$TYPE <- "LE"

#SET OA TRIP LIMITS
OASQ = data.table(LANDING_MONTH=(1:12), TL_SQ=c(1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500))
OAA1 = data.table(LANDING_MONTH=(1:12), TL_A1=c(2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500))
OAALL <- merge(OASQ,OAA1,by="LANDING_MONTH", all=TRUE)
OAALL$TYPE <- "OA"

#BIND TRIP LIMITS AND THEN MERGE WITH THE AGGREGATED TIX
LEOA <- rbind(LEALL, OAALL)
TIX4 <- merge(TIX3, LEOA, by = c("LANDING_MONTH", "TYPE"))

#SECTION 3: KNOBS AND DIALS TO MANIPULATE THE DATA (AS THAT IS WHAT TRIP LIMITS DO)
# (1)if catch is higher than new lower TL, then cap them off at new lower TL
# (2)if catch is 90%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# (3)if sq lbs lower than 90% of current limit, then assumed to be incidental catches so left them alone with higher limits
TIX4[ ,"A1LBS"]<- ifelse(TIX4$tlbs >= TIX4$TL_A1,TIX4$tlbs,ifelse((TIX4$tlbs >= TIX4$TL_A1 | TIX4$tlbs / TIX4$TL_SQ >= .7), TIX4$TL_A1, TIX4$tlbs))

#SECTION 4: SUMMARIES
#KJP 6/12 THIS PROJECTION ONLY INCLUDES THROUGH MAY, I THEN MANUALLY ADDED THE REST OF THE MONTHS BECAUSE OF THE CHANGES DUE TO QUILLBACK MITIGATION IN CA THAT HAPPENED IN 2024
#TRIP LIMIT PROJECTION FOR THE "COMPLEX"
PROJECTION <- ddply (TIX4, c("LANDING_MONTH", "TYPE"), summarise, P_SQMT = sum(tlbs /2204.6, na.rm=T),P_A1_MT = sum(A1LBS / 2204.6, na.rm=T))

write.csv(PROJECTION, "Yellowtail/2024/YTRK_N_PROJECTIONSoption1.csv", row.names = FALSE)
