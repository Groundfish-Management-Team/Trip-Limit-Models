####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: YELLOWTAIL TRIP LIMIT MODEL
#OUTPUT: html_notebook
#AUTHOR: "GMT"

####################################################################################################################################################################
####################################################################################################################################################################

# SECTION 1: SET-UP
setwd("C:/Users/heathch/Desktop/R Analysis/Data Input for R/Commercial Groundfish/Trip Limit Model")  # Change the directory path at your convenience
.libPaths("C:/Users/heathch/Desktop/R Analysis/rpackages")
options(scipen = 999)
library(easypackages)
libraries ("dplyr", "plyr","data.table", "ggplot2", "xlsx", "ROracle", "dtplyr", "tidyr", "lubridate")

#LOAD COMMON TICKET FILE FROM PACFIN
TIX<-read.csv("TL_Universal.csv")

#CREATE PERIOD FIELD
TIX$PERIOD <- ceiling(TIX$LANDING_MONTH/2)

#CREATE LE OA FIELD
TIX$TYPE <- NA
TIX$TYPE [TIX$DAHL_GROUNDFISH_CODE %in% c("05", "07", "09")] <- "LE"
TIX$TYPE [TIX$DAHL_GROUNDFISH_CODE %in% c("06", "08", "10")] <- "OA"
TIX$SECTOR [TIX$DAHL_GROUNDFISH_CODE %in% c("05", "06")] <- "NS"
TIX$SECTOR [TIX$DAHL_GROUNDFISH_CODE %in% c("07", "08", "09", "10")] <- "NON-NS"

#CREATE MAJOR AREAS FOR THE MAJOR NORTH OR SOUTH OF 4010 TRIP LIMIT AREAS
TIX$AREA <-NA
TIX$AREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE == "1C"] <- "N4010"
TIX$AREA [TIX$AGENCY_CODE %in% c("O","W")] <- "N4010"
TIX$AREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE != "1C"] <- "S4010"

#COMPLETE CURRENT YEAR (2019) BY RATIO OF POUNDS LANDED PER DECEMBER DAY
DEC_DAYS <- subset(TIX, PACFIN_YEAR == 2019 & LANDING_MONTH == 12)
DEC_DAYS$LANDING_DAY <- as.numeric(DEC_DAYS$LANDING_DAY)
DEC_DAYS <- max(DEC_DAYS$LANDING_DAY)
TIX$ROUND_WEIGHT_LBS <- ifelse(TIX$PACFIN_YEAR == 2019 & TIX$LANDING_MONTH == 12, TIX$ROUND_WEIGHT_LBS*31/DEC_DAYS, TIX$ROUND_WEIGHT_LBS)

#SET YOUR TRIP LIMIT SPECIES...NEED A LIST? USE: unique(TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME)
TIX$SPPGROUP <- NA
TIX$SPPGROUP [TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("PACIFIC OCEAN PERCH")] <- "POP"

# SECTION 2: CLEAN-UP, FILTER, SET TRIP LIMITS
#aggregate by boat and other stuff
TIX2 <- ddply(TIX, c("VESSEL_NUM", "PERIOD", "AREA", "TYPE", "PACFIN_YEAR", "SPPGROUP"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
TIX2 <- na.omit(TIX2)

#FILTERS FOR DIFFERENT TRIP LIMIT MODELING SPECIES / SPECIES GROUPS
TIX3 <- subset(TIX2, PACFIN_YEAR == 2019 & AREA == "N4010" & SPPGROUP == "POP")

#SET LE TRIP LIMITS (MONTHLY AND ALWAYS MAKE SURE HIGHER THAN OA)
LESQ = data.table(PERIOD=(1:12), TL_SQ=c(1800, 1800, 1800, 1800, 1800, 1800))
LEA1 = data.table(PERIOD=(1:12), TL_A1=c(3600, 3600, 3600, 3600, 3600, 3600))
LEALL <- merge(LESQ,LEA1,by="PERIOD", all=TRUE)
LEALL$TYPE <- "LE"

#BIND TRIP LIMITS AND THEN MERGE WITH THE AGGREGATED TIX
TIX4 <- merge(TIX3, LEALL, by = c("PERIOD", "TYPE"))

#SECTION 3: KNOBS AND DIALS TO MANIPULATE THE DATA (AS THAT IS WHAT TRIP LIMITS DO)
# (1)if catch is higher than new lower TL, then cap them off at new lower TL
# (2)if catch is 90%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# (3)if sq lbs lower than 90% of current limit, then assumed to be incidental catches so left them alone with higher limits
TIX4[ ,"A1LBS"]<- ifelse(TIX4$tlbs >= TIX4$TL_A1,TIX4$tlbs,ifelse((TIX4$tlbs >= TIX4$TL_A1 | TIX4$tlbs / TIX4$TL_SQ >= .9), TIX4$TL_A1, TIX4$tlbs))

#SECTION 4: SUMMARIES
#TRIP LIMIT PROJECTION FOR THE "COMPLEX"
PROJECTION <- ddply (TIX4, c("PACFIN_YEAR", "TYPE"), summarise, P_SQMT = sum(tlbs /2204.6, na.rm=T),P_A1_MT = sum(A1LBS / 2204.6, na.rm=T))

write.csv(PROJECTION, "C:/Users/heathch/Desktop/R Analysis/Data Output from R/Commercial Groundfish/Trip Limit Models/2019/N4010_POP.csv", row.names = FALSE)