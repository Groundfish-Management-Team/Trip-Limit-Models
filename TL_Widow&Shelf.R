####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: WIDOW & SHELF TRIP LIMIT MODEL
#OUTPUT: html_notebook
#AUTHOR: "GMT"

####################################################################################################################################################################
####################################################################################################################################################################

# SECTION 1: SET-UP
setwd("C:/Users/heathch/Desktop/R Analysis/Data Input/Commercial Groundfish/Trip Limit Model")  # Change the directory path at your convenience
.libPaths("C:/Users/heathch/Desktop/R Analysis/rpackages")
options(scipen = 999)
library(easypackages)
libraries ("dplyr", "plyr","data.table", "ggplot2", "xlsx", "ROracle", "dtplyr", "tidyr", "lubridate")

#LOAD COMMON TICKET FILE FROM PACFIN
TIX<-read.csv("TL_Universal.csv")

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
TIX$SPPGROUP [TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("SHORTBELLY ROCKFISH", "BOCACCIO", "WIDOW ROCKFISH", "CHILIPEPPER ROCKFISH", "COWCOD")] <- "WIDOW&SHELF"
TIX$SPPGROUP2 <- NA
TIX$SPPGROUP2 [TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("SHORTBELLY ROCKFISH", "BOCACCIO", "CHILIPEPPER ROCKFISH", "COWCOD")] <- "SHELF"
TIX$SPPGROUP3 <- NA
TIX$SPPGROUP3 [TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("WIDOW ROCKFISH")] <- "WIDOW"

#COMPLETE CURRENT YEAR (2019) BY RATIO OF POUNDS LANDED PER DECEMBER DAY
TIX$ROUND_WEIGHT_LBS <- ifelse(TIX$PACFIN_YEAR == 2019 & TIX$LANDING_MONTH == 12, TIX$ROUND_WEIGHT_LBS*31/8, TIX$ROUND_WEIGHT_LBS)

# SECTION 2: CLEAN-UP, FILTER, SET TRIP LIMITS
#aggregate by boat and other stuff
TIX2 <- ddply(TIX, c("VESSEL_NUM", "LANDING_MONTH", "AREA", "TYPE", "PACFIN_YEAR", "SPPGROUP"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
TIX2b <- ddply(TIX, c("VESSEL_NUM", "LANDING_MONTH", "AREA", "TYPE", "PACFIN_YEAR", "SPPGROUP2"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
colnames(TIX2b)[6] <- "SPPGROUP"
TIX2c <- ddply(TIX, c("VESSEL_NUM", "LANDING_MONTH", "AREA", "TYPE", "PACFIN_YEAR", "SPPGROUP3"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
colnames(TIX2c)[6] <- "SPPGROUP"
TIX2 <- rbind(TIX2, TIX2b, TIX2c)
TIX2 <- na.omit(TIX2)

#FILTERS FOR DIFFERENT TRIP LIMIT MODELING SPECIES / SPECIES GROUPS
TIX3 <- subset(TIX2, PACFIN_YEAR == 2019)

#SET LE TRIP LIMITS (MONTHLY AND ALWAYS MAKE SURE HIGHER THAN OA)
LESQ = data.table(LANDING_MONTH=(1:12), TL_SQ=c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200))
LEA1a = data.table(LANDING_MONTH=(1:12), TL_A1a=c(4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000, 4000))
LEA1b = data.table(LANDING_MONTH=(1:12), TL_A1b=c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200))
LEALL <- merge(merge(LESQ, LEA1a ,by="LANDING_MONTH", all=TRUE), LEA1b,by="LANDING_MONTH", all=TRUE)
LEALL$TYPE <- "LE"

#SET OA TRIP LIMITS
OASQ = data.table(LANDING_MONTH=(1:12), TL_SQ=c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200))
OAA1a = data.table(LANDING_MONTH=(1:12), TL_A1a=c(2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000))
OAA1b = data.table(LANDING_MONTH=(1:12), TL_A1b=c(200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200))
OAALL <- merge(merge(OASQ, OAA1a ,by="LANDING_MONTH", all=TRUE), OAA1b,by="LANDING_MONTH", all=TRUE)
OAALL$TYPE <- "OA"

#BIND TRIP LIMITS AND THEN MERGE WITH THE AGGREGATED TIX
LEOA <- rbind(LEALL, OAALL)
TIX4 <- merge(TIX3, LEOA, by = c("LANDING_MONTH", "TYPE"))

#SECTION 3: KNOBS AND DIALS TO MANIPULATE THE DATA (AS THAT IS WHAT TRIP LIMITS DO)
# (1)if catch is higher than new lower TL, then cap them off at new lower TL
# (2)if catch is 90%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# (3)if sq lbs lower than 90% of current limit, then assumed to be incidental catches so left them alone with higher limits
TIX4[ ,"A1aLBS"] <- ifelse(TIX4$SPPGROUP == "WIDOW", ifelse(TIX4$tlbs >= TIX4$TL_A1a,TIX4$tlbs,
  ifelse((TIX4$tlbs >= TIX4$TL_A1a | TIX4$tlbs / TIX4$TL_SQ >= .9), TIX4$TL_A1a, TIX4$tlbs)), 0)
TIX4[ ,"A1bLBS"] <- ifelse(TIX4$SPPGROUP == "SHELF", ifelse(TIX4$tlbs >= TIX4$TL_A1b,TIX4$tlbs,
  ifelse((TIX4$tlbs >= TIX4$TL_A1b | TIX4$tlbs / TIX4$TL_SQ >= .9), TIX4$TL_A1b, TIX4$tlbs)), 0)
TIX4[ ,"SQ_Shelf"] <- ifelse(TIX4$SPPGROUP == "WIDOW&SHELF", ifelse(TIX4$tlbs >= TIX4$TL_SQ, TIX4$TL_SQ, TIX4$tlbs), 0)
TIX4[ ,"SQ_Widow"] <- ifelse(TIX4$SPPGROUP == "WIDOW", TIX4$tlbs, 0)

#SECTION 4: SUMMARIES
#TRIP LIMIT PROJECTION FOR THE "COMPLEX"
PROJECTION <- ddply (TIX4, c("TYPE", "PACFIN_YEAR", "AREA"), summarise, P_SQMT_Widow = sum(SQ_Widow /2204.6, na.rm=T),
  P_SQMT_Shelf = sum(SQ_Shelf /2204.6, na.rm=T),P_WIDOW_MT = sum(A1aLBS / 2204.6, na.rm=T), P_SHELF_MT = sum(A1bLBS / 2204.6, na.rm=T))

write.csv(PROJECTION, "C:/Users/heathch/Desktop/R Analysis/Data Output from R/Commercial Groundfish/Trip Limit Models/2019/WIDOW&SHELF.csv", row.names = FALSE)