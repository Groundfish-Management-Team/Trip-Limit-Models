####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: SHELF ROCKFISH TRIP LIMIT MODEL
#OUTPUT: html_notebook
#AUTHOR: "GMT"

####################################################################################################################################################################
####################################################################################################################################################################

# SECTION 1: SET-UP
setwd("H:/My Documents/R/Data Input/Groundfish/Commercial_GMT/Onboard_Observer/2022")  # Change the directory path at your convenience
.libPaths("H:/My Documents/R/rpackages")
options(scipen = 999)
library(easypackages)
libraries ("dplyr", "plyr", "data.table", "ggplot2", "xlsx", "dtplyr", "tidyr", "reshape", "lubridate" ,"RODBC", "DBI", "keyring", "dbplyr", "odbc")

####################################################################################################################################################################
####################################################################################################################################################################
##### DOWNLOAD DATA #####

### ACCESS PACFIN DIRECTLY ###
#key_set(service = "PacFIN") #this will prompt you to enter your password in a pop-up box
con <- dbConnect(drv = odbc::odbc(), 
                 uid = "cheath", 
                 pwd = key_get(service = "PacFIN"), 
                 dsn = "PacFIN")
#FILTER WHAT YOU NEED
CompFT_tbl<-con %>% 
  tbl(in_schema("PACFIN_MARTS", "COMPREHENSIVE_FT"))
PacFIN <- CompFT_tbl %>% 
  filter( between(LANDING_YEAR, 2022, 2023),
          MANAGEMENT_GROUP_CODE == "GRND",
          NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("BRONZESPOTTED ROCKFISH", "BOCACCIO", "CHILIPEPPER ROCKFISH", "COWCOD ROCKFISH", "FLAG ROCKFISH", "FRECKLED ROCKFISH", 
          "GREENBLOTCHED ROCKFISH", "GREENSPOTTED ROCKFISH", "GREENSTRIPED ROCKFISH", "HALFBANDED ROCKFISH", "HARLEQUIN ROCKFISH", "HONEYCOMB ROCKFISH", "MEXICAN ROCKFISH", 
          "NOR. UNSP. SHELF ROCKFISH", "PINK ROCKFISH", "PYGMY ROCKFISH", "REDSTRIPE ROCKFISH", "ROSETHORN ROCKFISH", "ROSY ROCKFISH", "SILVERGRAY ROCKFISH", "SPECKLED ROCKFISH", 
          "SQUARESPOT ROCKFISH", "STARRY ROCKFISH", "STRIPETAIL ROCKFISH", "SWORDSPINE ROCKFISH", "TIGER ROCKFISH", "UNSP. SHELF ROCKFISH","VERMILION ROCKFISH"),
          DAHL_GROUNDFISH_CODE %in% c("05","06","07","08","09","10"),
          PARTICIPATION_GROUP_CODE == "C",
          COUNCIL_CODE == "P") %>%
  collect()

####################################################################################################################################################################
####################################################################################################################################################################
# SECTION 1: LABELING

#CREATE LE OA FIELD
PacFIN[ ,"TYPE"] <- ifelse(PacFIN$DAHL_GROUNDFISH_CODE == "05" | PacFIN$DAHL_GROUNDFISH_CODE == "07" | PacFIN$DAHL_GROUNDFISH_CODE == "09", "LE", "OA")
PacFIN[ ,"SECTOR"] <- ifelse(PacFIN$DAHL_GROUNDFISH_CODE == "07" | PacFIN$DAHL_GROUNDFISH_CODE == "08" | PacFIN$DAHL_GROUNDFISH_CODE == "09" | 
  PacFIN$DAHL_GROUNDFISH_CODE == "10", "NON_NS", ifelse(PacFIN$AGENCY_CODE == "O", "OR_NS", "CAL_NS"))

#CREATE AREAS AND SUBAREAS
PacFIN[ ,"AREA"] <- ifelse(PacFIN$AGENCY_CODE == "O" | PacFIN$AGENCY_CODE == "W", "N4010", ifelse(PacFIN$PACFIN_CATCH_AREA_CODE == "1C", "N4010","S4010"))
PacFIN[ ,"SUBAREA"] <- ifelse(PacFIN$AGENCY_CODE == "O" | PacFIN$AGENCY_CODE == "W", "N42", ifelse(PacFIN$PACFIN_CATCH_AREA_CODE == "1C", "4010_42","S4010"))

####################################################################################################################################################################
####################################################################################################################################################################
#SECTION 2: CLEAN-UP, FILTER, SET TRIP LIMITS

#AGGREGATE BY BOAT
PacFIN_LE <- ddply(subset(PacFIN, TYPE == "LE"), c("VESSEL_NUM", "LANDING_MONTH", "AREA", "TYPE", "SECTOR", "LANDING_YEAR"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
PacFIN_LE <- na.omit(PacFIN_LE)
PacFIN_OA <- ddply(subset(PacFIN, TYPE == "OA"), c("VESSEL_NUM", "LANDING_MONTH", "AREA", "TYPE", "SECTOR", "LANDING_YEAR"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
PacFIN_OA <- na.omit(PacFIN_OA)
colnames(PacFIN_OA)[2] <- c('LANDING_MONTH')

#SET LE TRIP LIMITS
LESQ = data.table(LANDING_MONTH=(1:12), TL_SQ=c(800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800))
LEA1 = data.table(LANDING_MONTH=(1:12), TL_A1=c(1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500))
LEALL <- merge(LESQ, LEA1, by="LANDING_MONTH", all=TRUE)
LEALL$TYPE <- "LE"

#SET OA TRIP LIMITS
OASQ = data.table(LANDING_MONTH=(1:12), TL_SQ=c(800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800))
OAA1 = data.table(LANDING_MONTH=(1:12), TL_A1=c(1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500))
OAALL <- merge(OASQ, OAA1, by="LANDING_MONTH", all=TRUE)
OAALL$TYPE <- "OA"

#BIND TRIP LIMITS AND THEN MERGE WITH THE AGGREGATED PacFIN
LE <- merge(PacFIN_LE, LEALL, by = c("LANDING_MONTH", "TYPE"))
OA <- merge(PacFIN_OA, OAALL, by = c("LANDING_MONTH", "TYPE"))
#Tix <- rbind(LE, OA)

####################################################################################################################################################################
####################################################################################################################################################################
#SECTION 3: ANALYSIS - COMPARE CURRENT YEAR TO PREVIOUS YEARS FOR PROJECTIONS




### REMOVE MONTHS ###
NS_LandedII <- subset(NS_Landed, MONTH < 9)
colnames(NS_LandedII)[c(7)] <- c("THRO_AUG_MT")

### AGGREGATE SPECIES BY AREA, DEPTH & CATCH ###
NS_ModelII <- aggregate(THRO_AUG_MT ~ YEAR + AREA + SPECIES,  data=NS_LandedII, FUN=sum) %>% arrange(YEAR, AREA, SPECIES)
NS_Model <- NS_ModelII %>% right_join(NS_Model, by=c("YEAR", "AREA", "SPECIES"))
NS_Model[is.na(NS_Model)] <- 0
NS_Model$RATIO <- NS_Model$THRO_AUG_MT / NS_Model$MT

### REFORMAT FOR EASE OF COMPARING ###
NS_Model_x <- subset(NS_Model, YEAR == x)
colnames(NS_Model_x)[c(4:6)] <- c("THRO_AUGx_MT", "MT_x", "RATIO_x")
NS_Model_x <- NS_Model_x[c(2:6)]
NS_Model_y <- subset(NS_Model, YEAR == y)
colnames(NS_Model_y)[c(4:6)] <- c("THRO_AUGy_MT", "MT_y", "RATIO_y")
NS_Model_y <- NS_Model_y[c(2:6)]
NS_Model_z <- subset(NS_Model, YEAR == z)
colnames(NS_Model_z)[c(4:5)] <- c("THRO_AUGz_MT", "MT_z")
NS_Model_z <- NS_Model_z[c(2:5)]

### BRING BACK TOGETHER
NS_Model <- NS_Model_x %>% right_join(NS_Model_y, by=c("AREA", "SPECIES"))
NS_Model <- NS_Model %>% right_join(NS_Model_z, by=c("AREA", "SPECIES"))
NS_Model[is.na(NS_Model)] <- 0

### COMPARE / WHICH EVER YEAR (OF THE LAST TWO) IS MORE REFLECTIVE OF CURRENT YEAR, USE THAT YEAR ###
NS_Model$Dif_x <- ifelse(NS_Model$THRO_AUGz_MT > NS_Model$THRO_AUGx_MT, NS_Model$THRO_AUGz_MT - NS_Model$THRO_AUGx_MT,
  NS_Model$THRO_AUGx_MT - NS_Model$THRO_AUGz_MT)
NS_Model$Dif_y <- ifelse(NS_Model$THRO_AUGz_MT > NS_Model$THRO_AUGy_MT, NS_Model$THRO_AUGz_MT - NS_Model$THRO_AUGy_MT,
  NS_Model$THRO_AUGy_MT - NS_Model$THRO_AUGz_MT)
NS_Model$RATIO <- ifelse(NS_Model$Dif_y > NS_Model$Dif_x, NS_Model$RATIO_x, NS_Model$RATIO_y)

### NEW YEAR ESTIMATES BASED ON PREVIOUS YEAR (EITHER OF LAST TWO YEARS, SEE ABOVE) RATIOS THROUGH AUGUST ###
NS_Model$ESTIMATED_MT = NS_Model$THRO_AUGz_MT / NS_Model$RATIO
NS_Estimates <- NS_Model[c(1:2,10,14)]
NS_Estimates$ESTIMATED_MT <- ifelse(NS_Estimates$MT_z > NS_Model$ESTIMATED_MT, NS_Estimates$MT_z, NS_Estimates$ESTIMATED_MT)







#FILTERS FOR DIFFERENT TRIP LIMIT SPECIFICATIONS (TIME & SPACE)
PacFIN_LE <- subset(PacFIN_LE, LANDING_YEAR == 2021 & LANDING_MONTH > 10 & AREA == "N4010" | LANDING_YEAR == 2022 & LANDING_MONTH < 11 & AREA == "N4010")
PacFIN_OA <- subset(PacFIN_OA, LANDING_YEAR == 2021 & LANDING_MONTH > 10 & AREA == "N4010" | LANDING_YEAR == 2022 & LANDING_MONTH < 11 & AREA == "N4010")







####################################################################################################################################################################
####################################################################################################################################################################
#SECTION 4: KNOBS AND DIALS TO MANIPULATE THE DATA (AS THAT IS WHAT TRIP LIMITS DO) AND SUMMARIES

### FOR FULL YEAR ###

# (1)if catch is higher than new lower TL, then cap them off at new lower TL
# (2)if catch is 90%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# (3)if sq lbs lower than 90% of current limit, then assumed to be incidental catches so left them alone with higher limits
#Tix[ ,"A1LBS"]<- ifelse(Tix$tlbs >= Tix$TL_A1,Tix$tlbs,ifelse((Tix$tlbs >= Tix$TL_A1 | Tix$tlbs / Tix$TL_SQ >= .54), Tix$TL_A1, Tix$tlbs))
LE[ ,"A1LBS"]<- ifelse(LE$tlbs >= LE$TL_A1,LE$tlbs,ifelse((LE$tlbs >= LE$TL_A1 | LE$tlbs / LE$TL_SQ >= .9), LE$TL_A1, LE$tlbs))
OA[ ,"A1LBS"]<- ifelse(OA$tlbs >= OA$TL_A1,OA$tlbs,ifelse((OA$tlbs >= OA$TL_A1 | OA$tlbs / OA$TL_SQ >= .54), OA$TL_A1, OA$tlbs))

#SET OA AT 54% (800/1500) AND LE AT 90% AS THERE IS NOT EXPECTED TO BE MUCH CHANGE...

#SUMMARISE DATA
#Tix2 <- ddply (Tix, c("TYPE"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T))
LE2 <- ddply (LE, c("TYPE"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T))
OA2 <- ddply (OA, c("TYPE"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T))
Tix2 <- rbind(LE2, OA2)

#write.csv(Tix2, "U:/Fishery Management/GMT_Models/Trip Limit Model/Shelf/ShelfRF_10272022.csv", row.names = FALSE)

####################################################################################################################################################################
####################################################################################################################################################################