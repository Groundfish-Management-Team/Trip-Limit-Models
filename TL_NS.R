####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: NEARSHORE ROCKFISH TRIP LIMIT MODEL
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
colnames(TIX)[14] <- c('SPECIES')
Species <- unique(TIX[c(14)])

#CREATE LE OA FIELD
TIX$SECTOR [TIX$DAHL_GROUNDFISH_CODE %in% c("05", "06")] <- "NS"
TIX <- na.omit(TIX)

#CREATE MAJOR AREAS FOR THE MAJOR NORTH OR SOUTH OF 4010 TRIP LIMIT AREAS
TIX$AREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE == "1C"] <- "N4010"
TIX$AREA [TIX$AGENCY_CODE %in% c("O","W")] <- "N4010"
TIX$AREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE != "1C"] <- "S4010"
TIX$SUBAREA <- "N3427"
TIX$SUBAREA [TIX$REGION_NAME %in% c("SOUTH CALIFORNIA") & TIX$SUBREGION_NAME != "SAN LUIS OBISPO"] <- "S3427"

#COMPLETE CURRENT YEAR (2019) BY RATIO OF POUNDS LANDED PER DECEMBER DAY
DEC_DAYS <- subset(TIX, PACFIN_YEAR == 2019 & LANDING_MONTH == 12)
DEC_DAYS$LANDING_DAY <- as.numeric(DEC_DAYS$LANDING_DAY)
DEC_DAYS <- max(DEC_DAYS$LANDING_DAY)
TIX$ROUND_WEIGHT_LBS <- ifelse(TIX$PACFIN_YEAR == 2019 & TIX$LANDING_MONTH == 12, TIX$ROUND_WEIGHT_LBS*31/DEC_DAYS, TIX$ROUND_WEIGHT_LBS)
TIX$EXVESSEL_REVENUE <- ifelse(TIX$PACFIN_YEAR == 2019 & TIX$LANDING_MONTH == 12, TIX$EXVESSEL_REVENUE*31/DEC_DAYS, TIX$EXVESSEL_REVENUE)

#SET YOUR TRIP LIMIT SPECIES...NEED A LIST? USE: unique(TIX$SPECIES)
CAL_SH <- subset(TIX, SPECIES == "CALIFORNIA SHEEPHEAD" & ROUND_WEIGHT_LBS < 110)
TIX <- subset(TIX, SPECIES == "BLACK ROCKFISH" | SPECIES == "BLUE ROCKFISH" | SPECIES == "DEACON ROCKFISH" | SPECIES == "BOCACCIO" | SPECIES == "CABEZON" |
  SPECIES == "KELP GREENLING" | SPECIES == "LINGCOD" | SPECIES == "BLACK AND YELLOW ROCKFISH" | SPECIES == "BLACK+BLUE ROCKFISH" | SPECIES == "CHINA ROCKFISH" |
  SPECIES == "GOPHER ROCKFISH" | SPECIES == "UNSP. GOPHER ROCKFISH" | SPECIES == "GRASS ROCKFISH" | SPECIES == "KELP ROCKFISH" | SPECIES == "OLIVE ROCKFISH" |
  SPECIES == "COPPER ROCKFISH" | SPECIES == "QUILLBACK ROCKFISH" | SPECIES == "CALICO ROCKFISH" | SPECIES == "CANARY ROCKFISH" | SPECIES == "BROWN ROCKFISH" |
  SPECIES == "UNSP. NEAR-SHORE ROCKFISH" | SPECIES == "NOR. UNSP. NEAR-SHORE ROCKFISH" | SPECIES == "YELLOWEYE ROCKFISH" | SPECIES == "UNSP. DEEP NEAR-SHORE RF" |
  SPECIES == "UNSP. ROCKFISH" | SPECIES == "TREEFISH" | SPECIES == "CALIFORNIA SCORPIONFISH")
TIX2 <- rbind(TIX, CAL_SH)
TIX2$SPECIES <- as.character(TIX2$SPECIES)
TIX2 <- subset(TIX2, PACFIN_YEAR == 2019)

#SET YOUR SPECIES GROUPS
TIX2$SPPGROUP <- TIX2$SPECIES
TIX2$SPPGROUP [TIX2$SPECIES %in% c("BLACK AND YELLOW ROCKFISH", "BLUE ROCKFISH", "CHINA ROCKFISH", "GOPHER ROCKFISH", "UNSP. GOPHER ROCKFISH", "GRASS ROCKFISH",
  "KELP ROCKFISH", "OLIVE ROCKFISH", "COPPER ROCKFISH", "QUILLBACK ROCKFISH", "CALICO ROCKFISH", "BROWN ROCKFISH", "UNSP. NEAR-SHORE ROCKFISH",
  "NOR. UNSP. NEAR-SHORE ROCKFISH", "UNSP. DEEP NEAR-SHORE RF", "TREEFISH")] <- "NEARSHORE"
TIX2$SPPGROUP <- ifelse(TIX2$AGENCY_CODE == "O" & TIX2$SPECIES == "BLACK ROCKFISH" | TIX2$AGENCY_CODE == "O" & TIX2$SPECIES == "BLUE ROCKFISH" |
  TIX2$AGENCY_CODE == "O" & TIX2$SPECIES == "DEACON ROCKFISH" | TIX2$AGENCY_CODE == "O" & TIX2$SPECIES == "BLACK+BLUE ROCKFISH",
  "BLACK/BLUE/DEACON ROCKFISH", TIX2$SPPGROUP)
TIX2$SPPGROUP <- ifelse(TIX2$AGENCY_CODE == "O" & TIX2$SPECIES == "CABEZON" | TIX2$AGENCY_CODE == "O" & TIX2$SPECIES == "KELP GREENLING",
   "CABEZON/KELP GREENLING", TIX2$SPPGROUP)
TIX2$SPPGROUP <- ifelse(TIX2$AGENCY_CODE == "W" & TIX2$SPECIES == "CANARY ROCKFISH", "CANARY ROCKFISH WA", TIX2$SPPGROUP)

#SECTION 2: SUMMARIES
#TRIP LIMIT PROJECTION FOR THE "COMPLEX"
PROJECTION <- ddply(TIX2, c("AGENCY_CODE", "AREA", "SUBAREA", "SPECIES", "SPPGROUP"), summarise, P_SQMT = sum(ROUND_WEIGHT_LBS/2204.6, na.rm=T),
  P_EXVESSEL = sum(EXVESSEL_REVENUE, na.rm=T))

write.csv(PROJECTION, "C:/Users/heathch/Desktop/R Analysis/Data Output/Commercial Groundfish/Trip Limit Models/2019/NS_RF.csv", row.names = FALSE)