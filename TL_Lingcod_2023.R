####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: LINGCOD TRIP LIMIT MODEL
#OUTPUT: html_notebook
#AUTHOR: "GMT"

####################################################################################################################################################################
####################################################################################################################################################################

# SECTION 1: SET-UP
setwd("H:/My Documents/R/Data Input/Groundfish/Commercial_GMT/Onboard_Observer/2023")  # Change the directory path at your convenience
.libPaths("H:/My Documents/R/rpackages")
options(scipen = 999)
library(easypackages)
libraries ("dplyr", "plyr", "data.table", "ggplot2", "xlsx", "dtplyr", "tidyr", "reshape", "lubridate" ,"RODBC", "DBI", "keyring", "dbplyr", "odbc")

####################################################################################################################################################################
####################################################################################################################################################################
##### DOWNLOAD DATA #####

### ACCESS PACFIN DIRECTLY ###
#key_set(service = "PacFIN") #this will prompt you to enter your password in a pop-up box
#con <- dbConnect(drv = odbc::odbc(), 
#                     uid = "cheath", 
#                     pwd = key_get(service = "PacFIN"), 
#                     dsn = "PacFIN")
#FILTER WHAT YOU NEED
#CompFT_tbl<-con %>% 
#  tbl(in_schema("PACFIN_MARTS", "COMPREHENSIVE_FT"))
#PacFIN <- CompFT_tbl %>% 
#  filter( between(LANDING_YEAR,2021,2022),
#          MANAGEMENT_GROUP_CODE=="GRND",
#          NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME == "LINGCOD",
#          DAHL_GROUNDFISH_CODE%in% c("05","06","07","08","09","10"),
#          PARTICIPATION_GROUP_CODE == "C",
#          COUNCIL_CODE == "P") %>%
#  collect()

PacFIN2 <- read.table("Lingcod/2023/lingcod.csv", 
  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
PacFIN<-PacFIN2%>%
  filter(MANAGEMENT_GROUP_CODE=="GRND",
                   DAHL_GROUNDFISH_CODE%in% c("5","6","7","8","9","10"))%>%
  collect()
####################################################################################################################################################################
####################################################################################################################################################################
# SECTION 1: LABELING

#CREATE PERIOD FIELD
PacFIN$PERIOD <- ceiling(PacFIN$LANDING_MONTH/2)

#CREATE LE OA FIELD
PacFIN[ ,"TYPE"] <- ifelse(PacFIN$DAHL_GROUNDFISH_CODE == "5" | PacFIN$DAHL_GROUNDFISH_CODE == "7" | PacFIN$DAHL_GROUNDFISH_CODE == "9", "LE", "OA")
PacFIN[ ,"SECTOR"] <- ifelse(PacFIN$DAHL_GROUNDFISH_CODE == "7" | PacFIN$DAHL_GROUNDFISH_CODE == "8" | PacFIN$DAHL_GROUNDFISH_CODE == "9" | 
  PacFIN$DAHL_GROUNDFISH_CODE == "10", "NON_NS", ifelse(PacFIN$AGENCY_CODE == "O", "OR_NS", "CAL_NS"))

#CREATE AREAS AND SUBAREAS
PacFIN[ ,"AREA"] <- ifelse(PacFIN$AGENCY_CODE == "O" | PacFIN$AGENCY_CODE == "W", "N4010", ifelse(PacFIN$PACFIN_CATCH_AREA_CODE == "1C", "N4010","S4010"))
PacFIN[ ,"SUBAREA"] <- ifelse(PacFIN$AGENCY_CODE == "O" | PacFIN$AGENCY_CODE == "W", "N42", ifelse(PacFIN$PACFIN_CATCH_AREA_CODE == "1C", "4010_42","S4010"))

#SET YOUR TRIP LIMIT SPECIES...NEED A LIST? USE: unique(PacFIN$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME)
PacFIN[ ,"SPPGROUP"] <- ifelse(PacFIN$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME == "LINGCOD", "LINGCOD", "")

####################################################################################################################################################################
####################################################################################################################################################################
#SECTION 2: CLEAN-UP, FILTER, SET TRIP LIMITS

#AGGREGATE BY BOAT / ETC.
PacFIN_LE <- ddply(subset(PacFIN, TYPE == "LE"), c("VESSEL_NUM", "PERIOD", "SUBAREA", "TYPE", "SECTOR", "LANDING_YEAR", "SPPGROUP"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
PacFIN_LE <- na.omit(PacFIN_LE)
PacFIN_OA <- ddply(subset(PacFIN, TYPE == "OA"), c("VESSEL_NUM", "LANDING_MONTH", "SUBAREA", "TYPE", "SECTOR", "LANDING_YEAR", "SPPGROUP"),
  summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(ROUND_WEIGHT_LBS, na.rm=T))
PacFIN_OA <- na.omit(PacFIN_OA)
colnames(PacFIN_OA)[2] <- c('PERIOD')

#FILTERS FOR DIFFERENT TRIP LIMIT MODELING SPECIES / SPECIES GROUPS
PacFIN_LE <- subset(PacFIN_LE, LANDING_YEAR == 2022 & PERIOD > 4 & SUBAREA == "N42" | LANDING_YEAR == 2023 & PERIOD < 5 & SUBAREA == "N42")
PacFIN_OA <- subset(PacFIN_OA, LANDING_YEAR == 2022 & PERIOD > 9 & SUBAREA == "N42" | LANDING_YEAR == 2023 & PERIOD < 10 & SUBAREA == "N42")

#SET LE TRIP LIMITS (MONTHLY NOW, BUT DOING BIMO SO X2 AND ALWAYS MAKE SURE HIGHER THAN OA)
LESQ = data.table(PERIOD=(1:6), TL_SQ=c(7000, 7000, 7000, 7000, 7000, 7000))
LEA1 = data.table(PERIOD=(1:6), TL_A1=c(9000, 9000, 9000, 9000, 9000, 9000))
LEA2 = data.table(PERIOD=(1:6), TL_A2=c(11000, 11000, 11000, 11000, 11000, 11000))
LEA3 = data.table(PERIOD=(1:6), TL_A3=c(14000, 14000, 14000, 14000, 14000, 14000))
#LEALL <- merge(LESQ, LEA1, by="PERIOD", all=TRUE)
#LEALL <- merge(merge(LESQ, LEA1, by="PERIOD", all=TRUE), LEA2, by="PERIOD", all=TRUE)
LEALL <- merge(merge(merge(LESQ, LEA1, by="PERIOD", all=TRUE), LEA2, by="PERIOD", all=TRUE), LEA3, by="PERIOD", all=TRUE)
LEALL$TYPE <- "LE"

#SET OA TRIP LIMITS
OASQ = data.table(PERIOD=(1:12), TL_SQ=c(3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500))
OAA1 = data.table(PERIOD=(1:12), TL_A1=c(4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500, 4500))
OAA2 = data.table(PERIOD=(1:12), TL_A2=c(5500, 5500, 5500, 5500, 5500, 5500, 5500, 5500, 5500, 5500, 5500, 5500))
OAA3 = data.table(PERIOD=(1:12), TL_A3=c(7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000, 7000))
#OAALL <- merge(OASQ, OAA1, by="PERIOD", all=TRUE)
#OAALL <- merge(merge(OASQ, OAA1, by="PERIOD", all=TRUE), OAA2, by="PERIOD", all=TRUE)
OAALL <- merge(merge(merge(OASQ, OAA1, by="PERIOD", all=TRUE), OAA2, by="PERIOD", all=TRUE), OAA3, by="PERIOD", all=TRUE)
OAALL$TYPE <- "OA"

#BIND TRIP LIMITS AND THEN MERGE WITH THE AGGREGATED PacFIN
PacFIN_LE <- merge(PacFIN_LE, LEALL, by = c("PERIOD", "TYPE"))
PacFIN_OA <- merge(PacFIN_OA, OAALL, by = c("PERIOD", "TYPE"))
Tix <- rbind(PacFIN_LE, PacFIN_OA)

####################################################################################################################################################################
####################################################################################################################################################################
#SECTION 3: KNOBS AND DIALS TO MANIPULATE THE DATA (AS THAT IS WHAT TRIP LIMITS DO) AND SUMMARIES

# (1)if catch is higher than new lower TL, then cap them off at new lower TL
# (2)if catch is 90%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# (3)if sq lbs lower than 90% of current limit, then assumed to be incidental catches so left them alone with higher limits
Tix[ ,"A1LBS"]<- ifelse(Tix$tlbs >= Tix$TL_A1,Tix$tlbs,ifelse((Tix$tlbs >= Tix$TL_A1 | Tix$tlbs / Tix$TL_SQ >= .8), Tix$TL_A1, Tix$tlbs))
Tix[ ,"A2LBS"]<- ifelse(Tix$tlbs >= Tix$TL_A2,Tix$tlbs,ifelse((Tix$tlbs >= Tix$TL_A2 | Tix$tlbs / Tix$TL_SQ >= .8), Tix$TL_A2, Tix$tlbs))
Tix[ ,"A3LBS"]<- ifelse(Tix$tlbs >= Tix$TL_A3,Tix$tlbs,ifelse((Tix$tlbs >= Tix$TL_A3 | Tix$tlbs / Tix$TL_SQ >= .8), Tix$TL_A3, Tix$tlbs))

### FOR FULL YEAR / SUMMARIZE DATA ###
#Tix2 <- ddply (Tix, c("TYPE"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T))
Tix2 <- ddply (Tix, c("TYPE"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T), A2_MT = sum(A2LBS / 2204.6, na.rm=T),A3_MT = sum(A3LBS / 2204.6, na.rm=T))

### FOR INSEASON CHANGES ###
#TRIP LIMIT PROJECTION FOR INSEASON ACTION - KEEP CURRENT YEAR TO DATE POUNDS AS IS AND BORROW SQ FROM PRIOR YEAR IF NECCESSARY - PROJECT ONLY FOR REMAINDER OF YEAR!
#INSEASON CHANGE ANTICIPATING IMPLIMENTATION ON NOVEMBER 1 (LE PERIOD 6 & OA PERIOD 11)
#Tix[ ,"INS_A1LBS"] <- ifelse(Tix$TYPE == "LE" & Tix$PERIOD > 4 |  Tix$TYPE == "OA" & Tix$PERIOD > 9, Tix$A1LBS, Tix$tlbs)
#Tix[ ,"INS_A2LBS"] <- ifelse(Tix$TYPE == "LE" & Tix$PERIOD > 4 |  Tix$TYPE == "OA" & Tix$PERIOD > 9, Tix$A2LBS, Tix$tlbs)
#Tix[ ,"INS_A3LBS"] <- ifelse(Tix$TYPE == "LE" & Tix$PERIOD > 4 |  Tix$TYPE == "OA" & Tix$PERIOD > 9, Tix$A3LBS, Tix$tlbs)

#SUMMARISE DATA / P=Partial Year / INS = Remaining Year Projection
#Tix2 <- ddply (Tix, c("TYPE"), summarise, P_SQ_MT = sum(tlbs /2204.6, na.rm=T), P_A1_MT = sum(A1LBS / 2204.6, na.rm=T), INS_A1_MT = sum(INS_A1LBS / 2204.6, na.rm=T))
#Tix2 <- ddply (Tix, c("TYPE", "SECTOR"), summarise, P_SQ_MT = sum(tlbs /2204.6, na.rm=T),P_A1_MT = sum(A1LBS / 2204.6, na.rm=T), 
#  P_A2_MT = sum(A2LBS / 2204.6, na.rm=T), INS_A1_MT = sum(INS_A1LBS / 2204.6, na.rm=T), INS_A2_MT = sum(INS_A2LBS / 2204.6, na.rm=T))
#Tix2 <- ddply (Tix, c("TYPE"), summarise, P_SQ_MT = sum(tlbs /2204.6, na.rm=T),P_A1_MT = sum(A1LBS / 2204.6, na.rm=T), P_A2_MT = sum(A2LBS / 2204.6, na.rm=T), 
#  INS_A1_MT = sum(INS_A1LBS / 2204.6, na.rm=T), INS_A2_MT = sum(INS_A2LBS / 2204.6, na.rm=T))
#Tix2 <- ddply (Tix, c("TYPE"), summarise, P_SQ_MT = sum(tlbs /2204.6, na.rm=T), P_A1_MT = sum(A1LBS / 2204.6, na.rm=T), P_A2_MT = sum(A2LBS / 2204.6, na.rm=T),
# P_A3_MT = sum(A3LBS / 2204.6, na.rm=T), INS_A1_MT = sum(INS_A1LBS / 2204.6, na.rm=T), INS_A2_MT = sum(INS_A2LBS / 2204.6, na.rm=T), INS_A3_MT = sum(INS_A3LBS / 2204.6, na.rm=T))

write.csv(Tix2, "Lingcod/2023/Lingcod_Nov_1020.csv", row.names = FALSE)
#write.csv(Tix2, "C:/Users/heathch/Desktop/R Analysis/Data Output/Commercial Groundfish/Trip Limit Models/2021/N42_Lingcod.csv", row.names = FALSE)

####################################################################################################################################################################
####################################################################################################################################################################
#SECTION 4: TACK ON THE YELLOWEYE ROCKFISH IMPACTS (FOR LINGCOD)

#UPDATE WTIH MOST RECENT NUMBERS BASED OFF OF THE NEARSHORE MODEL
#SHOW DIFFERENCE IN YEYE FOR ACTUAL (WITH OLD DMRS) AND PROJECTED USING NEW DMRS

#(1) NEED TO PARTITION LINGCOD TO EACH DEPTH MODEL DEPTH BIN: USING NEARSHORE MODEL COLUMNS V-Y (HIGHLIGHTED IN ORANGE)
LOR10 = 0.2654
LOR20 = 0.6400
LOR30 = 0.0897
LOR31 = 0.0049
Lncal10 = 0.2109
Lncal20 = 0.5837
Lncal30 = 0.2033
Lncal31 = 0.0021

#(2) PAIR  WITH THE DISCARD RATIOS OF YELLOWEYE ROCKFISH: USING NEARSHORE MODEL COLUMNS M-P (HIGHLIGHTED IN ORANGE)
YOR10 = 0.003
YOR20 = 0.016
YOR30 = 0.067
YOR31 = 0.005
Yncal10 = 0.001
Yncal20 = 0.008
Yncal30 = 0.070
Yncal31 = 0.045

#(3) ADD ON THE YEYE DMRS RATES
#BOTH = .28; .45; .67; 1.00

#(4) formula is, for each depth, == mt of total ling * % to that depth bin a * discard ratio for that bin * YEYE DMR in depth bin
Tix2[ ,"SQ_YEYE_MT"] <- (Tix2$SQ_MT * LOR10 * YOR10 * 0.28) + (Tix2$SQ_MT * LOR20 * YOR20 * 0.45) + (Tix2$SQ_MT * LOR30 * YOR30 * 0.67) + (Tix2$SQ_MT * LOR31 * YOR31 * 1.00)
Tix2[ ,"ALT1_YEYE_MT"] <- (Tix2$A1_MT * LOR10 * YOR10 * 0.28) + (Tix2$A1_MT * LOR20 * YOR20 * 0.45) + (Tix2$A1_MT * LOR30 * YOR30 * 0.67) + (Tix2$A1_MT * LOR31 * YOR31 * 1.00)
Tix2[ ,"ALT2_YEYE_MT"] <- (Tix2$A2_MT * LOR10 * YOR10 * 0.28) + (Tix2$A2_MT * LOR20 * YOR20 * 0.45) + (Tix2$A2_MT * LOR30 * YOR30 * 0.67) + (Tix2$A2_MT * LOR31 * YOR31 * 1.00)
Tix2[ ,"ALT3_YEYE_MT"] <- (Tix2$A3_MT * LOR10 * YOR10 * 0.28) + (Tix2$A3_MT * LOR20 * YOR20 * 0.45) + (Tix2$A3_MT * LOR30 * YOR30 * 0.67) + (Tix2$A3_MT * LOR31 * YOR31 * 1.00)

#Alternate version for when INSEASON changes are in place
#Tix2[ ,"SQ_YEYE_MT"] <- (Tix2$P_SQ_MT * LOR10 * YOR10 * 0.28) + (Tix2$P_SQ_MT * LOR20 * YOR20 * 0.45) + (Tix2$P_SQ_MT * LOR30 * YOR30 * 0.67) + (Tix2$P_SQ_MT * LOR31 * YOR31 * 1.00)
#Tix2[ ,"ALT1_YEYE_MT"] <- (Tix2$P_A1_MT * LOR10 * YOR10 * 0.28) + (Tix2$P_A1_MT * LOR20 * YOR20 * 0.45) + (Tix2$P_A1_MT * LOR30 * YOR30 * 0.67) + (Tix2$P_A1_MT * LOR31 * YOR31 * 1.00)
#Tix2[ ,"ALT2_YEYE_MT"] <- (Tix2$P_A2_MT * LOR10 * YOR10 * 0.28) + (Tix2$P_A2_MT * LOR20 * YOR20 * 0.45) + (Tix2$P_A2_MT * LOR30 * YOR30 * 0.67) + (Tix2$P_A2_MT * LOR31 * YOR31 * 1.00)
#Tix2[ ,"ALT3_YEYE_MT"] <- (Tix2$P_A3_MT * LOR10 * YOR10 * 0.28) + (Tix2$P_A3_MT * LOR20 * YOR20 * 0.45) + (Tix2$P_A3_MT * LOR30 * YOR30 * 0.67) + (Tix2$P_A3_MT * LOR31 * YOR31 * 1.00)
#Tix2[ ,"INS_ALT1_YEYE_MT"] <- (Tix2$INS_A1_MT * LOR10 * YOR10 * 0.28) + (Tix2$INS_A1_MT * LOR20 * YOR20 * 0.45) + (Tix2$INS_A1_MT * LOR30 * YOR30 * 0.67) + (Tix2$INS_A1_MT * LOR31 * YOR31 * 1.00)
#Tix2[ ,"INS_ALT2_YEYE_MT"] <- (Tix2$INS_A2_MT * LOR10 * YOR10 * 0.28) + (Tix2$INS_A2_MT * LOR20 * YOR20 * 0.45) + (Tix2$INS_A2_MT * LOR30 * YOR30 * 0.67) + (Tix2$INS_A2_MT * LOR31 * YOR31 * 1.00)
#Tix2[ ,"INS_ALT3_YEYE_MT"] <- (Tix2$INS_A3_MT * LOR10 * YOR10 * 0.28) + (Tix2$INS_A3_MT * LOR20 * YOR20 * 0.45) + (Tix2$INS_A3_MT * LOR30 * YOR30 * 0.67) + (Tix2$INS_A3_MT * LOR31 * YOR31 * 1.00)

#now add on the "extra yelloweye for alt 1 and alt2 and alt3
Tix2$Extra_YEYE_ALT1 <- Tix2$ALT1_YEYE_MT - Tix2$SQ_YEYE_MT
Tix2$Extra_YEYE_ALT2 <- Tix2$ALT2_YEYE_MT - Tix2$SQ_YEYE_MT
Tix2$Extra_YEYE_ALT3 <- Tix2$ALT3_YEYE_MT - Tix2$SQ_YEYE_MT
#Tix2$Extra_INS_YEYE_ALT1 <- Tix2$INS_ALT1_YEYE_MT - Tix2$SQ_YEYE_MT
#Tix2$Extra_INS_YEYE_ALT2 <- Tix2$INS_ALT2_YEYE_MT - Tix2$SQ_YEYE_MT
#Tix2$Extra_INS_YEYE_ALT3 <- Tix2$INS_ALT3_YEYE_MT - Tix2$SQ_YEYE_MT

write.csv(Tix2, "Lingcod/2023/lingcod_yeye_output_102023_80%.csv")
#write.csv(Tix2, "C:/Users/heathch/Desktop/R Analysis/Data Output/Groundfish/Commercial/Nearshore Model/2021/lingcod_yeye_output.csv")
