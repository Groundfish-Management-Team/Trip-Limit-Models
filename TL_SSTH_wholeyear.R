####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: SHORTSPINE THORNYHEAD TRIP LIMIT MODEL
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
TIX<- read.csv("SSTH/2024/universal.csv")
#Create Period Field
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

#CREATE SUBAREA CODES FOR THE LESSER TRIP LIMIT AREAS (NEED MEL'S INPUT ON HOW TO DEFINE N/S 3427)
TIX$SUBAREA <- NA
TIX$SUBAREA [TIX$AGENCY_CODE %in% c("O","W")] <-"N42"
TIX$SUBAREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE %in% c("1C")] <- "4010-42"
TIX$SUBAREA [TIX$AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE %in% c("1B","1A")& TIX$ACL_CODE=="SSPN_N"] <- "3427-4010" 
TIX$SUBAREA [TIX$ACL_CODE=="SSPN_S"] <-"South of 3427"

#SET YOUR TRIP LIMIT SPECIES...NEED A LIST? USE: unique(TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME)
TIX$SPPGROUP <- NA
TIX$SPPGROUP [TIX$NOMINAL_TO_ACTUAL_PACFIN_SPECIES_NAME %in% c("SHORTSPINE THORNYHEAD")] <- "SPTH"

# SECTION 2: CLEAN-UP, FILTER, SET TRIP LIMITS
#aggregate by boat and other stuff
PacFIN_LE <- ddply(subset(TIX, TYPE == "LE"), c("VESSEL_NUM", "LANDING_MONTH","PERIOD", "SUBAREA","AREA", "TYPE", "SECTOR", "LANDING_YEAR", "SPPGROUP"),
                   summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(LANDED_WEIGHT_LBS, na.rm=T),price=exvessel/tlbs)
PacFIN_LE <- na.omit(PacFIN_LE)
PacFIN_OA <- ddply(subset(TIX, TYPE == "OA"), c("VESSEL_REGISTRATION_ID", "LANDING_MONTH", "SUBAREA","AREA", "TYPE", "SECTOR", "LANDING_YEAR", "SPPGROUP"),
                   summarise, exvessel = sum(EXVESSEL_REVENUE, na.rm=T), tlbs = sum(LANDED_WEIGHT_LBS, na.rm=T))
PacFIN_OA <- na.omit(PacFIN_OA)
colnames(PacFIN_OA)[2] <- c('PERIOD')

##Not for the trip limit model but for a price per pound comparison
price1<-PacFIN_LE %>%
  group_by(PERIOD, SUBAREA)%>%
  summarize(PRICE=mean(price))
#write.csv(PacFIN_LE,"SSTH/2024/price.csv")

#FILTERS FOR DIFFERENT TRIP LIMIT MODELING SPECIES / SPECIES GROUPS
PacFIN_LEN <- subset(PacFIN_LE, LANDING_YEAR == 2023 & AREA == "N4010")
PacFIN_LES<-subset(PacFIN_LE, LANDING_YEAR == 2023 & SUBAREA == "3427-4010")
PacFIN_LESS<-subset(PacFIN_LE, LANDING_YEAR == 2023 & SUBAREA=="South of 3427")
PacFIN_OAN <- subset(PacFIN_OA, LANDING_YEAR == 2023 & AREA == "N4010")
PacFIN_OAS <- subset(PacFIN_OA,LANDING_YEAR == 2023 & SUBAREA == "3427-4010")
PacFIN_OASS<- subset(PacFIN_OA,LANDING_YEAR == 2023 & SUBAREA=="South of 3427")

#SET LEN TRIP LIMITS
LENSQ = data.table(PERIOD=(1:6), TL_SQ=c(2000, 2000, 2000, 2500, 2500, 2500))
LENA1 = data.table(PERIOD=(1:6), TL_A1=c(3000, 3000, 3000, 3000, 3000, 3000))
LENA2 = data.table(PERIOD=(1:6), TL_A2=c(750, 750, 750, 750, 750, 750))
LENA3 = data.table(PERIOD=(1:6), TL_A3=c(350, 350, 350, 350, 350, 350))
LENALL <- merge(merge(merge(LENSQ, LENA1, by="PERIOD", all=TRUE), LENA2, by="PERIOD", all=TRUE), LENA3, by="PERIOD", all=TRUE)
LENALL$TYPE <- "LEN"

#SET LES TRIP LIMITS
LESSQ = data.table(PERIOD=(1:6), TL_SQ=c(2000, 2000, 2000, 2500, 2500, 2500))
LESA1 = data.table(PERIOD=(1:6), TL_A1=c(3000, 3000, 3000, 3000, 3000, 3000))
LESA2 = data.table(PERIOD=(1:6), TL_A2=c(750, 750, 750, 750, 750, 750))
LESA3 = data.table(PERIOD=(1:6), TL_A3=c(350, 350, 350, 350, 350, 350))
LESALL <- merge(merge(merge(LESSQ, LESA1, by="PERIOD", all=TRUE), LESA2, by="PERIOD", all=TRUE), LESA3, by="PERIOD", all=TRUE)
LESALL$TYPE <- "LES"

#SET LESS S OF 3427 TRIP LIMITS
LESSSQ = data.table(PERIOD=(1:6), TL_SQ=c(3000, 3000, 3000, 3000, 3000, 3000))
LESSA1 = data.table(PERIOD=(1:6), TL_A1=c(4000, 4000, 4000, 4000, 4000, 4000))
LESSALL <- merge(LESSSQ, LESSA1, by="PERIOD", all=TRUE)
LESSALL$TYPE <- "LESS"

#SET OAN TRIP LIMITS (MONTHLY NOW, BUT DOING BIMO SO X2 AND ALWAYS MAKE SURE HIGHER THAN OA)
#OANSQ = data.table(PERIOD=(1:12), TL_SQ=c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50))
#OANA1 = data.table(PERIOD=(1:12), TL_A1=c(25,25,25,25,25,25,25,25,25,25,25,25))
#OANA2 = data.table(PERIOD=(1:12), TL_A2=c(20,20,20,20,20,20,20,20,20,20,20,20))
#OANALL <- merge(merge(OANSQ, OANA1, by="PERIOD", all=TRUE),OANA2, by="PERIOD", all=TRUE)
#OANALL$TYPE <- "OAN"

#SET OAN TRIP LIMITS (BiMONTHLY NOW)
OANSQ = data.table(PERIOD=(1:6), TL_SQ=c(100,100,100,100,100,100))
OANA1 = data.table(PERIOD=(1:6), TL_A1=c(50,50,50,50,50,50))
OANA2 = data.table(PERIOD=(1:6), TL_A2=c(40,40,40,40,40,40))
OANALL <- merge(merge(OANSQ, OANA1, by="PERIOD", all=TRUE),OANA2, by="PERIOD", all=TRUE)
OANALL$TYPE <- "OAN"

#SET OAS TRIP LIMITS (MONTHLY NOW, BUT DOING BIMO SO X2 AND ALWAYS MAKE SURE HIGHER THAN OA)
#OASSQ = data.table(PERIOD=(1:12), TL_SQ=c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50))
#OASA1 = data.table(PERIOD=(1:12), TL_A1=c(25,25,25,25,25,25,25,25,25,25,25,25))
#OASA2 = data.table(PERIOD=(1:12), TL_A2=c(20,20,20,20,20,20,20,20,20,20,20,20))
#OASALL <- merge(merge(OASSQ, OASA1,by="PERIOD", all=TRUE),OASA2, by="PERIOD", all=TRUE)
#OASALL$TYPE <- "OAS"

#SET OAS TRIP LIMITS (BiMONTHLY NOW)
OASSQ = data.table(PERIOD=(1:6), TL_SQ=c(100,100,100,100,100,100))
OASA1 = data.table(PERIOD=(1:6), TL_A1=c(50,50,50,50,50,50))
OASA2 = data.table(PERIOD=(1:6), TL_A2=c(40,40,40,40,40,40))
OASALL <- merge(merge(OASSQ, OASA1,by="PERIOD", all=TRUE),OASA2, by="PERIOD", all=TRUE)
OASALL$TYPE <- "OAS"

#BIND TRIP LIMITS AND THEN MERGE WITH THE AGGREGATED TIX
PacFIN_LEN <- merge(PacFIN_LEN, LENALL, by = c("PERIOD"))
PacFIN_LES <- merge(PacFIN_LES, LESALL, by = c("PERIOD"))
PacFIN_LESS<-merge(PacFIN_LESS, LESSALL, by = c("PERIOD"))
PacFIN_OAN <- merge(PacFIN_OAN, OANALL, by = c("PERIOD"))
PacFIN_OAS <- merge(PacFIN_OAS, OASALL, by = c("PERIOD"))
TixLE <- rbind(PacFIN_LEN,PacFIN_LES)
TixOA<-rbind(PacFIN_OAN,PacFIN_OAS)


#SECTION 3: KNOBS AND DIALS TO MANIPULATE THE DATA (AS THAT IS WHAT TRIP LIMITS DO)
# (1)if catch is higher than new lower TL, then cap them off at new lower TL
# (2)if catch is 90%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# (3)if sq lbs lower than 90% of current limit, then assumed to be incidental catches so left them alone with higher limits
#TixLE[ ,"A1LBS"]<- ifelse(TixLE$tlbs >= TixLE$TL_A1,TixLE$tlbs,ifelse((TixLE$tlbs >= TixLE$TL_A1 | TixLE$tlbs / TixLE$TL_SQ >= .9), TixLE$TL_A1, TixLE$tlbs))
#TixLE[ ,"A2LBS"]<- ifelse(TixLE$tlbs >= TixLE$TL_A2,TixLE$tlbs,ifelse((TixLE$tlbs >= TixLE$TL_A2 | TixLE$tlbs / TixLE$TL_SQ >= .9), TixLE$TL_A2, TixLE$tlbs))
#TixLE[ ,"A3LBS"]<- ifelse(TixLE$tlbs >= TixLE$TL_A3,TixLE$tlbs,ifelse((TixLE$tlbs >= TixLE$TL_A3 | TixLE$tlbs / TixLE$TL_SQ >= .9), TixLE$TL_A3, TixLE$tlbs))
 
#FOR WHEN THE TRIP LIMITS GO DOWN use" TixLE$tlbs >= TixLE$TL_A2,TixLE$TL_A2,"
TixLE[ ,"A1LBS"]<- ifelse(TixLE$tlbs >= TixLE$TL_A1,TixLE$tlbs,ifelse((TixLE$tlbs >= TixLE$TL_A1 | TixLE$tlbs / TixLE$TL_SQ >= .6), TixLE$TL_A1, TixLE$tlbs))
TixLE[ ,"A2LBS"]<- ifelse(TixLE$tlbs >= TixLE$TL_A2,TixLE$TL_A2,ifelse((TixLE$tlbs >= TixLE$TL_A2 | TixLE$tlbs / TixLE$TL_SQ >= .6), TixLE$TL_A2, TixLE$tlbs))
TixLE[ ,"A3LBS"]<- ifelse(TixLE$tlbs >= TixLE$TL_A3,TixLE$TL_A3,ifelse((TixLE$tlbs >= TixLE$TL_A3 | TixLE$tlbs / TixLE$TL_SQ >= .6), TixLE$TL_A3, TixLE$tlbs))

TixOA[ ,"A1LBS"]<- ifelse(TixOA$tlbs >= TixOA$TL_A1,TixOA$TL_A1,ifelse((TixOA$tlbs >= TixOA$TL_A1 | TixOA$tlbs / TixOA$TL_SQ >= .8), TixOA$TL_A1, TixOA$tlbs))
TixOA[ ,"A2LBS"]<- ifelse(TixOA$tlbs >= TixOA$TL_A2,TixOA$TL_A2,ifelse((TixOA$tlbs >= TixOA$TL_A2 | TixOA$tlbs / TixOA$TL_SQ >= .8), TixOA$TL_A2, TixOA$tlbs))

##For the South of 34 27- OA South of 34 27 in 2023 was only 2 vessels
PacFIN_LESS[ ,"A1LBS"]<- ifelse(PacFIN_LESS$tlbs >= PacFIN_LESS$TL_A1,PacFIN_LESS$Tlbs,ifelse((PacFIN_LESS$tlbs >= PacFIN_LESS$TL_A1 | PacFIN_LESS$tlbs / PacFIN_LESS$TL_SQ >= .8), PacFIN_LESS$TL_A1, PacFIN_LESS$tlbs))


#SECTION 4: SUMMARIES
#TRIP LIMIT PROJECTION FOR THE "COMPLEX"
ProjectionLE <- ddply (TixLE, c("TYPE.y"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T), A2_MT = sum(A2LBS / 2204.6, na.rm=T),A3_MT = sum(A3LBS / 2204.6, na.rm=T))
ProjectionSouth <- ddply (PacFIN_LESS, c("TYPE.y"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T))
ProjectionOA <- ddply (TixOA, c("TYPE.y"), summarise, SQ_MT = sum(tlbs /2204.6, na.rm=T), A1_MT = sum(A1LBS / 2204.6, na.rm=T), A2_MT = sum(A2LBS / 2204.6, na.rm=T))
write.csv(Projection, "SSTH/2024/SSTH25_26.csv", row.names = FALSE)