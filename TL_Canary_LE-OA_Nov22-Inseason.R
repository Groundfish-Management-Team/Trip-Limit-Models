####################################################################################################################################################################
####################################################################################################################################################################

#TITLE: Nov 2022 Inseason Canary Rockfish Trip Limit Model

#AUTHOR: "GMT"

####REMINDER: 
####1) EFP LANDINGS NEED TO BE REMOVED FROM SOUTH OF 4010 DATA BEFORE PROCEEDING WITH MODELING.#####
####   OBTAIN FTID OF EFP LANDINGS FROM CDFW.
####3) Identify vessel numbers for VESSEL_NUM==UKNOWN
####2) Period 6 landings from 2021 was used as a proxy for Period 6 in 2022. There were no P6 LEN landings in 2021. 
####################################################################################################################################################################
####################################################################################################################################################################
### Turn off scientific notation to get decimal places###
options(scipen =999)
#Load these packages
library(easypackages)
libraries ("dbplyr", "plyr", "dplyr","data.table", "ggplot2", "dtplyr", "tidyr", "lubridate", "Rmisc", "reshape", "reshape2", "lattice")

#LOAD COMMON TICKET FILE FROM PACFIN
CanLands<-read.csv("C:/Users/mmandrup/OneDrive - California Department of Fish and Wildlife/Documents/R/Trip limits/Canary/Inputs/pacfin_extract_2021-Oct2022.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Remove NT EFP FTIDs
TIX<-subset(CanLands, Is_NT_EFP == 'N' )

#Create LE and OA fields, add Dahl == 11 b/c there has been some canary rockfish caught in Directed OA with troll gear
TIX$TYPE <- NA
TIX$TYPE [TIX$DAHL_GROUNDFISH_CODE %in% c("5", "7", "9")] <- "LE"
TIX$TYPE [TIX$DAHL_GROUNDFISH_CODE %in% c("6", "8", "10", "11")] <- "OA"

#Stratify the data north and south fo 4010
TIX$AREA <-NA
TIX$AREA [TIX$ï..AGENCY_CODE %in% c("O","W")] <- "N4010"
TIX$AREA [TIX$ï..AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE == "1C"] <- "N4010"
TIX$AREA [TIX$ï..AGENCY_CODE %in% c("C") & TIX$PACFIN_CATCH_AREA_CODE != "1C"] <- "S4010"

#Grab proxy data from 2021 to estimate possible landings for Period 6 (Nov - Dec) of 2022. 
TIX21proxy <- subset(TIX, (LANDING_YEAR == '2021' & PERIOD == '6'))

TIX21proxy$period2<- NA
TIX21proxy$period2 [TIX21proxy$PERIOD == 6] <- 6

TIX21proxy$period <-TIX21proxy$period2

TIX21proxy$period2<- NULL

#Create a 'period' field for 2022
TIX2022 <- subset (TIX, LANDING_YEAR == "2022")
TIX2022$period <-NA
TIX2022$period [TIX2022$PERIOD ==1] <-1
TIX2022$period [TIX2022$PERIOD ==2] <-2
TIX2022$period [TIX2022$PERIOD ==3] <-3
TIX2022$period [TIX2022$PERIOD ==4] <-4
TIX2022$period [TIX2022$PERIOD ==5] <-5

#Combine proxy 2021 data with 2022 data to complete the year
TIX22full <-rbind (TIX21proxy, TIX2022)

#Create a 'period' field for 2021 for same number of variables
TIX2021 <- subset (TIX, LANDING_YEAR == "2021")
TIX2021$period <-NA
TIX2021$period [TIX2021$PERIOD ==1] <-1
TIX2021$period [TIX2021$PERIOD ==2] <-2
TIX2021$period [TIX2021$PERIOD ==3] <-3
TIX2021$period [TIX2021$PERIOD ==4] <-4
TIX2021$period [TIX2021$PERIOD ==5] <-5
TIX2021$period [TIX2021$PERIOD ==6] <-6

#Combine the full year of 2021 data with the "full" year of 2022 data
TIX2122full <- rbind (TIX2021, TIX22full)

#Sum the landings by vessel, period, sector, and permit type.
TIXProxyByArea<- ddply(TIX2122full, c("VESSEL_NUM", "period", "TYPE", "AREA"), summarise, AVGtlbs = sum(ROUND_WEIGHT_LBS, na.rm=T)/2)

#Filter by limited entry  
TIXProxyLENLimits <-subset(TIXProxyByArea, AREA =="N4010" & TYPE =="LE")
TIXProxyLESLimits <-subset(TIXProxyByArea, AREA =="S4010" & TYPE =="LE")

#Filter by open access  
TIXProxyOANLimits <-subset(TIXProxyByArea, AREA =="N4010" & TYPE =="OA")
TIXProxyOASLimits <-subset(TIXProxyByArea, AREA =="S4010" & TYPE =="OA")

#Set the LE trip limits, SQ = Opt1
#N of 4010
LENSQ=data.table(period=(1:6),TL_SQ=c(3000,3000,3000,3000,3000,3000))
LENOpt2=data.table(period=(1:6),TL_Opt2=c(3500,3500,3500,3500,3500,3500))
LENOpt3=data.table(period=(1:6),TL_Opt3=c(4000,4000,4000,4000,4000,4000))

LENall<-merge(merge(LENSQ, LENOpt2, by ="period", all=TRUE), LENOpt3, by="period", all=TRUE)
LENall$TYPE2<-"LEN"

TIXLEN<-merge(TIXProxyLENLimits, LENall, by = "period", all=TRUE)

#S of 4010
LESSQ=data.table(period=(1:6),TL_SQ=c(3500,3500,3500,3500,3500,3500))
LESOpt2=data.table(period=(1:6),TL_Opt2=c(4000,4000,4000,4000,4000,4000))

LESall<-merge(LESSQ, LESOpt2, by ="period", all=TRUE)
LESall$TYPE2<-"LES"

TIXLES<-merge(TIXProxyLESLimits, LESall, by ="period", all=TRUE)

#Set the OA trip limits
#N of 4010
OANSQ=data.table(period=(1:6),TL_SQ=c(1000,1000,1000,1000,1000,1000))
OANOpt2=data.table(period=(1:6),TL_Opt2=c(1500,1500,1500,1500,1500,1500))
OANOpt3=data.table(period=(1:6),TL_Opt3=c(2000,2000,2000,2000,2000,2000))

OANall<-merge(merge(OANSQ, OANOpt2, by="period", all=TRUE), OANOpt3,by="period", all=TRUE)
OANall$TYPE2<-"OAN"

TIXOAN<-merge(TIXProxyOANLimits, OANall, by ="period", all=TRUE)

#S of 4010
OASSQ=data.table(period=(1:6),TL_SQ=c(1500,1500,1500,1500,1500,1500))
OASOpt2=data.table(period=(1:6),TL_Opt2=c(2000,2000,2000,2000,2000,2000))

OASall<-merge(OASSQ, OASOpt2, by="period", all=TRUE)
OASall$TYPE2<-"OAS"
TIXOAS<-merge(TIXProxyOASLimits, OASall, by="period", all=TRUE)

#Add percentsq which is the percentage of the status quo trip limit that was taken by the vessel/period/type in the input data.  
TIXLEN$percentsq<-NA
TIXLEN$percentsq <- ifelse (TIXLEN$TL_SQ == 0, 0, TIXLEN$AVGtlbs / TIXLEN$TL_SQ)

TIXLES$percentsq<-NA
TIXLES$percentsq <- ifelse (TIXLES$TL_SQ == 0, 0, TIXLES$AVGtlbs / TIXLES$TL_SQ)

TIXOAN$percentsq<-NA
TIXOAN$percentsq <- ifelse (TIXOAN$TL_SQ == 0, 0, TIXOAN$AVGtlbs / TIXOAN$TL_SQ)

TIXOAS$percentsq<-NA
TIXOAS$percentsq <- ifelse (TIXOAS$TL_SQ == 0, 0, TIXOAS$AVGtlbs / TIXOAS$TL_SQ)


#Apply an additional buffer of 40 percent
# if catch is higher than new lower TL, then cap them off at new lower TL 
# if catch is 60%+ of current limit, then assumed to be a targeter that would catch the new higher limit
# if sq lbs lower than 80% of current limit, then multiple previous landings by percentsq described above 

#LEN, SQLBS = Opt 1
# TIXLEN[ ,"SQLBS"]<- ifelse ((TIXLEN$AVGtlbs >= TIXLEN$TL_SQ | TIXLEN$AVGtlbs / TIXLEN$TL_SQ >= .6), TIXLEN$TL_SQ, TIXLEN$percentsq*TIXLEN$TL_SQ) 
# TIXLEN[ ,"Opt2LBS"]<- ifelse ((TIXLEN$AVGtlbs >= TIXLEN$TL_Opt2 | TIXLEN$AVGtlbs / TIXLEN$TL_SQ >= .6), TIXLEN$TL_Opt2, TIXLEN$percentsq*TIXLEN$TL_Opt2)                           
# TIXLEN[ ,"Opt3LBS"]<- ifelse ((TIXLEN$AVGtlbs >= TIXLEN$TL_Opt3 | TIXLEN$AVGtlbs / TIXLEN$TL_SQ >= .6), TIXLEN$TL_Opt3, TIXLEN$percentsq*TIXLEN$TL_Opt3)                           

TIXLEN[ ,"SQLBS"]<- ifelse ((TIXLEN$AVGtlbs >= TIXLEN$TL_SQ | TIXLEN$AVGtlbs / TIXLEN$TL_SQ >= .6), TIXLEN$TL_SQ, TIXLEN$percentsq*TIXLEN$TL_SQ) 
TIXLEN[ ,"Opt2LBS"]<- ifelse ((TIXLEN$AVGtlbs >= TIXLEN$TL_Opt2 | TIXLEN$AVGtlbs / TIXLEN$TL_SQ >= .6), TIXLEN$TL_Opt2, TIXLEN$percentsq*TIXLEN$TL_Opt2)                           
TIXLEN[ ,"Opt3LBS"]<- ifelse ((TIXLEN$AVGtlbs >= TIXLEN$TL_Opt3 | TIXLEN$AVGtlbs / TIXLEN$TL_SQ >= .6), TIXLEN$TL_Opt3, TIXLEN$percentsq*TIXLEN$TL_Opt3)                           

#LES, SQLBS = Opt 1
TIXLES[ ,"SQLBS"]<- ifelse ((TIXLES$AVGtlbs >= TIXLES$TL_SQ | TIXLES$AVGtlbs / TIXLES$TL_SQ >= .6), TIXLES$TL_SQ, TIXLES$percentsq*TIXLES$TL_SQ) 
TIXLES[ ,"Opt2LBS"]<- ifelse ((TIXLES$AVGtlbs >= TIXLES$TL_Opt2 | TIXLES$AVGtlbs / TIXLES$TL_SQ >= .6), TIXLES$TL_Opt2, TIXLES$percentsq*TIXLES$TL_Opt2)                           

#LEN, SQLBS = Opt 1
TIXOAN[ ,"SQLBS"]<- ifelse ((TIXOAN$AVGtlbs >= TIXOAN$TL_SQ | TIXOAN$AVGtlbs / TIXOAN$TL_SQ >= .6), TIXOAN$TL_SQ, TIXOAN$percentsq*TIXOAN$TL_SQ) 
TIXOAN[ ,"Opt2LBS"]<- ifelse ((TIXOAN$AVGtlbs >= TIXOAN$TL_Opt2 | TIXOAN$AVGtlbs / TIXOAN$TL_SQ >= .6), TIXOAN$TL_Opt2, TIXOAN$percentsq*TIXOAN$TL_Opt2)                           
TIXOAN[ ,"Opt3LBS"]<- ifelse ((TIXOAN$AVGtlbs >= TIXOAN$TL_Opt3 | TIXOAN$AVGtlbs / TIXOAN$TL_SQ >= .6), TIXOAN$TL_Opt3, TIXOAN$percentsq*TIXOAN$TL_Opt3)                           

#OAS, SQLBS = Opt 1
TIXOAS[ ,"SQLBS"]<- ifelse ((TIXOAS$AVGtlbs >= TIXOAS$TL_SQ | TIXOAS$AVGtlbs / TIXOAS$TL_SQ >= .6), TIXOAS$TL_SQ, TIXOAS$percentsq*TIXOAS$TL_SQ) 
TIXOAS[ ,"Opt2LBS"]<- ifelse ((TIXOAS$AVGtlbs >= TIXOAS$TL_Opt2 | TIXOAS$AVGtlbs / TIXOAS$TL_SQ >= .6), TIXOAS$TL_Opt2, TIXOAS$percentsq*TIXOAS$TL_Opt2)                           

#Calculate the new trip limit projections by LE vs. OA
PROJECTIONLEN<- ddply (TIXLEN, c("TYPE","AREA","period"), summarise, P_SQMT = sum(SQLBS/2204.6, na.rm=T), P_Opt2 = sum(Opt2LBS/2204.6, na.rm=T), P_Opt3 = sum(Opt3LBS/2204.6, na.rm=T)) 
PROJECTIONLES<- ddply (TIXLES, c("TYPE","AREA","period"), summarise, P_SQMT = sum(SQLBS/2204.6, na.rm=T), P_Opt2 = sum(Opt2LBS/2204.6, na.rm=T)) 
PROJECTIONOAN<- ddply (TIXOAN, c("TYPE","AREA","period"), summarise, P_SQMT = sum(SQLBS/2204.6, na.rm=T), P_Opt2 = sum(Opt2LBS/2204.6, na.rm=T), P_Opt3 = sum(Opt3LBS/2204.6, na.rm=T))
PROJECTIONOAS<- ddply (TIXOAS, c("TYPE","AREA","period"), summarise, P_SQMT = sum(SQLBS/2204.6, na.rm=T), P_Opt2 = sum(Opt2LBS/2204.6, na.rm=T))

#Save projections to you local drive
write.csv(PROJECTIONLEN, "C:/Users/mmandrup/OneDrive - California Department of Fish and Wildlife/Documents/R/Trip limits/Canary/Outputs/2023_LENCanary_Trip_Limit_Summary_P6proxy.csv")
write.csv(PROJECTIONLES, "C:/Users/mmandrup/OneDrive - California Department of Fish and Wildlife/Documents/R/Trip limits/Canary/Outputs/2023_LESCanary_Trip_Limit_Summary_P6proxy.csv")
write.csv(PROJECTIONOAN, "C:/Users/mmandrup/OneDrive - California Department of Fish and Wildlife/Documents/R/Trip limits/Canary/Outputs/2023_OANCanary_Trip_Limit_Summary_P6proxy.csv")
write.csv(PROJECTIONOAS, "C:/Users/mmandrup/OneDrive - California Department of Fish and Wildlife/Documents/R/Trip limits/Canary/Outputs/2023_OASCanary_Trip_Limit_Summary_P6proxy.csv")



