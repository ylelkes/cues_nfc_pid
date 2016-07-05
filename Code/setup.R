library(dplyr)

data <- read.csv("Data/SSI_bertariyph.csv",sep = ",",header = T)
data <- subset(data,!is.na(PID))
data <- data[!duplicated(data$PID),]

source("~/Dropbox/func.R")

data$dempid <- 1-rowMeans(mapply(zero1,with(data,data.frame(huddyid_dem_1,huddyid_dem_2,huddyid_dem_3,huddyid_dem_4,huddyid_dem_5,huddyid_dem_6,huddyid_dem_7,huddyid_dem_8))),na.rm=T)
data$reppid <- 1-rowMeans(mapply(zero1,with(data,data.frame(Q11_1,Q11_2,Q11_3,Q11_4,Q11_5,Q11_6,Q11_7,Q11_8))),na.rm=T)

data$partyidentity <- rowMeans(data.frame(data$dempid,data$reppid),na.rm=T)
nfcmeasures <- data.frame(mapply(zero1,select(data,contains("NfC_"))[,5:22]))
nfcmeasures$NfC_3 <-  1-nfcmeasures$NfC_3
nfcmeasures$NfC_4 <-  1-nfcmeasures$NfC_4
nfcmeasures$NfC_5 <-  1-nfcmeasures$NfC_5
nfcmeasures$NfC_7 <-  1-nfcmeasures$NfC_7
nfcmeasures$NfC_8 <-  1-nfcmeasures$NfC_8
nfcmeasures$NfC_9 <-  1-nfcmeasures$NfC_9
nfcmeasures$NfC_12 <-  1-nfcmeasures$NfC_12
nfcmeasures$NfC_16 <-  1-nfcmeasures$NfC_16
nfcmeasures$NfC_17 <-  1-nfcmeasures$NfC_17
data$nfc <- rowMeans(nfcmeasures,na.rm=T)

data$pid <- NA
data$pid[is.na(data$dempid)]='Republican'
data$pid[is.na(data$reppid)]='Democrat'
data$pid[is.na(data$reppid) & is.na(data$dempid)]=NA

data$supportttip <- 1-zero1(rowMeans(data.frame(data$ttip_complex_reps,data$ttip_complex_dems,data$ttip_simple_reps,data$Q15),na.rm=T))


data$demrepcondition <- as.factor(car::recode(as.numeric(data$DO.BL.CueExperiment),"2='Democrats Support';3='Democrats Support';4='Republicans Support';5='Republicans Support';else=NA"))
data$simplecomplex <- as.factor(car::recode(as.numeric(data$DO.BL.CueExperiment),"2='Simple';3='Complex';4='Complex';5='Simple';else=NA"))

data$ownpartysupport <- 0
data$ownpartysupport[as.numeric(data$demrepcondition)==1 & data$pid=='Democrat']=1
data$ownpartysupport[as.numeric(data$demrepcondition)==2 & data$pid=='Republican']=1

summary(lm(supportttip~partyidentity*ownpartysupport*simplecomplex,data))

summary(lm(supportttip~nfc*ownpartysupport+pid+partyidentity*ownpartysupport,subset(data,simplecomplex=='Simple')))
summary(lm(supportttip~nfc*ownpartysupport+pid+partyidentity*ownpartysupport,subset(data,simplecomplex=='Complex')))
