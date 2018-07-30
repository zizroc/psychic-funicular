####
#Marcus J Thomson - 2017
#International Institute for Applied Systems Analysis
####

#This script will help generate comparison plots for rolling Spearman rank correlation tests.
library(scales)
library(tidyverse)
library(readr)
library(zoo)
library(data.table)

#The LAT-LON site locations shown here are randomply shifted by up to +/- 0.25 deg to hide their precise locations, to protect archaeological sites and comply with state law.
load(file="~/Data/SiteSPD1sigma_loc_jittered.Rdata") #site.spd.1sig
load(file="~/Data/SiteSPD2sigma_loc_jittered.Rdata") #site.spd.2sig

#These files contain EPIC crop simulation data organised by site index (corresponding to geolocation), year CE, and mean annual yield for 7 management strategies, for the rainfed (RFD) and irrigated (IRR) cases.
load(file="~/Data/data_RFD.Rdata")
Data.rfd <- Data.df
rm(Data.df)
load(file="~/Data/data_IRR.Rdata")
Data.irr <- Data.df
rm(Data.df)

dat.rfd.df <- Data.rfd
site.list <- unique(Data.rfd$INDEX)
dat.irr.df <- Data.irr
site.list <- unique(Data.irr$INDEX)
for(i in site.list){
  k = 1350 #cut-off year for analysis
  tmp00.rfd <- dat.rfd.df[dat.rfd.df$INDEX==i,] #reads in all strategy and spd data for site i, RFD
  tmp00.irr <- dat.irr.df[dat.irr.df$INDEX==i,] #reads in all strategy and spd data for site i, IRR
  
  spd <- data.frame(YEAR=site.spd.2sig[[i]]$YEAR_CE, SPD=site.spd.2sig[[i]]$SPD)
  tmp0.rfd <- merge(tmp00.rfd, spd)
  tmp0.irr <- merge(tmp00.irr, spd)
  
  tmp1.rfd <- tmp0.rfd[!is.na(tmp0.rfd$SPD),] #2-sigma spd for site i, NAs removed
  tmp1.irr <- tmp0.irr[!is.na(tmp0.irr$SPD),] #2-sigma spd for site i, NAs removed
  
  tmp2     <- c(11:(length(tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR)-10)) #establish 20 year buffers on either side of the data
  tmp.yrs  <- tmp1.rfd$YEAR[tmp2]
  tmp.spd  <- tmp1.rfd[tmp1.rfd$YEAR < k,]$SPD[tmp2]/sum(tmp1.rfd[tmp1.rfd$YEAR < k,]$SPD[tmp2]) #normalised 2-sigma spd for site i, NAs removed
  tmp.spdINV0 <- max(tmp.spd)-tmp.spd
  tmp.spdINV <- tmp.spdINV0/sum(tmp.spdINV0)
  tmp.len  <- length(tmp.spd) #length of spd data vector
  
  tmp.ms.rfd  <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MS)
  tmp.ic1.rfd <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC1)
  tmp.ic2.rfd <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC2)
  tmp.ic3.rfd <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC3)
  tmp.ic4.rfd <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC4)
  tmp.mc1.rfd <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MC1)
  tmp.mc2.rfd <- data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MC2)
  
  tmp.ms.irr  <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MS)
  tmp.ic1.irr <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC1)
  tmp.ic2.irr <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC2)
  tmp.ic3.irr <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC3)
  tmp.ic4.irr <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC4)
  tmp.mc1.irr <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MC1)
  tmp.mc2.irr <- data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MC2)
  
  sd.rfd.tim  <- tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR
  sd.rfd.time <- sd.rfd.tim[10:(length(sd.rfd.tim)-11)]
  tmp.ms.rfd.sd  <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MS, 21, sd))
  tmp.ic1.rfd.sd <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC1, 21, sd))
  tmp.ic2.rfd.sd <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC2, 21, sd))
  tmp.ic3.rfd.sd <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC3, 21, sd))
  tmp.ic4.rfd.sd <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC4, 21, sd))
  tmp.mc1.rfd.sd <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MC1, 21, sd))
  tmp.mc2.rfd.sd <- data.frame(YEAR=sd.rfd.time, SIGMA=rollapply(tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MC2, 21, sd))
  
  sd.irr.tim  <- tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR #RFD is the same length as IRR
  sd.irr.time <- sd.irr.tim[10:(length(sd.irr.tim)-11)]
  tmp.ms.irr.sd  <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MS, 21, sd))
  tmp.ic1.irr.sd <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC1, 21, sd))
  tmp.ic2.irr.sd <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC2, 21, sd))
  tmp.ic3.irr.sd <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC3, 21, sd))
  tmp.ic4.irr.sd <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC4, 21, sd))
  tmp.mc1.irr.sd <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MC1, 21, sd))
  tmp.mc2.irr.sd <- data.frame(YEAR=sd.irr.time, SIGMA=rollapply(tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MC2, 21, sd))
  
  
  tmpms  <- tmp.ms.rfd.sd[tmp.ms.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ms.rfd.sd[tmp.ms.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic1 <- tmp.ic1.rfd.sd[tmp.ic1.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic1.rfd.sd[tmp.ic1.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic2 <- tmp.ic2.rfd.sd[tmp.ic2.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic2.rfd.sd[tmp.ic2.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic3 <- tmp.ic3.rfd.sd[tmp.ic3.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic3.rfd.sd[tmp.ic3.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic4 <- tmp.ic4.rfd.sd[tmp.ic4.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic4.rfd.sd[tmp.ic4.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpmc1 <- tmp.mc1.rfd.sd[tmp.mc1.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.mc1.rfd.sd[tmp.mc1.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpmc2 <- tmp.mc2.rfd.sd[tmp.mc2.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.mc2.rfd.sd[tmp.mc2.rfd.sd$YEAR %in% tmp.yrs,]$SIGMA)
  
  tmpms.lo  <- tmpms
  tmpic1.lo <- tmpic1
  tmpic2.lo <- tmpic2
  tmpic3.lo <- tmpic3
  tmpic4.lo <- tmpic4
  tmpmc1.lo <- tmpmc1
  tmpmc2.lo <- tmpmc2
  
  tmp.ms.rfd.sd.ks   <- sum((tmpms.lo-tmp.spdINV[-c(1)])^2)
  tmp.ic1.rfd.sd.ks  <- sum((tmpic1.lo-tmp.spdINV[-c(1)])^2)
  tmp.ic2.rfd.sd.ks  <- sum((tmpic2.lo-tmp.spdINV[-c(1)])^2)
  tmp.ic3.rfd.sd.ks  <- sum((tmpic3.lo-tmp.spdINV[-c(1)])^2)
  tmp.ic4.rfd.sd.ks  <- sum((tmpic4.lo-tmp.spdINV[-c(1)])^2)
  tmp.mc1.rfd.sd.ks  <- sum((tmpmc1.lo-tmp.spdINV[-c(1)])^2)
  tmp.mc2.rfd.sd.ks  <- sum((tmpmc2.lo-tmp.spdINV[-c(1)])^2)
  
  rm(tmpms, tmpic1, tmpic2, tmpic3, tmpic4, tmpmc1, tmpmc2)
  
  tmpms  <- tmp.ms.irr.sd[tmp.ms.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ms.irr.sd[tmp.ms.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic1 <- tmp.ic1.irr.sd[tmp.ic1.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic1.irr.sd[tmp.ic1.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic2 <- tmp.ic2.irr.sd[tmp.ic2.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic2.irr.sd[tmp.ic2.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic3 <- tmp.ic3.irr.sd[tmp.ic3.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic3.irr.sd[tmp.ic3.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpic4 <- tmp.ic4.irr.sd[tmp.ic4.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.ic4.irr.sd[tmp.ic4.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpmc1 <- tmp.mc1.irr.sd[tmp.mc1.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.mc1.irr.sd[tmp.mc1.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  tmpmc2 <- tmp.mc2.irr.sd[tmp.mc2.irr.sd$YEAR %in% tmp.yrs,]$SIGMA/sum(tmp.mc2.irr.sd[tmp.mc2.irr.sd$YEAR %in% tmp.yrs,]$SIGMA)
  
  tmpms.lo  <- tmpms
  tmpic1.lo <- tmpic1
  tmpic2.lo <- tmpic2
  tmpic3.lo <- tmpic3
  tmpic4.lo <- tmpic4
  tmpmc1.lo <- tmpmc1
  tmpmc2.lo <- tmpmc2
  
  tmp.ms.irr.sd.ks   <- ks.test(tmpms.lo, tmp.spdINV)
  tmp.ic1.irr.sd.ks  <- ks.test(tmpic1.lo, tmp.spdINV)
  tmp.ic2.irr.sd.ks  <- ks.test(tmpic2.lo, tmp.spdINV)
  tmp.ic3.irr.sd.ks  <- ks.test(tmpic3.lo, tmp.spdINV)
  tmp.ic4.irr.sd.ks  <- ks.test(tmpic4.lo, tmp.spdINV)
  tmp.mc1.irr.sd.ks  <- ks.test(tmpmc1.lo, tmp.spdINV)
  tmp.mc2.irr.sd.ks  <- ks.test(tmpmc2.lo, tmp.spdINV)
  rm(tmpms, tmpic1, tmpic2, tmpic3, tmpic4, tmpmc1, tmpmc2)
  
  tmp.ms.rfd.smooth  <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MS), 21, mean))
  tmp.ic1.rfd.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC1), 21, mean))
  tmp.ic2.rfd.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC2), 21, mean))
  tmp.ic3.rfd.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC3), 21, mean))
  tmp.ic4.rfd.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_IC4), 21, mean))
  tmp.mc1.rfd.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MC1), 21, mean))
  tmp.mc2.rfd.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.rfd[tmp1.rfd$YEAR < k,]$YEAR, YIELD=tmp1.rfd[tmp1.rfd$YEAR < k,]$YIELD_MC2), 21, mean))
  
  tmp.ms.irr.smooth  <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MS), 21, mean))
  tmp.ic1.irr.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC1), 21, mean))
  tmp.ic2.irr.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC2), 21, mean))
  tmp.ic3.irr.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC3), 21, mean))
  tmp.ic4.irr.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_IC4), 21, mean))
  tmp.mc1.irr.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MC1), 21, mean))
  tmp.mc2.irr.smooth <- as.data.frame(rollapply(data.frame(YEAR=tmp1.irr[tmp1.irr$YEAR < k,]$YEAR, YIELD=tmp1.irr[tmp1.irr$YEAR < k,]$YIELD_MC2), 21, mean))
  
  tmpms  <- tmp.ms.rfd.smooth[tmp.ms.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ms.rfd.smooth[tmp.ms.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic1 <- tmp.ic1.rfd.smooth[tmp.ic1.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic1.rfd.smooth[tmp.ic1.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic2 <- tmp.ic2.rfd.smooth[tmp.ic2.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic2.rfd.smooth[tmp.ic2.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic3 <- tmp.ic3.rfd.smooth[tmp.ic3.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic3.rfd.smooth[tmp.ic3.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic4 <- tmp.ic4.rfd.smooth[tmp.ic4.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic4.rfd.smooth[tmp.ic4.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpmc1 <- tmp.mc1.rfd.smooth[tmp.mc1.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.mc1.rfd.smooth[tmp.mc1.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpmc2 <- tmp.mc2.rfd.smooth[tmp.mc2.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.mc2.rfd.smooth[tmp.mc2.rfd.smooth$YEAR %in% tmp.yrs,]$YIELD)
  
  tmp.ms.rfd.ks  <- ks.test(tmpms,  tmp.spd)
  tmp.ic1.rfd.ks <- ks.test(tmpic1, tmp.spd)
  tmp.ic2.rfd.ks <- ks.test(tmpic2, tmp.spd)
  tmp.ic3.rfd.ks <- ks.test(tmpic3, tmp.spd)
  tmp.ic4.rfd.ks <- ks.test(tmpic4, tmp.spd)
  tmp.mc1.rfd.ks <- ks.test(tmpmc1, tmp.spd)
  tmp.mc2.rfd.ks <- ks.test(tmpmc2, tmp.spd)
  rm(tmpms, tmpic1, tmpic2, tmpic3, tmpic4, tmpmc1, tmpmc2)
  
  tmpms  <- tmp.ms.irr.smooth[tmp.ms.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ms.irr.smooth[tmp.ms.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic1 <- tmp.ic1.irr.smooth[tmp.ic1.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic1.irr.smooth[tmp.ic1.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic2 <- tmp.ic2.irr.smooth[tmp.ic2.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic2.irr.smooth[tmp.ic2.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic3 <- tmp.ic3.irr.smooth[tmp.ic3.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic3.irr.smooth[tmp.ic3.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpic4 <- tmp.ic4.irr.smooth[tmp.ic4.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.ic4.irr.smooth[tmp.ic4.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpmc1 <- tmp.mc1.irr.smooth[tmp.mc1.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.mc1.irr.smooth[tmp.mc1.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  tmpmc2 <- tmp.mc2.irr.smooth[tmp.mc2.irr.smooth$YEAR %in% tmp.yrs,]$YIELD/sum(tmp.mc2.irr.smooth[tmp.mc2.irr.smooth$YEAR %in% tmp.yrs,]$YIELD)
  
  tmp.ms.irr.ks  <- ks.test(tmpms, tmp.spd)
  tmp.ic1.irr.ks <- ks.test(tmpic1, tmp.spd)
  tmp.ic2.irr.ks <- ks.test(tmpic2, tmp.spd)
  tmp.ic3.irr.ks <- ks.test(tmpic3, tmp.spd)
  tmp.ic4.irr.ks <- ks.test(tmpic4, tmp.spd)
  tmp.mc1.irr.ks <- ks.test(tmpmc1, tmp.spd)
  tmp.mc2.irr.ks <- ks.test(tmpmc2, tmp.spd)
  
  tmp.ran0   <- runif(length(tmp1.rfd$YEAR), 0, 1) #stochastic "crop yield"
  tmp.ran1   <- rollapply(tmp.ran0, 21, mean)
  tmp.ran    <- tmp.ran1/sum(tmp.ran1)
  rm(tmp.ran0)
  tmp.ran0   <- runif(length(tmp1.rfd$YEAR), 0, 1)
  tmp.ran1   <- rollapply(tmp.ran0, 21, mean)
  tmp.ran.sd <- tmp.ran1/sum(tmp.ran1)
  rm(tmp.ran0)
  
  tmp.ctrl    <- ks.test(tmp.ran, tmp.spd)
  tmp.ctrl.sd <- ks.test(tmp.ran.sd, tmp.spd)
  
  if(tmp.ms.rfd.ks[2]  <= 0.05) Tmp.ms.rfd.sd  <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ms.rfd.sd.ks[1]),  p_value=as.numeric(tmp.ms.rfd.sd.ks[2]))
  if(tmp.ic1.rfd.ks[2] <= 0.05) Tmp.ic1.rfd.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic1.rfd.sd.ks[1]), p_value=as.numeric(tmp.ic1.rfd.sd.ks[2]))
  if(tmp.ic2.rfd.ks[2] <= 0.05) Tmp.ic2.rfd.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic2.rfd.sd.ks[1]), p_value=as.numeric(tmp.ic2.rfd.sd.ks[2]))
  if(tmp.ic3.rfd.ks[2] <= 0.05) Tmp.ic3.rfd.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic3.rfd.sd.ks[1]), p_value=as.numeric(tmp.ic3.rfd.sd.ks[2]))
  if(tmp.ic4.rfd.ks[2] <= 0.05) Tmp.ic4.rfd.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic4.rfd.sd.ks[1]), p_value=as.numeric(tmp.ic4.rfd.sd.ks[2]))
  if(tmp.mc1.rfd.ks[2] <= 0.05) Tmp.mc1.rfd.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc1.rfd.sd.ks[1]), p_value=as.numeric(tmp.mc1.rfd.sd.ks[2]))
  if(tmp.mc2.rfd.ks[2] <= 0.05) Tmp.mc2.rfd.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc2.rfd.sd.ks[1]), p_value=as.numeric(tmp.mc2.rfd.sd.ks[2]))
  
  if(tmp.ms.rfd.ks[2]  <= 0.05) Tmp.ms.irr.sd  <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ms.irr.sd.ks[1]),  p_value=as.numeric(tmp.ms.irr.sd.ks[2]))
  if(tmp.ic1.rfd.ks[2] <= 0.05) Tmp.ic1.irr.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic1.irr.sd.ks[1]), p_value=as.numeric(tmp.ic1.irr.sd.ks[2]))
  if(tmp.ic2.rfd.ks[2] <= 0.05) Tmp.ic2.irr.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic2.irr.sd.ks[1]), p_value=as.numeric(tmp.ic2.irr.sd.ks[2]))
  if(tmp.ic3.rfd.ks[2] <= 0.05) Tmp.ic3.irr.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic3.irr.sd.ks[1]), p_value=as.numeric(tmp.ic3.irr.sd.ks[2]))
  if(tmp.ic4.rfd.ks[2] <= 0.05) Tmp.ic4.irr.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic4.irr.sd.ks[1]), p_value=as.numeric(tmp.ic4.irr.sd.ks[2]))
  if(tmp.mc1.rfd.ks[2] <= 0.05) Tmp.mc1.irr.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc1.irr.sd.ks[1]), p_value=as.numeric(tmp.mc1.irr.sd.ks[2]))
  if(tmp.mc2.rfd.ks[2] <= 0.05) Tmp.mc2.irr.sd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc2.irr.sd.ks[1]), p_value=as.numeric(tmp.mc2.irr.sd.ks[2]))
  
  if(tmp.ctrl.sd[2] <= 0.05) Tmp.ctrl.sd <- data.frame(site=i, control=1, ks_test=as.numeric(tmp.ctrl.sd[1]), p_value=as.numeric(tmp.ctrl.sd[2]))
  
  if(tmp.ms.rfd.ks[2]  <= 0.05) Tmp.ms.rfd  <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ms.rfd.ks[1]),  p_value=as.numeric(tmp.ms.rfd.ks[2]))
  if(tmp.ic1.rfd.ks[2] <= 0.05) Tmp.ic1.rfd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic1.rfd.ks[1]), p_value=as.numeric(tmp.ic1.rfd.ks[2]))
  if(tmp.ic2.rfd.ks[2] <= 0.05) Tmp.ic2.rfd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic2.rfd.ks[1]), p_value=as.numeric(tmp.ic2.rfd.ks[2]))
  if(tmp.ic3.rfd.ks[2] <= 0.05) Tmp.ic3.rfd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic3.rfd.ks[1]), p_value=as.numeric(tmp.ic3.rfd.ks[2]))
  if(tmp.ic4.rfd.ks[2] <= 0.05) Tmp.ic4.rfd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic4.rfd.ks[1]), p_value=as.numeric(tmp.ic4.rfd.ks[2]))
  if(tmp.mc1.rfd.ks[2] <= 0.05) Tmp.mc1.rfd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc1.rfd.ks[1]), p_value=as.numeric(tmp.mc1.rfd.ks[2]))
  if(tmp.mc2.rfd.ks[2] <= 0.05) Tmp.mc2.rfd <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc2.rfd.ks[1]), p_value=as.numeric(tmp.mc2.rfd.ks[2]))
  
  if(tmp.ms.irr.ks[2]  <= 0.05) Tmp.ms.irr  <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ms.irr.ks[1]),  p_value=as.numeric(tmp.ms.irr.ks[2]))
  if(tmp.ic1.irr.ks[2] <= 0.05) Tmp.ic1.irr <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic1.irr.ks[1]), p_value=as.numeric(tmp.ic1.irr.ks[2]))
  if(tmp.ic2.irr.ks[2] <= 0.05) Tmp.ic2.irr <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic2.irr.ks[1]), p_value=as.numeric(tmp.ic2.irr.ks[2]))
  if(tmp.ic3.irr.ks[2] <= 0.05) Tmp.ic3.irr <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic3.irr.ks[1]), p_value=as.numeric(tmp.ic3.irr.ks[2]))
  if(tmp.ic4.irr.ks[2] <= 0.05) Tmp.ic4.irr <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.ic4.irr.ks[1]), p_value=as.numeric(tmp.ic4.irr.ks[2]))
  if(tmp.mc1.irr.ks[2] <= 0.05) Tmp.mc1.irr <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc1.irr.ks[1]), p_value=as.numeric(tmp.mc1.irr.ks[2]))
  if(tmp.mc2.irr.ks[2] <= 0.05) Tmp.mc2.irr <- data.frame(site=i, control=0, ks_test=as.numeric(tmp.mc2.irr.ks[1]), p_value=as.numeric(tmp.mc2.irr.ks[2]))
  
  if(tmp.ctrl[2] <= 0.05) Tmp.ctrl <- data.frame(site=i, control=1, ks_test=as.numeric(tmp.ctrl[1]), p_value=as.numeric(tmp.ctrl[2]))
  
  if(i==min(site.list)){
    TMP.ms.rfd  <- Tmp.ms.rfd
    TMP.ic1.rfd <- Tmp.ic1.rfd
    TMP.ic2.rfd <- Tmp.ic2.rfd
    TMP.ic3.rfd <- Tmp.ic3.rfd
    TMP.ic4.rfd <- Tmp.ic4.rfd
    TMP.mc1.rfd <- Tmp.mc1.rfd
    TMP.mc2.rfd <- Tmp.mc2.rfd
    
    TMP.ms.irr  <- Tmp.ms.irr
    TMP.ic1.irr <- Tmp.ic1.irr
    TMP.ic2.irr <- Tmp.ic2.irr
    TMP.ic3.irr <- Tmp.ic3.irr
    TMP.ic4.irr <- Tmp.ic4.irr
    TMP.mc1.irr <- Tmp.mc1.irr
    TMP.mc2.irr <- Tmp.mc2.irr
    
    TMP.ctrl <- Tmp.ctrl
    
    TMP.ms.rfd.sd  <- Tmp.ms.rfd.sd
    TMP.ic1.rfd.sd <- Tmp.ic1.rfd.sd
    TMP.ic2.rfd.sd <- Tmp.ic2.rfd.sd
    TMP.ic3.rfd.sd <- Tmp.ic3.rfd.sd
    TMP.ic4.rfd.sd <- Tmp.ic4.rfd.sd
    TMP.mc1.rfd.sd <- Tmp.mc1.rfd.sd
    TMP.mc2.rfd.sd <- Tmp.mc2.rfd.sd
    
    TMP.ms.irr.sd  <- Tmp.ms.irr.sd
    TMP.ic1.irr.sd <- Tmp.ic1.irr.sd
    TMP.ic2.irr.sd <- Tmp.ic2.irr.sd
    TMP.ic3.irr.sd <- Tmp.ic3.irr.sd
    TMP.ic4.irr.sd <- Tmp.ic4.irr.sd
    TMP.mc1.irr.sd <- Tmp.mc1.irr.sd
    TMP.mc2.irr.sd <- Tmp.mc2.irr.sd
    
    TMP.ctrl.sd <- Tmp.ctrl.sd
    
  }
  if(i >min(site.list)){
    TMP.ms.rfd  <- rbind(TMP.ms.rfd,  Tmp.ms.rfd)
    TMP.ic1.rfd <- rbind(TMP.ic1.rfd, Tmp.ic1.rfd)
    TMP.ic2.rfd <- rbind(TMP.ic2.rfd, Tmp.ic2.rfd)
    TMP.ic3.rfd <- rbind(TMP.ic3.rfd, Tmp.ic3.rfd)
    TMP.ic4.rfd <- rbind(TMP.ic4.rfd, Tmp.ic4.rfd)
    TMP.mc1.rfd <- rbind(TMP.mc1.rfd, Tmp.mc1.rfd)
    TMP.mc2.rfd <- rbind(TMP.mc2.rfd, Tmp.mc2.rfd)
    
    TMP.ms.irr  <- rbind(TMP.ms.irr,  Tmp.ms.irr)
    TMP.ic1.irr <- rbind(TMP.ic1.irr, Tmp.ic1.irr)
    TMP.ic2.irr <- rbind(TMP.ic2.irr, Tmp.ic2.irr)
    TMP.ic3.irr <- rbind(TMP.ic3.irr, Tmp.ic3.irr)
    TMP.ic4.irr <- rbind(TMP.ic4.irr, Tmp.ic4.irr)
    TMP.mc1.irr <- rbind(TMP.mc1.irr, Tmp.mc1.irr)
    TMP.mc2.irr <- rbind(TMP.mc2.irr, Tmp.mc2.irr)
    
    TMP.ctrl <- rbind(TMP.ctrl, Tmp.ctrl)
    
    TMP.ms.rfd.sd  <- rbind(TMP.ms.rfd.sd,  Tmp.ms.rfd.sd)
    TMP.ic1.rfd.sd <- rbind(TMP.ic1.rfd.sd, Tmp.ic1.rfd.sd)
    TMP.ic2.rfd.sd <- rbind(TMP.ic2.rfd.sd, Tmp.ic2.rfd.sd)
    TMP.ic3.rfd.sd <- rbind(TMP.ic3.rfd.sd, Tmp.ic3.rfd.sd)
    TMP.ic4.rfd.sd <- rbind(TMP.ic4.rfd.sd, Tmp.ic4.rfd.sd)
    TMP.mc1.rfd.sd <- rbind(TMP.mc1.rfd.sd, Tmp.mc1.rfd.sd)
    TMP.mc2.rfd.sd <- rbind(TMP.mc2.rfd.sd, Tmp.mc2.rfd.sd)
    
    TMP.ms.irr.sd  <- rbind(TMP.ms.irr.sd,  Tmp.ms.irr.sd)
    TMP.ic1.irr.sd <- rbind(TMP.ic1.irr.sd, Tmp.ic1.irr.sd)
    TMP.ic2.irr.sd <- rbind(TMP.ic2.irr.sd, Tmp.ic2.irr.sd)
    TMP.ic3.irr.sd <- rbind(TMP.ic3.irr.sd, Tmp.ic3.irr.sd)
    TMP.ic4.irr.sd <- rbind(TMP.ic4.irr.sd, Tmp.ic4.irr.sd)
    TMP.mc1.irr.sd <- rbind(TMP.mc1.irr.sd, Tmp.mc1.irr.sd)
    TMP.mc2.irr.sd <- rbind(TMP.mc2.irr.sd, Tmp.mc2.irr.sd)
    
    TMP.ctrl.sd <- rbind(TMP.ctrl.sd, Tmp.ctrl.sd)
    
    
  }
  
  #this part steps through the time series with time windows and performs spearman's correlation tests
  spearman.ms.rfd  <- vector()
  spearman.ic1.rfd <- vector()
  spearman.ic2.rfd <- vector()
  spearman.ic3.rfd <- vector()
  spearman.ic4.rfd <- vector()
  spearman.mc1.rfd <- vector()
  spearman.mc2.rfd <- vector()
  
  spearman.ms.irr  <- vector()
  spearman.ic1.irr <- vector()
  spearman.ic2.irr <- vector()
  spearman.ic3.irr <- vector()
  spearman.ic4.irr <- vector()
  spearman.mc1.irr <- vector()
  spearman.mc2.irr <- vector()
  
  sd.ms.rfd  <- vector()
  sd.ic1.rfd <- vector()
  sd.ic2.rfd <- vector()  
  sd.ic3.rfd <- vector()
  sd.ic4.rfd <- vector()
  sd.mc1.rfd <- vector()
  sd.mc2.rfd <- vector()
  
  sd.ms.irr  <- vector()
  sd.ic1.irr <- vector()
  sd.ic2.irr <- vector()  
  sd.ic3.irr <- vector()
  sd.ic4.irr <- vector()
  sd.mc1.irr <- vector()
  sd.mc2.irr <- vector()
  
  time.step <- tmp2[1:tmp.len]
  for(j in 1:length(time.step)){
    l = j
    
    spearman.ms.rfd[l]  <- cor(tmp.ms.rfd.smooth[l:(l+25),2],  tmp.spd[l:(l+25)], method="spearman")
    spearman.ic1.rfd[l] <- cor(tmp.ic1.rfd.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.ic2.rfd[l] <- cor(tmp.ic2.rfd.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.ic3.rfd[l] <- cor(tmp.ic3.rfd.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.ic4.rfd[l] <- cor(tmp.ic4.rfd.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.mc1.rfd[l] <- cor(tmp.mc1.rfd.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.mc2.rfd[l] <- cor(tmp.mc2.rfd.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    
    spearman.ms.irr[l]  <- cor(tmp.ms.irr.smooth[l:(l+25),2],  tmp.spd[l:(l+25)], method="spearman")
    spearman.ic1.irr[l] <- cor(tmp.ic1.irr.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.ic2.irr[l] <- cor(tmp.ic2.irr.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.ic3.irr[l] <- cor(tmp.ic3.irr.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.ic4.irr[l] <- cor(tmp.ic4.irr.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.mc1.irr[l] <- cor(tmp.mc1.irr.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    spearman.mc2.irr[l] <- cor(tmp.mc2.irr.smooth[l:(l+25),2], tmp.spd[l:(l+25)], method="spearman")
    
    sd.ms.rfd[l]   <- sd(tmp.ms.rfd[l:(l+25),2])
    sd.ic1.rfd[l]  <- sd(tmp.ic1.rfd[l:(l+25),2])
    sd.ic2.rfd[l]  <- sd(tmp.ic2.rfd[l:(l+25),2])
    sd.ic3.rfd[l]  <- sd(tmp.ic3.rfd[l:(l+25),2])
    sd.ic4.rfd[l]  <- sd(tmp.ic4.rfd[l:(l+25),2])
    sd.mc1.rfd[l]  <- sd(tmp.mc1.rfd[l:(l+25),2])
    sd.mc2.rfd[l]  <- sd(tmp.mc2.rfd[l:(l+25),2])
    
    sd.ms.irr[l]   <- sd(tmp.ms.irr[l:(l+25),2])
    sd.ic1.irr[l]  <- sd(tmp.ic1.irr[l:(l+25),2])
    sd.ic2.irr[l]  <- sd(tmp.ic2.irr[l:(l+25),2])
    sd.ic3.irr[l]  <- sd(tmp.ic3.irr[l:(l+25),2])
    sd.ic4.irr[l]  <- sd(tmp.ic4.irr[l:(l+25),2])
    sd.mc1.irr[l]  <- sd(tmp.mc1.irr[l:(l+25),2])
    sd.mc2.irr[l]  <- sd(tmp.mc2.irr[l:(l+25),2])
    
    sd.yrs <- tmp.ms.rfd[(l+10):(l+25-10),1]
  }
  tmp.rfd.df <- data.frame(index=rep(i, length(spearman.ms.rfd)), year=tmp.yrs[time.step], cbind(spearman.ic1.rfd, spearman.ic2.rfd, spearman.ic3.rfd, spearman.ic4.rfd, spearman.mc1.rfd, spearman.mc2.rfd, spearman.ms.rfd))
  tmp.irr.df <- data.frame(index=rep(i, length(spearman.ms.irr)), year=tmp.yrs[time.step], cbind(spearman.ic1.irr, spearman.ic2.irr, spearman.ic3.irr, spearman.ic4.irr, spearman.mc1.irr, spearman.mc2.irr, spearman.ms.irr))
  sd.rfd.df <- data.frame(index=rep(i, length(sd.ms.rfd)), year=tmp.yrs[time.step], cbind(sd.ic1.rfd, sd.ic2.rfd, sd.ic3.rfd, sd.ic4.rfd, sd.mc1.rfd, sd.mc2.rfd, sd.ms.rfd))
  sd.irr.df <- data.frame(index=rep(i, length(sd.ms.irr)), year=tmp.yrs[time.step], cbind(sd.ic1.irr, sd.ic2.irr, sd.ic3.irr, sd.ic4.irr, sd.mc1.irr, sd.mc2.irr, sd.ms.irr))
  if(i==min(site.list)){
    Tmp.rfd.df <- tmp.rfd.df
    Sd.rfd.df  <- sd.rfd.df
    
    Tmp.irr.df <- tmp.irr.df
    Sd.irr.df  <- sd.irr.df
  }
  if(i >min(site.list)){
    Tmp.rfd.df <- rbind(Tmp.rfd.df, tmp.rfd.df)
    Sd.rfd.df  <- rbind(Sd.rfd.df, sd.rfd.df)
    
    Tmp.irr.df <- rbind(Tmp.irr.df, tmp.irr.df)
    Sd.irr.df  <- rbind(Sd.irr.df, sd.irr.df)
  }
}

data.rfd.df <- rbind(data.frame(strategy=rep("IC1", length(TMP.ic1.rfd[,1])), TMP.ic1.rfd), data.frame(strategy=rep("IC2", length(TMP.ic2.rfd[,1])), TMP.ic2.rfd), data.frame(strategy=rep("IC3", length(TMP.ic3.rfd[,1])), TMP.ic3.rfd), data.frame(strategy=rep("IC4", length(TMP.ic4.rfd[,1])), TMP.ic4.rfd), data.frame(strategy=rep("MC1", length(TMP.mc1.rfd[,1])), TMP.mc1.rfd), data.frame(strategy=rep("MC2", length(TMP.mc2.rfd[,1])), TMP.mc2.rfd), data.frame(strategy=rep("MS", length(TMP.ms.rfd[,1])), TMP.ms.rfd))
data.irr.df <- rbind(data.frame(strategy=rep("IC1", length(TMP.ic1.irr[,1])), TMP.ic1.irr), data.frame(strategy=rep("IC2", length(TMP.ic2.irr[,1])), TMP.ic2.irr), data.frame(strategy=rep("IC3", length(TMP.ic3.irr[,1])), TMP.ic3.irr), data.frame(strategy=rep("IC4", length(TMP.ic4.irr[,1])), TMP.ic4.irr), data.frame(strategy=rep("MC1", length(TMP.mc1.irr[,1])), TMP.mc1.irr), data.frame(strategy=rep("MC2", length(TMP.mc2.irr[,1])), TMP.mc2.irr), data.frame(strategy=rep("MS", length(TMP.ms.irr[,1])), TMP.ms.irr))
data.ctrl.df <- data.frame(strategy=rep("CTRL", length(TMP.ctrl[,1])), TMP.ctrl)

data.rfd.df.sd <- rbind(data.frame(strategy=rep("IC1", length(TMP.ic1.rfd.sd[,1])), TMP.ic1.rfd.sd), data.frame(strategy=rep("IC2", length(TMP.ic2.rfd.sd[,1])), TMP.ic2.rfd.sd), data.frame(strategy=rep("IC3", length(TMP.ic3.rfd.sd[,1])), TMP.ic3.rfd.sd), data.frame(strategy=rep("IC4", length(TMP.ic4.rfd.sd[,1])), TMP.ic4.rfd.sd), data.frame(strategy=rep("MC1", length(TMP.mc1.rfd.sd[,1])), TMP.mc1.rfd.sd), data.frame(strategy=rep("MC2", length(TMP.mc2.rfd.sd[,1])), TMP.mc2.rfd.sd), data.frame(strategy=rep("MS", length(TMP.ms.rfd.sd[,1])), TMP.ms.rfd.sd))
data.irr.df.sd <- rbind(data.frame(strategy=rep("IC1", length(TMP.ic1.irr.sd[,1])), TMP.ic1.irr.sd), data.frame(strategy=rep("IC2", length(TMP.ic2.irr.sd[,1])), TMP.ic2.irr.sd), data.frame(strategy=rep("IC3", length(TMP.ic3.irr.sd[,1])), TMP.ic3.irr.sd), data.frame(strategy=rep("IC4", length(TMP.ic4.irr.sd[,1])), TMP.ic4.irr.sd), data.frame(strategy=rep("MC1", length(TMP.mc1.irr.sd[,1])), TMP.mc1.irr.sd), data.frame(strategy=rep("MC2", length(TMP.mc2.irr.sd[,1])), TMP.mc2.irr.sd), data.frame(strategy=rep("MS", length(TMP.ms.irr.sd[,1])), TMP.ms.irr.sd))
data.ctrl.df.sd <- data.frame(strategy=rep("CTRL", length(TMP.ctrl.sd[,1])), TMP.ctrl.sd)

all.data.rfd.df.sd <- rbind(data.rfd.df.sd, data.ctrl.df.sd)
all.data.irr.df.sd <- rbind(data.irr.df.sd, data.ctrl.df.sd)

all.data.rfd.df <- rbind(data.rfd.df, data.ctrl.df)
all.data.irr.df <- rbind(data.irr.df, data.ctrl.df)

#sites location list with site number index
for(i in site.list){
  tmp <- data.frame(INDEX=rep(i, length(site.spd.1sig[[i]]$LON)), LON=site.spd.1sig[[i]]$LON, LAT=site.spd.1sig[[i]]$LAT)
  if(i==1) Tmp <- tmp
  if(i >1) Tmp <- rbind(Tmp,tmp)
}
sites <- unique(Tmp)
rownames(sites)<-c()

site.list <- unique(Data.rfd$INDEX)
site.list = sites[sites$LON < -111 & sites$LAT > 41,]$INDEX #GSL Fremont sites
site.list = sites[sites$LON >= -111 & sites$LAT > 40,]$INDEX #UB Fremont sites
site.list = na.omit(sites[sites$LON < -111.5 & sites$LON > -113.1 & sites$LAT > 37.5 & sites$LAT < 39.5,]$INDEX) #Wasatch Front

test.rfd.df    <- all.data.rfd.df[all.data.rfd.df$site %in% site.list,]
test.rfd.df.sd <- all.data.rfd.df.sd[all.data.rfd.df.sd$site %in% site.list,]

test.irr.df    <- all.data.irr.df[all.data.irr.df$site %in% site.list,]
test.irr.df.sd <- all.data.irr.df.sd[all.data.irr.df.sd$site %in% site.list,]

site.spd.col <- alpha(c("purple", "blue"), 0.1)
line.cols <- alpha(c(1, 2, 3, 4, 5, 6, 7, "#FFFFFF"), 0.7)
line.cols[8] <- "#FFFFFF"

par(mfrow=c(3,2), mar=c(4,5,3,1))

site.list = sites[sites$LON < -111 & sites$LAT > 41,]$INDEX #GSL Fremont sites
site.list = sites[sites$LON >= -111 & sites$LAT > 40,]$INDEX #UB Fremont sites
site.list = na.omit(sites[sites$LON < -111.5 & sites$LON > -113.1 & sites$LAT > 37.5 & sites$LAT < 39.5,]$INDEX) #Wasatch Front

plot(c(1,1), xlim=c(850, 1250), ylim=c(-1,1), bty="n", ylab=expression(rho), xlab="YEAR CE")
# for(i in nsites){
for(i in site.list){
  tmp.rfd.df <- Tmp.rfd.df[Tmp.rfd.df$index==i & Tmp.rfd.df$year < 1250,]
  # tmp.irr.df <- Tmp.irr.df[Tmp.irr.df$index==i & Tmp.irr.df$year < 1250,]
  
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.ic1.rfd, col=alpha(line.cols[2], 0.05))
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.ic2.rfd, col=alpha(line.cols[3], 0.05))
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.ic3.rfd, col=alpha(line.cols[4], 0.05))
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.ic4.rfd, col=alpha(line.cols[5], 0.05))
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.mc1.rfd, col=alpha(line.cols[6], 0.05))
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.mc2.rfd, col=alpha(line.cols[7], 0.05))
  lines(tmp.rfd.df$year, tmp.rfd.df$spearman.ms.rfd,  col=alpha(line.cols[1], 0.05))
  
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.ic1.irr, col=alpha(line.cols[2], 0.05))
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.ic2.irr, col=alpha(line.cols[3], 0.05))
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.ic3.irr, col=alpha(line.cols[4], 0.05))
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.ic4.irr, col=alpha(line.cols[5], 0.05))
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.mc1.irr, col=alpha(line.cols[6], 0.05))
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.mc2.irr, col=alpha(line.cols[7], 0.05))
  # lines(tmp.irr.df$year, tmp.irr.df$spearman.ms.irr,  col=alpha(line.cols[1], 0.05))
  
}
abline(h=0)

Tmp.ls <- Tmp.rfd.df[Tmp.rfd.df$index %in% site.list,]
# Tmp.ls <- Tmp.irr.df[Tmp.irr.df$index %in% site.list,]

tmp<-aggregate(Tmp.ls, by=list(Tmp.ls$year), FUN=mean, na.rm=TRUE)
# tmp0 <- tmp[tmp$year < 1150,] #GSL sites
tmp0 <- tmp[tmp$year < 1100,] #UB sites
# tmp0 <- tmp[tmp$year < 1250,] #WASATCH sites

lines(tmp0$year, tmp0$spearman.ic1.rfd, col=alpha(line.cols[2],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ic2.rfd, col=alpha(line.cols[3],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ic3.rfd, col=alpha(line.cols[4],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ic4.rfd, col=alpha(line.cols[5],1), lwd=3)
lines(tmp0$year, tmp0$spearman.mc1.rfd, col=alpha(line.cols[6],1), lwd=3)
lines(tmp0$year, tmp0$spearman.mc2.rfd, col=alpha(line.cols[7],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ms.rfd,  col=alpha(line.cols[1],1), lwd=3)

legend("topleft", "Rolling rank correlation", bty="n")
# legend("topright", "GSL sites, RFD", bty="n", cex=1.2)
legend("topright", "UB sites, RFD", bty="n", cex=1.2)
# legend("topright", "WAS sites, RFD", bty="n", cex=1.2)
# legend("topright", "GSL sites, IRR", bty="n", cex=1.2)
# # legend("topright", "UB sites, IRR", bty="n", cex=1.2)
# # legend("topright", "WAS sites, IRR", bty="n", cex=1.2)

plot(c(1,1), pch="", xlim=c(850, 1250), ylim=c(-0.25,0.25), bty="n", ylab=expression(rho), xlab="YEAR CE")

abline(h=0)

ic1lm <- predict(lm(tmp0$spearman.ic1~tmp0$year), data.frame(tmp0$year), interval="confidence")
ic2lm <- predict(lm(tmp0$spearman.ic2~tmp0$year), data.frame(tmp0$year), interval="confidence")
ic3lm <- predict(lm(tmp0$spearman.ic3~tmp0$year), data.frame(tmp0$year), interval="confidence")
ic4lm <- predict(lm(tmp0$spearman.ic4~tmp0$year), data.frame(tmp0$year), interval="confidence")
mc1lm <- predict(lm(tmp0$spearman.mc1~tmp0$year), data.frame(tmp0$year), interval="confidence")
mc2lm <- predict(lm(tmp0$spearman.mc2~tmp0$year), data.frame(tmp0$year), interval="confidence")
mslm  <- predict(lm(tmp0$spearman.ms~tmp0$year), data.frame(tmp0$year), interval="confidence")

polygon(c(rev(tmp0$year), tmp0$year), c(rev(ic1lm[ ,3]), ic1lm[ ,2]), col = alpha(line.cols[2], 0.1), border = NA)
lines(tmp0$year, ic1lm[,1], col=alpha(line.cols[2],1), lwd=3, xlim=c(860,1250))
polygon(c(rev(tmp0$year), tmp0$year), c(rev(ic2lm[ ,3]), ic2lm[ ,2]), col = alpha(line.cols[3], 0.1), border = NA)
lines(tmp0$year, ic2lm[,1], col=alpha(line.cols[3],1), lwd=3, xlim=c(860,1250))
polygon(c(rev(tmp0$year), tmp0$year), c(rev(ic3lm[ ,3]), ic3lm[ ,2]), col = alpha(line.cols[4], 0.1), border = NA)
lines(tmp0$year, ic3lm[,1], col=alpha(line.cols[4],1), lwd=3, xlim=c(860,1250))
polygon(c(rev(tmp0$year), tmp0$year), c(rev(ic4lm[ ,3]), ic4lm[ ,2]), col = alpha(line.cols[5], 0.1), border = NA)
lines(tmp0$year, ic4lm[,1], col=alpha(line.cols[5],1), lwd=3, xlim=c(860,1250))
polygon(c(rev(tmp0$year), tmp0$year), c(rev(mc1lm[ ,3]), mc1lm[ ,2]), col = alpha(line.cols[6], 0.1), border = NA)
lines(tmp0$year, mc1lm[,1], col=alpha(line.cols[6],1), lwd=3, xlim=c(860,1250))
polygon(c(rev(tmp0$year), tmp0$year), c(rev(mc2lm[ ,3]), mc2lm[ ,2]), col = alpha(line.cols[7], 0.1), border = NA)
lines(tmp0$year, mc2lm[,1], col=alpha(line.cols[7],1), lwd=3, xlim=c(860,1250))
polygon(c(rev(tmp0$year), tmp0$year), c(rev(mslm[ ,3]), mslm[ ,2]), col = alpha(line.cols[1], 0.1), border = NA)
lines(tmp0$year, mslm[,1],  col=alpha(line.cols[1],1), lwd=3, xlim=c(860,1250))
legend("topleft", expression(Linear~trend~ensemble~mean~rho), bty="n")
legend("bottomright", c("IC1", "IC2", "IC3", "IC4", "MC1", "MC2", "MS"), col=alpha(line.cols[c(2:7,1)], 1), lwd=3, cex=0.9)


###

# nsite.list = sites[sites$LON < -111 & sites$LAT > 41,]$INDEX #GSL Fremont sites
# nsite.list = sites[sites$LON >= -111 & sites$LAT > 40,]$INDEX #UB Fremont sites
nsite.list = na.omit(sites[sites$LON < -111.5 & sites$LON > -113.1 & sites$LAT > 37.5 & sites$LAT < 39.5,]$INDEX) #Wasatch Front

plot(c(1,1), xlim=c(850, 1250), ylim=c(-1,1), bty="n", ylab="Spearman coefficient", xlab="YEAR CE")
# for(i in nsites){
for(i in site.list){
  tmp.df <- na.omit(Tmp.rfd.df[Tmp.rfd.df$index==i & Tmp.rfd.df$year < 1250,])
  
  lines(tmp.df$year, tmp.df$sd.ic1, col=alpha(line.cols[2], 0.05))
  lines(tmp.df$year, tmp.df$sd.ic2, col=alpha(line.cols[3], 0.05))
  lines(tmp.df$year, tmp.df$sd.ic3, col=alpha(line.cols[4], 0.05))
  lines(tmp.df$year, tmp.df$sd.ic4, col=alpha(line.cols[5], 0.05))
  lines(tmp.df$year, tmp.df$sd.mc1, col=alpha(line.cols[6], 0.05))
  lines(tmp.df$year, tmp.df$sd.mc2, col=alpha(line.cols[7], 0.05))
  lines(tmp.df$year, tmp.df$sd.ms,  col=alpha(line.cols[1], 0.05))
}
abline(h=0)

Tmp.ls <- Tmp.df[Tmp.df$index %in% site.list,]

tmp<-aggregate(Tmp.ls, by=list(Tmp.ls$year), FUN=mean, na.rm=TRUE)
# tmp0 <- tmp[tmp$year < 1150,] #GSL sites
# tmp0 <- tmp[tmp$year < 1100,] #UB sites
tmp0 <- tmp[tmp$year < 1250,] #WASATCH sites

lines(tmp0$year, tmp0$spearman.ic1, col=alpha(line.cols[2],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ic2, col=alpha(line.cols[3],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ic3, col=alpha(line.cols[4],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ic4, col=alpha(line.cols[5],1), lwd=3)
lines(tmp0$year, tmp0$spearman.mc1, col=alpha(line.cols[6],1), lwd=3)
lines(tmp0$year, tmp0$spearman.mc2, col=alpha(line.cols[7],1), lwd=3)
lines(tmp0$year, tmp0$spearman.ms,  col=alpha(line.cols[1],1), lwd=3)

# legend("topleft", "GSL sites, IRR", bty="n")
# legend("topleft", "UB sites, IRR", bty="n")
legend("topleft", "WAS sites, IRR", bty="n")

plot(c(1,1), pch="", xlim=c(850, 1250), ylim=c(-0.25,0.25), bty="n", ylab="Linear trend in time", xlab="YEAR CE")

abline(h=0)
lines(tmp0$year, predict(lm(tmp0$spearman.ic1~tmp0$year), as.data.frame(tmp0$year)), col=alpha(line.cols[2],1), lwd=3, xlim=c(860,1250))
lines(tmp0$year, predict(lm(tmp0$spearman.ic2~tmp0$year), as.data.frame(tmp0$year)), col=alpha(line.cols[3],1), lwd=3, xlim=c(860,1250))
lines(tmp0$year, predict(lm(tmp0$spearman.ic3~tmp0$year), as.data.frame(tmp0$year)), col=alpha(line.cols[4],1), lwd=3, xlim=c(860,1250))
lines(tmp0$year, predict(lm(tmp0$spearman.ic4~tmp0$year), as.data.frame(tmp0$year)), col=alpha(line.cols[5],1), lwd=3, xlim=c(860,1250))
lines(tmp0$year, predict(lm(tmp0$spearman.mc1~tmp0$year), as.data.frame(tmp0$year)), col=alpha(line.cols[6],1), lwd=3, xlim=c(860,1250))
lines(tmp0$year, predict(lm(tmp0$spearman.mc2~tmp0$year), as.data.frame(tmp0$year)), col=alpha(line.cols[7],1), lwd=3, xlim=c(860,1250))
lines(tmp0$year, predict(lm(tmp0$spearman.ms~tmp0$year), as.data.frame(tmp0$year)),  col=alpha(line.cols[1],1), lwd=3, xlim=c(860,1250))
legend("topleft", "Linear trend in Spearman coefficient at left", bty="n")
legend("bottomright", c("IC1", "IC2", "IC3", "IC4", "MC1", "MC2", "MS"), col=alpha(line.cols[c(2:7,1)], 1), lwd=3, cex=0.9)
