library(ncdf4)
library(raster)
library(qmap)
library(akima)

nc.dir <- dir("/media/zizroc/Extra Drive 1/Data/NCEP_Reanalysis_Surface_Flux/2mTemp/", full.names=TRUE)

air2mtemp <- list()
for(i in 1:58){
  nc <- nc_open(nc.dir[i])
  dname <- "air"
  lon    <- ncvar_get(nc, "lon")
  nlon   <- dim(lon)
  lat    <- ncvar_get(nc, "lat")
  nlat   <- dim(lat)
  t      <- ncvar_get(nc, "time")
  tunits <- ncatt_get(nc, "time", "units")
  nt     <- dim(t)
  #ts.array  <- ncvar_get(nc, dname, start = c(1, 1, k*72999), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
  ts.array  <- ncvar_get(nc, dname) #this file is enormous (e.g., reset memory.size)
  dlname    <- ncatt_get(nc ,dname, "long_name")
  dunits    <- ncatt_get(nc, dname, "units")
  fillvalue <- ncatt_get(nc, dname, "_FillValue")
  nc_close(nc)
  rm(nc)
  gc()

  temp.r <- list()
  for(j in 1:nt){
    tmp <- raster(t(ts.array[130:138, 24:30, j]))
    extent(tmp) <- c(lon[130], lon[138], lat[30], lat[24])
    temp.r[[j]] <- tmp
  }
  # if(i==1){Tmp <- temp.r}
  # if(i >1){Tmp <- stack(Tmp, temp.r)}
  air2mtemp[[i+1947]] <- stack(temp.r)
  rm(ts.array, temp.r)
  print(i)
}
save(air2mtemp, file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTemp_1948to2016.Rdata")

##
#Maximum temperature
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.TREFHTMX.18500101-20051231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.TREFHTMX.18500101-20051231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.TREFHTMX.18500101-20051231.nc")
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.TREFHTMX.18500101-20051231.nc")

dname <- "TREFHTMX"

#Variables
lon    <- ncvar_get(tmp.nc, "lon")
nlon   <- dim(lon)
lat    <- ncvar_get(tmp.nc, "lat")
nlat   <- dim(lat)
dat    <- ncvar_get(tmp.nc, "date") #current date (YYYMMDD)
ndat   <- dim(dat)
t      <- ncvar_get(tmp.nc, "time")
tunits <- ncatt_get(tmp.nc, "time", "units")
nt     <- dim(t)

#k = 1/72999 # for start at t=1
#k = 1 #for start at t=200
#ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, k*72999), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
ts.array  <- ncvar_get(tmp.nc, dname) #this file is enormous (e.g., reset memory.size)
dlname    <- ncatt_get(tmp.nc ,dname, "long_name")
dunits    <- ncatt_get(tmp.nc, dname, "units")
fillvalue <- ncatt_get(tmp.nc, dname, "_FillValue")
nc_close(tmp.nc)
rm(tmp.nc)
gc()

tmax.r <- list()
nt=56940 #1850 to 2005 CE
for(i in 1:nt){
  tmp <- raster(t(ts.array[99:102, 71:67, i]))
  extent(tmp) <- c(lon[99], lon[102], lat[67], lat[71])
  tmax.r[[i]] <- tmp
}
rm(ts.array)
gc()
TREFHTMX.1850to2005.dailyRast <- list()
n=156
#n=200
for(i in 1:n){
  index <- seq(i+364*(i-1), i+364*(i-1)+364, 1)
  TREFHTMX.1850to2005.dailyRast[[i+1849]] <- tmax.r[index]
}
#save(tmax.r, file="/media/zizroc/Extra Drive 1/Data/R environments/tmax_for_scaling.Rdata")
rm(tmax.r)
#Saves a list of daily GCM values
# save(TREFHTMX.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em002.Rdata")
# save(TREFHTMX.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em003.Rdata")
# save(TREFHTMX.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em004.Rdata")
save(TREFHTMX.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em005.Rdata")

##
#Minimum temperature
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.TREFHTMN.18500101-20051231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.TREFHTMN.18500101-20051231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.TREFHTMN.18500101-20051231.nc")
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.TREFHTMN.18500101-20051231.nc")
dname <- "TREFHTMN"

#Variables
lon    <- ncvar_get(tmp.nc, "lon")
nlon   <- dim(lon)
lat    <- ncvar_get(tmp.nc, "lat")
nlat   <- dim(lat)
dat    <- ncvar_get(tmp.nc, "date") #current date (YYYMMDD)
ndat   <- dim(dat)
t      <- ncvar_get(tmp.nc, "time")
tunits <- ncatt_get(tmp.nc, "time", "units")
nt     <- dim(t)

#k = 1/72999 # for start at t=1
#k = 1 #for start at t=200
#ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, k*72999), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
ts.array  <- ncvar_get(tmp.nc, dname) #this file is enormous (e.g., reset memory.size)
dlname    <- ncatt_get(tmp.nc ,dname, "long_name")
dunits    <- ncatt_get(tmp.nc, dname, "units")
fillvalue <- ncatt_get(tmp.nc, dname, "_FillValue")
nc_close(tmp.nc)
rm(tmp.nc)
gc()

tmin.r <- list()
nt=56940 #1850 to 2005 CE
for(i in 1:nt){
  tmp <- raster(t(ts.array[99:102, 71:67, i]))
  extent(tmp) <- c(lon[99], lon[102], lat[67], lat[71])
  tmin.r[[i]] <- tmp
}
rm(ts.array)
gc()

TREFHTMN.1850to2005.dailyRast <- list()
n=156
#n=200
for(i in 1:n){
  index <- seq(i+364*(i-1), i+364*(i-1)+364, 1)
  TREFHTMN.1850to2005.dailyRast[[i+1849]] <- tmin.r[index]
}
rm(tmin.r, dname, index, n, nt, dlname, dunits, fillvalue, i, dat, ndat, nlat, nlon, t, tunits, tmp)
#Saves a list of daily GCM values
# save(TREFHTMN.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em002.Rdata")
# save(TREFHTMN.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em003.Rdata")
# save(TREFHTMN.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em004.Rdata")
save(TREFHTMN.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em005.Rdata")
gc()

###
#This block loads daily rasters for maximum and minimum reference height temperature from the GCM.
#Load the correct ensemble member.
#CESM ensemble member 002
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em002.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em002.Rdata") #TREFHTMN.1850to2005.dailyRast
#CESM ensemble member 003
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em003.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em003.Rdata") #TREFHTMN.1850to2005.dailyRast
#CESM ensemble member 004
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em004.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em004.Rdata") #TREFHTMN.1850to2005.dailyRast
#CESM ensemble member 005
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em005.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em005.Rdata") #TREFHTMN.1850to2005.dailyRast
###

###
#This block reads in Reanalysis 2 data for bias-correction.
load(file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTemp_1948to2016.Rdata") #air2mtemp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/tmax_for_scaling.Rdata") #tmax.r

gcm.ext <- extent(tmax.r[[1]])
cal.ext <- extent(air2mtemp[[1948]][[1]])

air2mtemp.max <- list()
air2mtemp.min <- list()
air2mtemp.ave <- list()
for(i in 1:58){
  tmp.max <- list()
  tmp.min <- list()
  tmp.ave <- list()
  for(j in 1:365){
    index <- seq(j+3*(j-1), j+3*(j-1)+3, 1)
    tmp.max[[j]] <- max(air2mtemp[[i+1947]][[index]])
    tmp.min[[j]] <- min(air2mtemp[[i+1947]][[index]])
    tmp.ave[[j]] <- mean(air2mtemp[[i+1947]][[index]])
  }
  air2mtemp.max[[i+1947]] <- tmp.max
  air2mtemp.min[[i+1947]] <- tmp.min
  air2mtemp.ave[[i+1947]] <- tmp.ave
  rm(tmp.max, tmp.min, tmp.ave)
  print(i)
}

rm(air2mtemp, index, i, cal.ext)
save(air2mtemp.max, file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTempMax.Rdata")
save(air2mtemp.min, file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTempMin.Rdata")
save(air2mtemp.ave, file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTempAve.Rdata")
gc()

load(file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTempMax.Rdata")
load(file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTempMin.Rdata")
load(file="/media/zizroc/Extra Drive 1/Data/R environments/air2mTempAve.Rdata")

cal.tmax <- list()
cal.tmin <- list()
cal.tave <- list()
for(i in 1:58){ #Calibration set is over 1948 to 2005 period
  for(j in 1:365){
    tmp1.max  <- air2mtemp.max[[i+1947]][[j]]
    tmp1.min  <- air2mtemp.min[[i+1947]][[j]]
    tmp1.ave  <- air2mtemp.ave[[i+1947]][[j]]
    tmp2.max <- resample(tmp1.max, TREFHTMX.1850to2005.dailyRast[[i+1849+98]][[j]], method="bilinear")
    tmp2.min <- resample(tmp1.min, TREFHTMX.1850to2005.dailyRast[[i+1849+98]][[j]], method="bilinear")
    tmp2.ave <- resample(tmp1.ave, TREFHTMX.1850to2005.dailyRast[[i+1849+98]][[j]], method="bilinear")
    if(j==1){
      tmp3.max <- tmp2.max
      tmp3.min <- tmp2.min
      tmp3.ave <- tmp2.ave
    }
    if(j>1) {
      tmp3.max <- stack(tmp3.max, tmp2.max)
      tmp3.min <- stack(tmp3.min, tmp2.min)
      tmp3.ave <- stack(tmp3.ave, tmp2.ave)
    }
    rm(tmp1.max, tmp1.min, tmp1.ave, tmp2.max, tmp2.min, tmp2.ave)
  }
  cal.tmax[[i+1947]] <- tmp3.max
  cal.tmin[[i+1947]] <- tmp3.min
  cal.tave[[i+1947]] <- tmp3.ave
  rm(tmp3.max, tmp3.min, tmp3.ave)
  print(paste("Calibration data rectified to GCM-scale, for year", i+1947, sep=" "))
}
save(cal.tmax, file="/media/zizroc/Extra Drive 1/Data/R environments/calTmax.Rdata")
save(cal.tmin, file="/media/zizroc/Extra Drive 1/Data/R environments/calTmin.Rdata")
save(cal.tave, file="/media/zizroc/Extra Drive 1/Data/R environments/calTave.Rdata")
rm(air2mtemp.max, air2mtemp.min, air2mtemp.ave)
gc()
###

###
#Block loads Reanalysis 2 data for BC.
load(file="/media/zizroc/Extra Drive 1/Data/R environments/calTmax.Rdata") #cal.tmax
load(file="/media/zizroc/Extra Drive 1/Data/R environments/calTmin.Rdata") #cal.tmin
load(file="/media/zizroc/Extra Drive 1/Data/R environments/calTave.Rdata") #cal.tave
###











###
#Calibration Block
#This makes vectorized GCM and GCM-upscaled Reanalysis 2 data for the quantile mapping algorithm.
# cal.Temp.max <- list()
# cal.Temp.min <- list()
# cal.Temp.ave <- list()
# for(j in 1:58){
#   tmp.max <- list()
#   tmp.min <- list()
#   tmp.ave <- list()
#   for(i in 1:365){
#     # index <- seq(i+3*(i-1), i+3*(i-1)+3, 1)
#     tmp.max[[i]] <- max(cal.Temp[[j+1947]][[i]])
#     tmp.min[[i]] <- min(cal.Temp[[j+1947]][[i]])
#     tmp.ave[[i]] <- mean(cal.Temp[[j+1947]][[i]])
#   }
#   cal.Temp.max[[j+1947]] <- tmp.max
#   cal.Temp.min[[j+1947]] <- tmp.min
#   cal.Temp.ave[[j+1947]] <- tmp.ave
#   rm(tmp.max, tmp.min, tmp.ave)
#   print(j)
# }

gcm.tmax.bias <- list()
gcm.tmin.bias <- list()
cal.tmax.bias <- list()
cal.tmin.bias <- list()
cal.tave.bias <- list()
for(i in 1:365){
  gcm.tmax.m  <- matrix(nrow=58, ncol=20)
  gcm.tmin.m  <- matrix(nrow=58, ncol=20)
  cal.tmax.m  <- matrix(nrow=58, ncol=20)
  cal.tmin.m  <- matrix(nrow=58, ncol=20)
  cal.tave.m  <- matrix(nrow=58, ncol=20)
  for(j in 1:58){
    gcm.tmax.tmp   <- TREFHTMX.1850to2005.dailyRast[[j+1849+98]][[i]]
    gcm.tmin.tmp   <- TREFHTMN.1850to2005.dailyRast[[j+1849+98]][[i]]
    cal.tmax.tmp   <- cal.tmax[[j+1947]][[i]]
    cal.tmin.tmp   <- cal.tmin[[j+1947]][[i]]
    cal.tave.tmp   <- cal.tave[[j+1947]][[i]]
    gcm.tmax.m[j,] <- as.vector(gcm.tmax.tmp)
    gcm.tmin.m[j,] <- as.vector(gcm.tmin.tmp)
    cal.tmax.m[j,] <- as.vector(cal.tmax.tmp)
    cal.tmin.m[j,] <- as.vector(cal.tmin.tmp)
    cal.tave.m[j,] <- as.vector(cal.tave.tmp)
  }
  gcm.tmax.bias[[i]] <- gcm.tmax.m
  gcm.tmin.bias[[i]] <- gcm.tmin.m
  cal.tmax.bias[[i]] <- cal.tmax.m
  cal.tmin.bias[[i]] <- cal.tmin.m
  cal.tave.bias[[i]] <- cal.tave.m
  rm(gcm.tmax.tmp, gcm.tmin.tmp, cal.tmax.tmp, cal.tmin.tmp, cal.tave.tmp, gcm.tmax.m, gcm.tmin.m, cal.tmax.m, cal.tmin.m, cal.tave.m)
  print(paste("GCM and calib bias data for day", i, sep=" "))
}
# save(cal.tmax.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_Tmax_1948to2005.Rdata")
# save(cal.tmin.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_Tmin_1948to2005.Rdata")
# save(cal.tave.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_Tave_1948to2005.Rdata")
# #Ensemble member 002
# save(gcm.tmax.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em002.Rdata")
# save(gcm.tmin.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em002.Rdata")
# #Ensemble member 003
# save(gcm.tmax.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em003.Rdata")
# save(gcm.tmin.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em003.Rdata")
# #Ensemble member 004
# save(gcm.tmax.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em004.Rdata")
# save(gcm.tmin.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em004.Rdata")
#Ensemble member 005
save(gcm.tmax.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em005.Rdata")
save(gcm.tmin.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em005.Rdata")
gc()
#end calibration block
###

###
#Loads vectorized GCM and GCM-upscaled Reanalysis 2 data for quantile mapping algorithm.
load(file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_Tmax_1948to2005.Rdata")
load(file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_Tmin_1948to2005.Rdata")
load(file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_Tave_1948to2005.Rdata")
# #Ensemble member 002
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005.Rdata")
# #Ensemble member 003
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em003.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em003.Rdata")
# #Ensemble member 004
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em004.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em004.Rdata")
#Ensemble member 005
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1948to2005_em005.Rdata")
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1948to2005_em005.Rdata")
###

#Tailor the length of the calibration time-series here
# gcm.tmax.bias_calib <- list()
# gcm.tmin.bias_calib <- list()
# cal.temp.bias_calib <- list()
# for(i in 1:12){
  # tmp.tmax <- gcm.tmax.bias[[i]][-c(seq(1,9,1)),] #removes the first decade from calibration set
  # tmp.tmin <- gcm.tmin.bias[[i]][-c(seq(1,9,1)),]
  # tmp.cali <- cal.temp.bias[[i]][-c(seq(1,9,1)),]
  # 
  # gcm.tmax.bias_calib[[i]] <- tmp.tmax[-c(seq(91,110,1)),] #removes the last 2 decades from calibration set
  # gcm.tmin.bias_calib[[i]] <- tmp.tmin[-c(seq(91,110,1)),]
  # cal.temp.bias_calib[[i]] <- tmp.cali[-c(seq(91,110,1)),]
# }
#If tailoring of calibration time-series is unnecessary, use the following:
gcm.tmax.bias_calib <- gcm.tmax.bias
gcm.tmin.bias_calib <- gcm.tmin.bias
cal.tave.bias_calib <- cal.tave.bias

###
#Quantile Mapping Block - Temperature
#This rescales GCM data distributions to the ranges and means of observation (Reanalysis 2) data.
library(qmap)
BiasCells.tmax <- list()
BiasCells.tmin <- list()
fitQUANT.tave  <- list()
for(i in 1:365){
  biasQUANT.tave <- list()
  biasCells.tmax <- list()
  biasCells.tmin <- list()
  for(j in 1:20){
    mod.tmax <- gcm.tmax.bias_calib[[i]][,j]-272.15
    mod.tmin <- gcm.tmin.bias_calib[[i]][,j]-272.15
    mod.tave <- (mod.tmax + mod.tmin)/2
    obs.tave <- cal.tave.bias_calib[[i]][,j]-272.15
    tmp1  <- fitQmapQUANT(obs.tave, mod.tave, wet.day=FALSE, qstep=0.01)
    tmp2a <- doQmapQUANT(mod.tmax, tmp1)
    tmp2b <- doQmapQUANT(mod.tmin, tmp1)
    biasQUANT.tave[[j]] <- tmp1
    biasCells.tmax[[j]] <- tmp2a
    biasCells.tmin[[j]] <- tmp2b
    rm(tmp1, tmp2a, tmp2b, mod.tmax, mod.tmin, mod.tave, obs.tave)
  }
  fitQUANT.tave[[i]]  <- biasQUANT.tave #Assigned to a list to apply to daily GCM
  BiasCells.tmax[[i]] <- biasCells.tmax
  BiasCells.tmin[[i]] <- biasCells.tmin
  rm(biasCells.tmax, biasCells.tmin, biasQUANT.tave)
}
# #Ensemble member 002
# save(fitQUANT.tave, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT.Rdata")
# save(BiasCells.tmax, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1948to2005.Rdata")
# save(BiasCells.tmin, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1948to2005.Rdata")
# #Ensemble member 003
# save(fitQUANT.tave, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em003.Rdata")
# save(BiasCells.tmax, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1948to2005_em003.Rdata")
# save(BiasCells.tmin, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1948to2005_em003.Rdata")
# #Ensemble member 004
# save(fitQUANT.tave, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em004.Rdata")
# save(BiasCells.tmax, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1948to2005_em004.Rdata")
# save(BiasCells.tmin, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1948to2005_em004.Rdata")
#Ensemble member 005
save(fitQUANT.tave, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em005.Rdata")
save(BiasCells.tmax, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1948to2005_em005.Rdata")
save(BiasCells.tmin, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1948to2005_em005.Rdata")
#end quantile mapping block - temperature
###

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT.Rdata") #fitQUANT.tave
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em003.Rdata") #fitQUANT.tave
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em004.Rdata") #fitQUANT.tave
load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em005.Rdata") #fitQUANT.tave

###
#Bias Correction Block
#This bias-corrects the complete time-series.
gcm.tmax.bias.1850to2005 <- list()
gcm.tmin.bias.1850to2005 <- list()
for(i in 1:365){
  gcm.tmax.m  <- matrix(nrow=156, ncol=20)
  gcm.tmin.m  <- matrix(nrow=156, ncol=20)
  for(j in 1:156){
    gcm.tmax.tmp   <- TREFHTMX.1850to2005.dailyRast[[j+1849]][[i]]
    gcm.tmin.tmp   <- TREFHTMN.1850to2005.dailyRast[[j+1849]][[i]]
    gcm.tmax.m[j,] <- as.vector(gcm.tmax.tmp)
    gcm.tmin.m[j,] <- as.vector(gcm.tmin.tmp)
  }
  gcm.tmax.bias.1850to2005[[i]] <- gcm.tmax.m
  gcm.tmin.bias.1850to2005[[i]] <- gcm.tmin.m
  rm(gcm.tmax.tmp, gcm.tmin.tmp)
}
# #Ensemble member 002
# save(gcm.tmax.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1850to2005.Rdata")
# save(gcm.tmin.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1850to2005.Rdata")
# #Ensemble member 003
# save(gcm.tmax.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1850to2005_em003.Rdata")
# save(gcm.tmin.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1850to2005_em003.Rdata")
# #Ensemble member 004
# save(gcm.tmax.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1850to2005_em004.Rdata")
# save(gcm.tmin.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1850to2005_em004.Rdata")
#Ensemble member 005
save(gcm.tmax.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1850to2005_em005.Rdata")
save(gcm.tmin.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1850to2005_em005.Rdata")
rm(gcm.tmax.m, gcm.tmin.m)
gc()

BiasCells.tmax.1850to2005 <- list()
BiasCells.tmin.1850to2005 <- list()
for(i in 1:365){
  biasCells.tmax <- list()
  biasCells.tmin <- list()
  for(j in 1:20){
    mod.tmax <- gcm.tmax.bias.1850to2005[[i]][,j]-272.15
    mod.tmin <- gcm.tmin.bias.1850to2005[[i]][,j]-272.15
    tmp1a <- doQmapQUANT(mod.tmax, fitQUANT.tave[[i]][[j]])
    tmp1b <- doQmapQUANT(mod.tmin, fitQUANT.tave[[i]][[j]])
    biasCells.tmax[[j]] <- tmp1a
    biasCells.tmin[[j]] <- tmp1b
    rm(tmp1a, tmp1b, mod.tmax, mod.tmin)
  }
  BiasCells.tmax.1850to2005[[i]] <- biasCells.tmax
  BiasCells.tmin.1850to2005[[i]] <- biasCells.tmin
  rm(biasCells.tmax, biasCells.tmin)
}
# #Ensemble member 002
# save(BiasCells.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1850to2005.Rdata")
# save(BiasCells.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1850to2005.Rdata")
# #Ensemble member 003
# save(BiasCells.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1850to2005_em003.Rdata")
# save(BiasCells.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1850to2005_em003.Rdata")
# #Ensemble member 004
# save(BiasCells.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1850to2005_em004.Rdata")
# save(BiasCells.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1850to2005_em004.Rdata")
#Ensemble member 005
save(BiasCells.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1850to2005_em005.Rdata")
save(BiasCells.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1850to2005_em005.Rdata")

#Organise data by day interpolate
GCM.tmax.1850to2005.to_interp <- list()
GCM.tmin.1850to2005.to_interp <- list()
for(i in 1:365){
  tmp.tmax <- do.call(cbind, BiasCells.tmax.1850to2005[[i]])
  tmp.tmin <- do.call(cbind, BiasCells.tmin.1850to2005[[i]])
  rownames(tmp.tmax) <- c(1850:2005)
  rownames(tmp.tmin) <- c(1850:2005)
  GCM.tmax.1850to2005.to_interp[[i]] <- tmp.tmax
  GCM.tmin.1850to2005.to_interp[[i]] <- tmp.tmin
  rm(tmp.tmax, tmp.tmin)
}
# #Ensemble member 002
# save(GCM.tmax.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax.Rdata")
# save(GCM.tmin.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin.Rdata")
# #Ensemble member 003
# save(GCM.tmax.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_em003.Rdata")
# save(GCM.tmin.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_em003.Rdata")
# #Ensemble member 004
# save(GCM.tmax.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_em004.Rdata")
# save(GCM.tmin.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_em004.Rdata")
#Ensemble member 005
save(GCM.tmax.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_em005.Rdata")
save(GCM.tmin.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_em005.Rdata")

#Interpolate
xmin = -115
xmax = -107.5
ymin = 35.05263
ymax = 42.63158
xo = seq(xmin, xmax, length.out=45)
yo = seq(ymin, ymax, length.out=46)
d1 <- expand.grid(x = xo, y = yo)

x1 = seq(xmin, xmax, length.out=4)
y1 = seq(ymin, ymax, length.out=5)
d2 <- expand.grid(x = x1, y = y1)

load(file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata") #tmp.xyz
# tmp.xyz <- rasterToPoints(TREFHTMX.1850to2005.dailyRast[[1850]][[1]])
# save(tmp.xyz, file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata")

##
# #Loads GCM data 
# #Ensemble member 002
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin.Rdata") #GCM.tmin.1850to2005.to_interp
# #Ensemble member 003
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_003.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_003.Rdata") #GCM.tmin.1850to2005.to_interp
# #Ensemble member 004
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_004.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_004.Rdata") #GCM.tmin.1850to2005.to_interp
#Ensemble member 005
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_005.Rdata") #GCM.tmax.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_005.Rdata") #GCM.tmin.1850to2005.to_interp
##

library(akimia)
GCM.tmax.1850to2005.Interp <- list()
for(j in 1:365){
  Tmp <- list()
  for(i in 1:156){
    # tmp.ak <- interp(tmp.xyz[,1]-360, tmp.xyz[,2], GCM.tmax.1850to2005.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak <- interp(d2[,1], d2[,2], GCM.tmax.1850to2005.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak.ras <- raster(tmp.ak)
    Tmp[[i]] <- tmp.ak.ras
  }
  GCM.tmax.1850to2005.Interp[[j]] <- Tmp
  rm(Tmp, tmp.ak, tmp.ak.ras)
  print(paste("Chug, chug GCM.tmax: need 365 and I'm at", j, sep=" "))
}
# #Ensemble member 002
# save(GCM.tmax.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX.Rdata")
# #Ensemble member 003
# save(GCM.tmax.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_em003.Rdata")
# #Ensemble member 004
# save(GCM.tmax.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_em004.Rdata")
#Ensemble member 005
save(GCM.tmax.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_em005.Rdata")

rm(GCM.tmax.1850to2005.to_interp, GCM.tmax.1850to2005.Interp)
gc()
GCM.tmin.1850to2005.Interp <- list()
for(j in 1:365){
  Tmp <- list()
  for(i in 1:156){
    # tmp.ak <- interp(tmp.xyz[,1]-360, tmp.xyz[,2], GCM.tmin.1850to2005.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak <- interp(d2[,1], d2[,2], GCM.tmin.1850to2005.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak.ras <- raster(tmp.ak)
    Tmp[[i]] <- tmp.ak.ras
  }
  GCM.tmin.1850to2005.Interp[[j]] <- Tmp
  rm(Tmp, tmp.ak, tmp.ak.ras)
  print(paste("Chug, chug GCM.tmin: need 365 and I'm at", j, sep=" "))
}
# #Ensemble member 002
# save(GCM.tmin.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN.Rdata")
# #Ensemble member 003
# save(GCM.tmin.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_em003.Rdata")
# #Ensemble member 004
# save(GCM.tmin.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_em004.Rdata")
#Ensemble member 005
save(GCM.tmin.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_em005.Rdata")

rm(GCM.tmin.1850to2005.to_interp, GCM.tmin.1850to2005.Interp)
gc()
#end bias-correction block
###

###
#PRISM Block
#Uses PRISM station-based interpolated regression model data for spatial downscaling.
###
xmin = -115
xmax = -107.5
ymin = 35.05263
ymax = 42.63158
prism.ext <- raster()
extent(prism.ext) <- c(xmin, xmax, ymin, ymax)

#The following re-writes PRISM data without aggregating it to GCM-scale
tmax.dir <- dir("/media/zizroc/My Passport/Data/PRISMdata/recent_daily/tmax", full.names=TRUE)
tmin.dir <- dir("/media/zizroc/My Passport/Data/PRISMdata/recent_daily/tmin", full.names=TRUE)
prec.dir <- dir("/media/zizroc/My Passport/Data/PRISMdata/recent_daily/ppt", full.names=TRUE)

pris.ppt     <- list()
pris.max.TS  <- list()
pris.min.TS  <- list()
pris.mean.TS <- list()
difference   <- list()
for(i in 1:25){ #from 1981 to 2005 CE
  #maximum daily temperatures
  bilfiles  <- dir(tmax.dir[i], pattern="bil.bil", ignore.case=FALSE, full.name=TRUE)
  auxfiles  <- dir(tmax.dir[i], pattern="bil.aux", ignore.case=FALSE, full.name=TRUE)
  pris.tmax <- bilfiles[!bilfiles %in% auxfiles] #only BIL files
  rm(bilfiles, auxfiles)

  #minimum daily temperatures
  bilfiles  <- dir(tmin.dir[i], pattern="bil.bil", ignore.case=FALSE, full.name=TRUE)
  auxfiles  <- dir(tmin.dir[i], pattern="bil.aux", ignore.case=FALSE, full.name=TRUE)
  pris.tmin <- bilfiles[!bilfiles %in% auxfiles] #only BIL files
  rm(bilfiles, auxfiles)
  
  # #daily precipitation
  # bilfiles  <- dir(prec.dir[i], pattern="bil.bil", ignore.case=FALSE, full.name=TRUE)
  # auxfiles  <- dir(prec.dir[i], pattern="bil.aux", ignore.case=FALSE, full.name=TRUE)
  # pris.tmp  <- bilfiles[!bilfiles %in% auxfiles] #only BIL files
  # rm(bilfiles, auxfiles)
  
  tmp3 <- list()
  for(j in 1:365){
    tmp1 <- raster(pris.tmax[j])
    tmp2 <- crop(tmp1, prism.ext)
    tmp3[[j]] <- tmp2
    rm(tmp1, tmp2)
  }
  pris.max.TS[[i]] <- tmp3
  rm(tmp3)
  
  tmp3 <- list()
  for(j in 1:365){
    tmp1 <- raster(pris.tmin[j])
    tmp2 <- crop(tmp1, prism.ext)
    tmp3[[j]] <- tmp2
    rm(tmp1, tmp2)
  }
  pris.min.TS[[i]] <- tmp3
  rm(tmp3)

  # pris.mean.TS[[i]] <- (pris.min.TS[[i]] + pris.max.TS[[i]])/2
  # difference[[i]] <- pris.max.TS[[i]] - pris.min.TS[[i]]
  
  # for(j in 1:length(pris.tmp)){
  #   #for(j in 1:1440){
  #   tmp1 <- raster(pris.tmp[j])
  #   tmp2 <- crop(tmp1, prism.ext)
  #   if(j==1){tmp3 <- tmp2}
  #   if(j>1) {tmp3 <- stack(tmp3, tmp2)}
  #   rm(tmp1, tmp2)
  # }
  # pris.ppt[[i]] <- tmp3
  # rm(tmp3)
  print(paste("PRISM data for year", i+1980, sep=" "))
}
rm(tmax.dir, tmin.dir)
save(pris.max.TS, file="/media/zizroc/Extra Drive 1/Data/R environments/tmaxTS_IIASA.Rdata")
save(pris.min.TS, file="/media/zizroc/Extra Drive 1/Data/R environments/tminTS_IIASA.Rdata")
# save(difference, file="/media/zizroc/Extra Drive 1/Data/R environments/difference.Rdata")
# save(pris.ppt, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMppt_daily.Rdata")
gc()
####
################################
## Normalisation sub-block    ##
################################
#
pris.ave.TS <- list()
pris.dif.TS <- list()
for(i in 1:25){
  for(j in 1:365){
    pris.ave.TS[[j+(i-1)*364]] <- mean(pris.max.TS[[i]][[j]], pris.min.TS[[i]][[j]])
    pris.dif.TS[[j+(i-1)*364]] <- pris.max.TS[[i]][[j]] - pris.min.TS[[i]][[j]]
  }
  print(i)
}
save(pris.ave.TS, file="/media/zizroc/Extra Drive 1/Data/R environments/TSmean.Rdata")
save(pris.dif.TS, file="/media/zizroc/Extra Drive 1/Data/R environments/TSdiff.Rdata")

#This is the mean PRISM data for each of 365 days over normalisation period (1981-2005 CE)
pris.max.sampl <- list()
pris.min.sampl <- list()
pris.ave.sampl <- list()
pris.dif.sampl <- list()
for(i in 1:365){
  tmp.pris.max.sampl <- list()
  tmp.pris.min.sampl <- list()
  tmp.pris.ave.sampl <- list()
  tmp.pris.dif.sampl <- list()
  for(j in 1:25){
    tmp.pris.max.sampl[[j]] <- pris.max.TS[[j]][[i]]
    tmp.pris.min.sampl[[j]] <- pris.min.TS[[j]][[i]]
    tmp.pris.ave.sampl[[j]] <- mean(pris.max.TS[[j]][[i]], pris.min.TS[[j]][[i]])
    tmp.pris.dif.sampl[[j]] <- pris.max.TS[[j]][[i]] - pris.min.TS[[j]][[i]]
  }
  pris.max.sampl[[i]] <- tmp.pris.max.sampl
  pris.min.sampl[[i]] <- tmp.pris.min.sampl
  pris.ave.sampl[[i]] <- tmp.pris.ave.sampl
  pris.dif.sampl[[i]] <- tmp.pris.dif.sampl
  rm(tmp.pris.max.sampl, tmp.pris.min.sampl, tmp.pris.ave.sampl, tmp.pris.dif.sampl)
  print(i)
}
save(pris.max.sampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISmaxSampl.Rdata")
save(pris.min.sampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISminSampl.Rdata")
save(pris.ave.sampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISaveSampl.Rdata")
save(pris.dif.sampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISdifSampl.Rdata")
gc()
###

###
#Loads PRISM data.
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISmaxSampl.Rdata") #pris.max.sampl
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISminSampl.Rdata") #pris.min.sampl
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISaveSampl.Rdata") #pris.ave.sampl
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISdifSampl.Rdata") #pris.dif.sampl
###

#Makes stacks of PRISM rasters for each temperature case from which to randomly draw samples.
#Randomly pull days from each of the 25 years.
pris.max.sampl.day <- list() #single raster for each of 25 years for each of 365 days
pris.min.sampl.day <- list()
pris.ave.sampl.day <- list()
pris.dif.sampl.day <- list()
pris.max.sampl.stk <- list() #raster stack containing all rasters for 25 years for each of 365 days
pris.min.sampl.stk <- list()
pris.ave.sampl.stk <- list()
pris.dif.sampl.stk <- list()
for(i in 1:365){
  j = sample(c(1:25), 1) #random sample from 1:25 years generated
  pris.max.sampl.day[[i]] <- pris.max.sampl[[i]][[j]]
  pris.min.sampl.day[[i]] <- pris.min.sampl[[i]][[j]]
  pris.ave.sampl.day[[i]] <- pris.ave.sampl[[i]][[j]]
  pris.dif.sampl.day[[i]] <- pris.dif.sampl[[i]][[j]]
  
  pris.max.sampl.stk[[i]] <- stack(pris.max.sampl[[i]])
  pris.min.sampl.stk[[i]] <- stack(pris.min.sampl[[i]])
  pris.ave.sampl.stk[[i]] <- stack(pris.ave.sampl[[i]])
  pris.dif.sampl.stk[[i]] <- stack(pris.dif.sampl[[i]])
  print(i)
}

pr.max <- list()
pr.min <- list()
pr.ave <- list()
pr.dif <- list()
for(i in 1:365){
  pr.max[[i]] <- mean(pris.max.sampl.stk[[i]])
  pr.min[[i]] <- mean(pris.min.sampl.stk[[i]])
  pr.ave[[i]] <- mean(pris.ave.sampl.stk[[i]])
  pr.dif[[i]] <- mean(pris.dif.sampl.stk[[i]])
}

save(pr.max, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tmaxStack.Rdata")
save(pr.min, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tminStack.Rdata")
save(pr.ave, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_taveStack.Rdata")
save(pr.dif, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tdifStack.Rdata")

rm(pr.max, pr.min, pris.max.sampl.stk, pris.min.sampl.stk, pris.ave.sampl.stk, pris.dif.sampl.stk, pris.max.sampl.day, pris.min.sampl.day, pris.ave.sampl.day, pris.dif.sampl.day, pris.max.sampl, pris.min.sampl, pris.ave.sampl, pris.dif.sampl)

#Dump excess memory




xmin = -115
xmax = -107.5
ymin = 35.05263
ymax = 42.63158
prism.ext <- raster()
extent(prism.ext) <- c(xmin, xmax, ymin, ymax)
#The following re-writes PRISM data without aggregating it to GCM-scale
tmax.dir <- dir("/media/zizroc/My Passport/Data/PRISMdata/recent_daily/tmax", full.names=TRUE)
tmin.dir <- dir("/media/zizroc/My Passport/Data/PRISMdata/recent_daily/tmin", full.names=TRUE)
pris.tmax <- list()
pris.tmin <- list()
for(i in 1:34){ #from 1981 to 2014 CE
  #daily maximum temperature
  bilfiles  <- dir(tmax.dir[i], pattern="bil.bil", ignore.case=FALSE, full.name=TRUE)
  auxfiles  <- dir(tmax.dir[i], pattern="bil.aux", ignore.case=FALSE, full.name=TRUE)
  tmp.tmax  <- bilfiles[!bilfiles %in% auxfiles] #only BIL files
  rm(bilfiles, auxfiles)

  #daily minimum temperature
  bilfiles  <- dir(tmin.dir[i], pattern="bil.bil", ignore.case=FALSE, full.name=TRUE)
  auxfiles  <- dir(tmin.dir[i], pattern="bil.aux", ignore.case=FALSE, full.name=TRUE)
  tmp.tmin  <- bilfiles[!bilfiles %in% auxfiles] #only BIL files
  rm(bilfiles, auxfiles)

  for(j in 1:365){
    tmp1 <- raster(tmp.tmax[j])
    tmp2 <- crop(tmp1, prism.ext)
    if(j==1){tmp3 <- tmp2}
    if(j>1) {tmp3 <- stack(tmp3, tmp2)}
    rm(tmp1, tmp2)
  }
  pris.tmax[[i+1980]] <- tmp3
  rm(tmp3)
  print(paste("Building PRISM TMAX data for SD. It's", sep=" ", Sys.time(), "and I'm processing year", i+1980, "/ 2014."))
      
  for(j in 1:365){
    tmp1 <- raster(tmp.tmin[j])
    tmp2 <- crop(tmp1, prism.ext)
    if(j==1){tmp3 <- tmp2}
    if(j>1) {tmp3 <- stack(tmp3, tmp2)}
    rm(tmp1, tmp2)
  }
  pris.tmin[[i+1980]] <- tmp3
  rm(tmp3)
  print(paste("Building PRISM TMIN data for SD. It's", sep=" ", Sys.time(), "and I'm processing year", i+1980, "/ 2014."))
}
rm(prec.dir)
save(pris.tmax, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMtmax_IIASA.Rdata")
save(pris.tmin, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMtmin_IIASA.Rdata")
gc()

################################
## PRISM resampling sub-block ##
################################
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMtmax_IIASA.Rdata") #pris.tmax
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMtmin_IIASA.Rdata") #pris.tmin

load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax.Rdata") #GCM.tmax.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005.Rdata") #TREFHTMN.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin.Rdata") #GCM.tmin.1850to2005.to_interp
#Ensemble member 003
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em003.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_003.Rdata") #GCM.tmax.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em003.Rdata") #TREFHTMN.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_003.Rdata") #GCM.tmin.1850to2005.to_interp

#Ensemble member 004
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em004.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_em004.Rdata") #GCM.tmax.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em004.Rdata") #TREFHTMN.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_em004.Rdata") #GCM.tmin.1850to2005.to_interp

#Create a library of GCM-scaled PRISM raster data

gcm.ext <- extent(TREFHTMX.1850to2005.dailyRast[[1950]][[180]])
tmp.mod <- TREFHTMX.1850to2005.dailyRast[[1950]][[180]]
mod.ext <- c(xmin(gcm.ext)-360, xmax(gcm.ext)-360, ymin(gcm.ext), ymax(gcm.ext))
extent(tmp.mod) <- mod.ext
values(tmp.mod) <- NA
pris.tmax.resampl <- list()
pris.tmin.resampl <- list()
for(i in 1:34){
  tmp <- list()
  for(j in 1:365){
    tmp.obs  <- pris.tmax[[i+1980]][[j]]
    tmp[[j]] <- resample(tmp.obs, tmp.mod, method="bilinear")
    rm(tmp.obs)
  }
  pris.tmax.resampl[[i+1980]] <- tmp
  rm(tmp)
  
  tmp <- list()
  for(j in 1:365){
    tmp.obs  <- pris.tmin[[i+1980]][[j]]
    tmp[[j]] <- resample(tmp.obs, tmp.mod, method="bilinear")
    rm(tmp.obs)
  }
  pris.tmin.resampl[[i+1980]] <- tmp
  rm(tmp)
  
  print(paste("Building resampled PRISM tmax and tmin lists. It's", sep=" ", Sys.time(), "and I'm processing year", i+1980, "/ 2014."))
}
save(pris.tmax.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMAX_1981to2005.Rdata")
save(pris.tmin.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMIN_1981to2005.Rdata")

#Ensemble member 003
save(pris.tmax.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMAX_1981to2005_em003.Rdata")
save(pris.tmin.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMIN_1981to2005_em003.Rdata")
#Ensemble member 004
save(pris.tmax.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMAX_1981to2005_em004.Rdata")
save(pris.tmin.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMIN_1981to2005_em004.Rdata")
###
#Pattern Selector Block
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMAX_1981to2005.Rdata") #pris.tmax.resampl
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledTMIN_1981to2005.Rdata") #pris.tmin.resampl
library(hydroGOF)
#This selects for the PRISM temp raster whose difference from the GCM raster is a minimum. This PRISM raster is then used as the most similar precipiation distribution to the GCM.
norm <- function(x) sqrt(sum(x^2))
GCM.PRISM.tmax.matches <- list()
for(i in 1:365){
  for(j in 1:156){
    Tmp <- data.frame()
    for(k in 1:34){
      tmp.obs <- as.vector(pris.tmax.resampl[[k+1980]][[i]])
      tmp.mod <- GCM.tmax.1850to2005.to_interp[[i]][j,]
      tmp.cor <- cor(tmp.mod, tmp.obs)
      tmp.rms <- rmse(tmp.mod, tmp.obs)
      if(is.na(tmp.cor) == TRUE){tmp.cor <- 0}
      tmp.all <- cbind(tmp.cor, tmp.rms, k+1980, j+1849, i)
      if(k==1){Tmp <- tmp.all}
      if(k >1){Tmp <- rbind(Tmp, tmp.all)}
      rm(tmp.obs, tmp.mod, tmp.cor, tmp.rms, tmp.all)
    }
    Tmp <- as.data.frame(Tmp)
    names(Tmp) <- c("PEARSON_CORR", "RMSE", "PRISM.YEAR", "GCM.YEAR", "DAY")
    Tmp.max <- Tmp[which.max(Tmp[,1]),]
    if(Tmp.max[,1] == 0) {Tmp.max <- Tmp[which.min(Tmp[,2]),]} #If the correlation test does not apply, select the row with the lowest RMSE.
    
    if(j==1){TMP.max <- Tmp.max}
    if(j >1){TMP.max <- rbind(TMP.max, Tmp.max)}
    rm(Tmp.max)
  }
  GCM.PRISM.tmax.matches[[i]] <- TMP.max
  print(paste("Finding similar rasters to GCM. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
  rm(TMP.max)
}
save(GCM.PRISM.tmax.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesTMAX.Rdata")
save(GCM.PRISM.tmax.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesTMAX_em003.Rdata")
save(GCM.PRISM.tmax.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesTMAX_em004.Rdata")

GCM.PRISM.tmin.matches <- list()
for(i in 1:365){
  for(j in 1:156){
    Tmp <- data.frame()
    for(k in 1:34){
      tmp.obs <- as.vector(pris.tmin.resampl[[k+1980]][[i]])
      tmp.mod <- GCM.tmin.1850to2005.to_interp[[i]][j,]
      tmp.cor <- cor(tmp.mod, tmp.obs)
      tmp.rms <- rmse(tmp.mod, tmp.obs)
      if(is.na(tmp.cor) == TRUE){tmp.cor <- 0}
      tmp.all <- cbind(tmp.cor, tmp.rms, k+1980, j+1849, i)
      if(k==1){Tmp <- tmp.all}
      if(k >1){Tmp <- rbind(Tmp, tmp.all)}
      rm(tmp.obs, tmp.mod, tmp.cor, tmp.rms, tmp.all)
    }
    Tmp <- as.data.frame(Tmp)
    names(Tmp) <- c("PEARSON_CORR", "RMSE", "PRISM.YEAR", "GCM.YEAR", "DAY")
    Tmp.max <- Tmp[which.max(Tmp[,1]),]
    if(Tmp.max[,1] == 0) {Tmp.max <- Tmp[which.min(Tmp[,2]),]} #If the correlation test does not apply, select the row with the lowest RMSE.
    
    if(j==1){TMP.max <- Tmp.max}
    if(j >1){TMP.max <- rbind(TMP.max, Tmp.max)}
    rm(Tmp.max)
  }
  GCM.PRISM.tmin.matches[[i]] <- TMP.max
  print(paste("Finding similar rasters to GCM. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
  rm(TMP.max)
}
save(GCM.PRISM.tmin.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesTMIN.Rdata")
save(GCM.PRISM.tmin.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesTMIN_em003.Rdata")
save(GCM.PRISM.tmin.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesTMIN_em004.Rdata")
#end pattern selector block
###


load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005.Rdata") #TREFHTMN.1850to2005.dailyRast

load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em003.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em003.Rdata") #TREFHTMN.1850to2005.dailyRast

load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1850to2005_em004.Rdata") #TREFHTMX.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1850to2005_em004.Rdata") #TREFHTMN.1850to2005.dailyRast
###
#Temperature Block
#Makes PRISM-scale max and min temperature bounds from the GCM; i.e., BCSD CESM for TREFHTMX and TREFHTMN. 
library(raster)

load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tmaxStack.Rdata") #pr.max
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tminStack.Rdata") #pr.min
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tdifStack.Rdata") #pr.dif
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX.Rdata") #GCM.tmin.1850to2005.Interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN.Rdata") #GCM.tmin.1850to2005.Interp

#Ensemble member 003
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_em003.Rdata") #GCM.tmin.1850to2005.Interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_em003.Rdata") #GCM.tmin.1850to2005.Interp
#Ensemble member 004
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_em004.Rdata") #GCM.tmin.1850to2005.Interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_em004.Rdata") #GCM.tmin.1850to2005.Interp

#GCM.tmax.1850to2005 <- list()
GCM.tmin.1850to2005 <- list()
for(i in 1:156){
    #Tmp.max <- list()
    Tmp.min <- list()
    for(j in 1:365){
      tmp.max <- resample(GCM.tmax.1850to2005.Interp[[j]][[i]], pr.max[[j]], method="bilinear")
      tmp.min <- resample(GCM.tmin.1850to2005.Interp[[j]][[i]], pr.min[[j]], method="bilinear")
      tmp.av1 <- (tmp.max + tmp.min)/2
      tmp.av2 <- calc(stack(pr.min[[j]], pr.max[[j]]), fun=mean)
      tmp.agg <- aggregate(tmp.av2, c(44,44))
      tmp.ag1 <- resample(tmp.agg, tmp.av2)
      tmp.dif <- tmp.av1 - tmp.ag1
      #Tmp.max[[j]] <- pr.max[[j]] + tmp.dif
      Tmp.min[[j]] <- pr.min[[j]] + tmp.dif
      # #Tmp.max[[j]] <- tmp.max + pr.max[[j]] - max(na.omit(values(tmp.max)))
      # Tmp.min[[j]] <- tmp.min + pr.min[[j]] - max(na.omit(values(tmp.min)))
      rm(tmp.max, tmp.min, tmp.av1, tmp.av2, tmp.agg, tmp.ag1, tmp.dif)
  }
  #GCM.tmax.1850to2005[[i]] <- Tmp.max
  GCM.tmin.1850to2005[[i]] <- Tmp.min
  rm(Tmp.min)
  gc()
  print(paste("Patience. It's", sep=" ", Sys.time(), "and I'm processing year", i+1849, "/ 2005."))
}
#save(GCM.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1850to2005.Rdata")
#save(GCM.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1850to2005.Rdata")
#Ensemble Member 003
#save(GCM.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1850to2005_em003.Rdata")
#save(GCM.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1850to2005_em003.Rdata")
#Ensemble Member 003
#save(GCM.tmax.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1850to2005_em004.Rdata")
save(GCM.tmin.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1850to2005_em004.Rdata")
#end temperature block
###

####
#Maximum temperature
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.TREFHTMX.08500101-18491231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.TREFHTMX.08500101-18491231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.TREFHTMX.08500101-18491231.nc")
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.TREFHTMX.08500101-18491231.nc")
dname <- "TREFHTMX"
#Minimum temperature
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.TREFHTMN.08500101-18491231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.TREFHTMN.08500101-18491231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.TREFHTMN.08500101-18491231.nc")
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.TREFHTMN.08500101-18491231.nc")
dname <- "TREFHTMN"

#Variables
lon    <- ncvar_get(tmp.nc, "lon")
nlon   <- dim(lon)
lat    <- ncvar_get(tmp.nc, "lat")
nlat   <- dim(lat)
dat    <- ncvar_get(tmp.nc, "date") #current date (YYYMMDD)
ndat   <- dim(dat)
t      <- ncvar_get(tmp.nc, "time")
tunits <- ncatt_get(tmp.nc, "time", "units")
nt     <- dim(t)

k = 2
ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, k*73000+1), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
#ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, 146001), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
#ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, 1), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
#ts.array  <- ncvar_get(tmp.nc, dname) #this file is enormous (e.g., reset memory.size)
dlname    <- ncatt_get(tmp.nc ,dname, "long_name")
dunits    <- ncatt_get(tmp.nc, dname, "units")
fillvalue <- ncatt_get(tmp.nc, dname, "_FillValue")
nc_close(tmp.nc)
rm(tmp.nc)
gc()

tmax.r <- list()
nt=73000
for(i in 1:nt){
  tmp <- raster(t(ts.array[99:102, 71:67, i]))
  extent(tmp) <- c(lon[99], lon[102], lat[67], lat[71])
  tmax.r[[i]] <- tmp
}
rm(ts.array)
gc()

# TREFHTMX.0850to1049.dailyRast <- list()
# TREFHTMX.1050to1249.dailyRast <- list()
TREFHTMX.1250to1449.dailyRast <- list()
n=200
for(i in 1:n){
  index <- seq(i+364*(i-1), i+364*(i-1)+364, 1)
  # TREFHTMX.0850to1049.dailyRast[[i+849]] <- tmax.r[index]
  # TREFHTMX.1050to1249.dailyRast[[i+1049]] <- tmax.r[index]
  TREFHTMX.1250to1449.dailyRast[[i+1249]] <- tmax.r[index]
}
rm(tmax.r, dname, index, n, nt, dlname, dunits, fillvalue, i, dat, ndat, nlat, nlon, t, tunits, tmp)
#Saves a list of daily GCM values
#save(TREFHTMX.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_0850to1049.Rdata")
# save(TREFHTMX.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1050to1249.Rdata")
#save(TREFHTMX.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1250to1449.Rdata")

#save(TREFHTMX.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_0850to1049_em003.Rdata")
#save(TREFHTMX.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1050to1249_em003.Rdata")
# save(TREFHTMX.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1250to1449_em003.Rdata")

#save(TREFHTMX.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_0850to1049_em004.Rdata")
# save(TREFHTMX.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1050to1249_em004.Rdata")
# save(TREFHTMX.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1250to1449_em004.Rdata")

# save(TREFHTMX.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_0850to1049_em005.Rdata")
# save(TREFHTMX.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1050to1249_em005.Rdata")
save(TREFHTMX.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMX_1250to1449_em005.Rdata")
gc()

tmin.r <- list()
nt=73000
for(i in 1:nt){
  tmp <- raster(t(ts.array[99:102, 71:67, i]))
  extent(tmp) <- c(lon[99], lon[102], lat[67], lat[71])
  tmin.r[[i]] <- tmp
}
rm(ts.array)
gc()

# TREFHTMN.0850to1049.dailyRast <- list()
# TREFHTMN.1050to1249.dailyRast <- list()
TREFHTMN.1250to1449.dailyRast <- list()
n=200
for(i in 1:n){
  index <- seq(i+364*(i-1), i+364*(i-1)+364, 1)
  # TREFHTMN.0850to1049.dailyRast[[i+849]] <- tmin.r[index]
  # TREFHTMN.1050to1249.dailyRast[[i+1049]] <- tmin.r[index]
 TREFHTMN.1250to1449.dailyRast[[i+1249]] <- tmin.r[index]
}
rm(tmin.r, dname, index, n, nt, dlname, dunits, fillvalue, i, dat, ndat, nlat, nlon, t, tunits, tmp)
#Saves a list of daily GCM values
#save(TREFHTMN.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_0850to1049.Rdata")
# save(TREFHTMN.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1050to1249.Rdata")
#save(TREFHTMN.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1250to1449.Rdata")

# save(TREFHTMN.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_0850to1049_em003.Rdata")
# save(TREFHTMN.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1050to1249_em003.Rdata")
# save(TREFHTMN.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1250to1449_em003.Rdata")

# save(TREFHTMN.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_0850to1049_em004.Rdata")
# save(TREFHTMN.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1050to1249_em004.Rdata")
# save(TREFHTMN.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1250to1449_em004.Rdata")

# save(TREFHTMN.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_0850to1049_em005.Rdata")
# save(TREFHTMN.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1050to1249_em005.Rdata")
save(TREFHTMN.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/TREFHTMN_1250to1449_em005.Rdata")
gc()

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT.Rdata") #fitQUANT.tave
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em003.Rdata") #fitQUANT.tave
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em004.Rdata") #fitQUANT.tave
load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_em005.Rdata") #fitQUANT.tave

#Bias-correct complete time-series
# gcm.tmax.bias.0850to1049 <- list()
# gcm.tmin.bias.0850to1049 <- list()
# gcm.tmax.bias.1050to1249 <- list()
# gcm.tmin.bias.1050to1249 <- list()
gcm.tmax.bias.1250to1449 <- list()
gcm.tmin.bias.1250to1449 <- list()
for(i in 1:365){
  gcm.tmax.m  <- matrix(nrow=200, ncol=20)
  gcm.tmin.m  <- matrix(nrow=200, ncol=20)
  for(j in 1:200){
    # gcm.tmax.tmp   <- TREFHTMX.0850to1049.dailyRast[[j+849]][[i]]
    # gcm.tmin.tmp   <- TREFHTMN.0850to1049.dailyRast[[j+849]][[i]]
    # gcm.tmax.tmp   <- TREFHTMX.1050to1249.dailyRast[[j+1049]][[i]]
    # gcm.tmin.tmp   <- TREFHTMN.1050to1249.dailyRast[[j+1049]][[i]]
    gcm.tmax.tmp   <- TREFHTMX.1250to1449.dailyRast[[j+1249]][[i]]
    gcm.tmin.tmp   <- TREFHTMN.1250to1449.dailyRast[[j+1249]][[i]]
    gcm.tmax.m[j,] <- as.vector(gcm.tmax.tmp)
    gcm.tmin.m[j,] <- as.vector(gcm.tmin.tmp)
  }
  # gcm.tmax.bias.0850to1049[[i]] <- gcm.tmax.m
  # gcm.tmin.bias.0850to1049[[i]] <- gcm.tmin.m
  # gcm.tmax.bias.1050to1249[[i]] <- gcm.tmax.m
  # gcm.tmin.bias.1050to1249[[i]] <- gcm.tmin.m
  gcm.tmax.bias.1250to1449[[i]] <- gcm.tmax.m
  gcm.tmin.bias.1250to1449[[i]] <- gcm.tmin.m
  rm(gcm.tmax.tmp, gcm.tmin.tmp)
  print(paste("It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
}
# save(gcm.tmax.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_0850to1049.Rdata")
# save(gcm.tmin.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_0850to1049.Rdata")
# save(gcm.tmax.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1050to1249.Rdata")
# save(gcm.tmin.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1050to1249.Rdata")
# save(gcm.tmax.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1250to1449.Rdata")
# save(gcm.tmin.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1250to1449.Rdata")

# save(gcm.tmax.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_0850to1049_em003.Rdata")
# save(gcm.tmin.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_0850to1049_em003.Rdata")
# save(gcm.tmax.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1050to1249_em003.Rdata")
# save(gcm.tmin.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1050to1249_em003.Rdata")
# save(gcm.tmax.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1250to1449_em003.Rdata")
# save(gcm.tmin.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1250to1449_em003.Rdata")

# save(gcm.tmax.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_0850to1049_em004.Rdata")
# save(gcm.tmin.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_0850to1049_em004.Rdata")
# save(gcm.tmax.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1050to1249_em004.Rdata")
# save(gcm.tmin.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1050to1249_em004.Rdata")
# save(gcm.tmax.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1250to1449_em004.Rdata")
# save(gcm.tmin.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1250to1449_em004.Rdata")

# save(gcm.tmax.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_0850to1049_em005.Rdata")
# save(gcm.tmin.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_0850to1049_em005.Rdata")
# save(gcm.tmax.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1050to1249_em005.Rdata")
# save(gcm.tmin.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1050to1249_em005.Rdata")
save(gcm.tmax.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMAX_1250to1449_em005.Rdata")
save(gcm.tmin.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_TMIN_1250to1449_em005.Rdata")
rm(gcm.tmax.m, gcm.tmin.m)
gc()

# BiasCells.tmax.0850to1049 <- list()
# BiasCells.tmin.0850to1049 <- list()
# BiasCells.tmax.1050to1249 <- list()
# BiasCells.tmin.1050to1249 <- list()
BiasCells.tmax.1250to1449 <- list()
BiasCells.tmin.1250to1449 <- list()
for(i in 1:365){
  biasCells.tmax <- list()
  biasCells.tmin <- list()
  for(j in 1:20){
    # mod.tmax <- gcm.tmax.bias.0850to1049[[i]][,j]-272.15
    # mod.tmin <- gcm.tmin.bias.0850to1049[[i]][,j]-272.15
    # mod.tmax <- gcm.tmax.bias.1050to1249[[i]][,j]-272.15
    # mod.tmin <- gcm.tmin.bias.1050to1249[[i]][,j]-272.15
    mod.tmax <- gcm.tmax.bias.1250to1449[[i]][,j]-272.15
    mod.tmin <- gcm.tmin.bias.1250to1449[[i]][,j]-272.15
    tmp1a <- doQmapQUANT(mod.tmax, fitQUANT.tave[[i]][[j]])
    tmp1b <- doQmapQUANT(mod.tmin, fitQUANT.tave[[i]][[j]])
    biasCells.tmax[[j]] <- tmp1a
    biasCells.tmin[[j]] <- tmp1b
    rm(tmp1a, tmp1b, mod.tmax, mod.tmin)
  }
  # BiasCells.tmax.0850to1049[[i]] <- biasCells.tmax
  # BiasCells.tmin.0850to1049[[i]] <- biasCells.tmin
  # BiasCells.tmax.1050to1249[[i]] <- biasCells.tmax
  # BiasCells.tmin.1050to1249[[i]] <- biasCells.tmin
  BiasCells.tmax.1250to1449[[i]] <- biasCells.tmax
  BiasCells.tmin.1250to1449[[i]] <- biasCells.tmin
  print(paste("It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
  rm(biasCells.tmax, biasCells.tmin)
}
# save(BiasCells.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_0850to1049.Rdata")
# save(BiasCells.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_0850to1049.Rdata")
# save(BiasCells.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1050to1249.Rdata")
# save(BiasCells.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1050to1249.Rdata")
#save(BiasCells.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1250to1449.Rdata")
#save(BiasCells.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1250to1449.Rdata")

# save(BiasCells.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_0850to1049_em003.Rdata")
# save(BiasCells.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_0850to1049_em003.Rdata")
# save(BiasCells.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1050to1249_em003.Rdata")
# save(BiasCells.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1050to1249_em003.Rdata")
# save(BiasCells.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1250to1449_em003.Rdata")
# save(BiasCells.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1250to1449_em003.Rdata")

# save(BiasCells.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_0850to1049_em004.Rdata")
# save(BiasCells.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_0850to1049_em004.Rdata")
# save(BiasCells.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1050to1249_em004.Rdata")
# save(BiasCells.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1050to1249_em004.Rdata")
# save(BiasCells.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1250to1449_em004.Rdata")
# save(BiasCells.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1250to1449_em004.Rdata")

# save(BiasCells.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_0850to1049_em005.Rdata")
# save(BiasCells.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_0850to1049_em005.Rdata")
# save(BiasCells.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1050to1249_em005.Rdata")
# save(BiasCells.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1050to1249_em005.Rdata")
save(BiasCells.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMAX_1250to1449_em005.Rdata")
save(BiasCells.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_TMIN_1250to1449_em005.Rdata")


#Organise data by day interpolate
# GCM.tmax.0850to1049.to_interp <- list()
# GCM.tmin.0850to1049.to_interp <- list()
# GCM.tmax.1050to1249.to_interp <- list()
# GCM.tmin.1050to1249.to_interp <- list()
GCM.tmax.1250to1449.to_interp <- list()
GCM.tmin.1250to1449.to_interp <- list()
for(i in 1:365){
  # tmp.tmax <- do.call(cbind, BiasCells.tmax.0850to1049[[i]])
  # tmp.tmin <- do.call(cbind, BiasCells.tmin.0850to1049[[i]])
  # tmp.tmax <- do.call(cbind, BiasCells.tmax.1050to1249[[i]])
  # tmp.tmin <- do.call(cbind, BiasCells.tmin.1050to1249[[i]])
  tmp.tmax <- do.call(cbind, BiasCells.tmax.1250to1449[[i]])
  tmp.tmin <- do.call(cbind, BiasCells.tmin.1250to1449[[i]])
  # rownames(tmp.tmax) <- c(850:1049)
  # rownames(tmp.tmin) <- c(850:1049)
  # rownames(tmp.tmax) <- c(1050:1249)
  # rownames(tmp.tmin) <- c(1050:1249)
  rownames(tmp.tmax) <- c(1250:1449)
  rownames(tmp.tmin) <- c(1250:1449)
  # GCM.tmax.0850to1049.to_interp[[i]] <- tmp.tmax
  # GCM.tmin.0850to1049.to_interp[[i]] <- tmp.tmin
  # GCM.tmax.1050to1249.to_interp[[i]] <- tmp.tmax
  # GCM.tmin.1050to1249.to_interp[[i]] <- tmp.tmin
  GCM.tmax.1250to1449.to_interp[[i]] <- tmp.tmax
  GCM.tmin.1250to1449.to_interp[[i]] <- tmp.tmin
  rm(tmp.tmax, tmp.tmin)
}
# save(GCM.tmax.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_0850to1049.Rdata")
# save(GCM.tmin.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_0850to1049.Rdata")
# save(GCM.tmax.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1050to1249.Rdata")
# save(GCM.tmin.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1050to1249.Rdata")
# save(GCM.tmax.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1250to1449.Rdata")
# save(GCM.tmin.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1250to1449.Rdata")

# save(GCM.tmax.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_0850to1049_em003.Rdata")
# save(GCM.tmin.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_0850to1049_em003.Rdata")
# save(GCM.tmax.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1050to1249_em003.Rdata")
# save(GCM.tmin.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1050to1249_em003.Rdata")
# save(GCM.tmax.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1250to1449_em003.Rdata")
# save(GCM.tmin.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1250to1449_em003.Rdata")

# save(GCM.tmax.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_0850to1049_em004.Rdata")
# save(GCM.tmin.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_0850to1049_em004.Rdata")
# save(GCM.tmax.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1050to1249_em004.Rdata")
# save(GCM.tmin.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1050to1249_em004.Rdata")
# save(GCM.tmax.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1250to1449_em004.Rdata")
# save(GCM.tmin.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1250to1449_em004.Rdata")

# save(GCM.tmax.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_0850to1049_em005.Rdata")
# save(GCM.tmin.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_0850to1049_em005.Rdata")
# save(GCM.tmax.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1050to1249_em005.Rdata")
# save(GCM.tmin.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1050to1249_em005.Rdata")
save(GCM.tmax.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_1250to1449_em005.Rdata")
save(GCM.tmin.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_1250to1449_em005.Rdata")
#Interpolate
xmin = -115
xmax = -107.5
ymin = 35.05263
ymax = 42.63158
xo = seq(xmin, xmax, length.out=45)
yo = seq(ymin, ymax, length.out=46)
d1 <- expand.grid(x = xo, y = yo)

x1 = seq(xmin, xmax, length.out=4)
y1 = seq(ymin, ymax, length.out=5)
d2 <- expand.grid(x = x1, y = y1)

load(file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata") #tmp.xyz
# tmp.xyz <- rasterToPoints(TREFHTMX.1850to2005.dailyRast[[1850]][[1]])
# save(tmp.xyz, file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmax_0850to1049.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_tmin_0850to1049.Rdata") #GCM.tmin.1850to2005.to_interp

library(akima)
# GCM.tmax.0850to1049.Interp <- list()
# GCM.tmax.1050to1249.Interp <- list()
GCM.tmax.1250to1449.Interp <- list()
for(j in 1:365){
  Tmp <- list()
  for(i in 1:200){
    # tmp.ak <- interp(d2[,1], d2[,2], GCM.tmax.0850to1049.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
   # tmp.ak <- interp(d2[,1], d2[,2], GCM.tmax.1050to1249.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
   tmp.ak <- interp(d2[,1], d2[,2], GCM.tmax.1250to1449.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak.ras <- raster(tmp.ak)
    Tmp[[i]] <- tmp.ak.ras
  }
  # GCM.tmax.0850to1049.Interp[[j]] <- Tmp
  # GCM.tmax.1050to1249.Interp[[j]] <- Tmp
  GCM.tmax.1250to1449.Interp[[j]] <- Tmp
  rm(Tmp, tmp.ak, tmp.ak.ras)
  print(paste("Patience. It's", sep=" ", Sys.time(), "and I'm processing day", j, "/ 365."))
}
#save(GCM.tmax.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049.Rdata")
# rm(GCM.tmax.0850to1049.to_interp, GCM.tmax.0850to1049.Interp)
# save(GCM.tmax.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249.Rdata")
#save(GCM.tmax.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449.Rdata")

#save(GCM.tmax.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049_em003.Rdata")
# rm(GCM.tmax.0850to1049.to_interp, GCM.tmax.0850to1049.Interp)
#save(GCM.tmax.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249_em003.Rdata")
# save(GCM.tmax.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449_em003.Rdata")

# save(GCM.tmax.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049_em004.Rdata")
# save(GCM.tmax.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249_em004.Rdata")
# save(GCM.tmax.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449_em004.Rdata")

# save(GCM.tmax.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049_em005.Rdata")
# save(GCM.tmax.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249_em005.Rdata")
save(GCM.tmax.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449_em005.Rdata")
# rm(GCM.tmax.1250to1449.to_interp)
gc()

# GCM.tmin.0850to1049.Interp <- list()
# GCM.tmin.1050to1249.Interp <- list()
GCM.tmin.1250to1449.Interp <- list()
for(j in 1:365){
  Tmp <- list()
  for(i in 1:200){
    # tmp.ak <- interp(d2[,1], d2[,2], GCM.tmin.0850to1049.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    # tmp.ak <- interp(d2[,1], d2[,2], GCM.tmin.1050to1249.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak <- interp(d2[,1], d2[,2], GCM.tmin.1250to1449.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak.ras <- raster(tmp.ak)
    Tmp[[i]] <- tmp.ak.ras
  }
  # GCM.tmin.0850to1049.Interp[[j]] <- Tmp
 # GCM.tmin.1050to1249.Interp[[j]] <- Tmp
  GCM.tmin.1250to1449.Interp[[j]] <- Tmp
  rm(Tmp, tmp.ak, tmp.ak.ras)
  print(paste("Patience. It's", sep=" ", Sys.time(), "and I'm processing day", j, "/ 365."))
}
# save(GCM.tmin.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049.Rdata")
# rm(GCM.tmin.0850to1049.to_interp, GCM.tmin.1850to2005.Interp)
# save(GCM.tmin.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249.Rdata")
# save(GCM.tmin.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449.Rdata")
# rm(GCM.tmin.1050to1249.to_interp)

#save(GCM.tmin.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049_em003.Rdata")
# rm(GCM.tmin.0850to1049.to_interp, GCM.tmin.1850to2005.Interp)
#save(GCM.tmin.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249_em003.Rdata")
# save(GCM.tmin.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449_em003.Rdata")

# save(GCM.tmin.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049_em004.Rdata")
# rm(GCM.tmin.0850to1049.to_interp, GCM.tmin.1850to2005.Interp)
# save(GCM.tmin.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249_em004.Rdata")
# save(GCM.tmin.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449_em004.Rdata")

# save(GCM.tmin.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049_em005.Rdata")
# save(GCM.tmin.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249_em005.Rdata")
save(GCM.tmin.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449_em005.Rdata")
gc()


load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tmaxStack.Rdata") #pr.max
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tminStack.Rdata") #pr.min
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tdifStack.Rdata") #pr.dif
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049.Rdata") #GCM.tmax.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049.Rdata") #GCM.tmin.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249.Rdata") #GCM.tmax.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249.Rdata") #GCM.tmin.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449.Rdata") #GCM.tmax.1250to1449.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449.Rdata") #GCM.tmin.1250to1449.Interp

#ensemble member 003
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049_em003.Rdata") #GCM.tmax.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049_em003.Rdata") #GCM.tmin.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249_em003.Rdata") #GCM.tmax.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249_em003.Rdata") #GCM.tmin.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449_em003.Rdata") #GCM.tmax.1250to1449.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449_em003.Rdata") #GCM.tmin.1250to1449.Interp

#ensemble member 004
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049_em004.Rdata") #GCM.tmax.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049_em004.Rdata") #GCM.tmin.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249_em004.Rdata") #GCM.tmax.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249_em004.Rdata") #GCM.tmin.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449_em004.Rdata") #GCM.tmax.1250to1449.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449_em004.Rdata") #GCM.tmin.1250to1449.Interp

#ensemble member 005
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_0850to1049_em005.Rdata") #GCM.tmax.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_0850to1049_em005.Rdata") #GCM.tmin.0850to1049.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1050to1249_em005.Rdata") #GCM.tmax.1050to1249.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1050to1249_em005.Rdata") #GCM.tmin.1050to1249.Interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMAX_1250to1449_em005.Rdata") #GCM.tmax.1250to1449.Interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_TMIN_1250to1449_em005.Rdata") #GCM.tmin.1250to1449.Interp

# GCM.tmax.0850to1049 <- list()
# GCM.tmin.0850to1049 <- list()
# GCM.tmax.1050to1249 <- list()
# GCM.tmin.1050to1249 <- list()
GCM.tmax.1250to1449 <- list()
GCM.tmin.1250to1449 <- list()
# l=0
l=100
for(i in 1:100){
  Tmp.max <- list()
  Tmp.min <- list()
  for(j in 1:365){
    # tmp.max <- resample(GCM.tmax.0850to1049.Interp[[j]][[i+l]], pr.max[[j]], method="bilinear")
    # tmp.min <- resample(GCM.tmin.0850to1049.Interp[[j]][[i+l]], pr.min[[j]], method="bilinear")
    # tmp.max <- resample(GCM.tmax.1050to1249.Interp[[j]][[i+l]], pr.max[[j]], method="bilinear")
    # tmp.min <- resample(GCM.tmin.1050to1249.Interp[[j]][[i+l]], pr.min[[j]], method="bilinear")
    tmp.max <- resample(GCM.tmax.1250to1449.Interp[[j]][[i+l]], pr.max[[j]], method="bilinear")
    tmp.min <- resample(GCM.tmin.1250to1449.Interp[[j]][[i+l]], pr.min[[j]], method="bilinear")
    tmp.av1 <- (tmp.max + tmp.min)/2
    tmp.av2 <- calc(stack(pr.min[[j]], pr.max[[j]]), fun=mean)
    tmp.agg <- aggregate(tmp.av2, c(44,44))
    tmp.ag1 <- resample(tmp.agg, tmp.av2)
    tmp.dif <- tmp.av1 - tmp.ag1
    Tmp.max[[j]] <- pr.max[[j]] + tmp.dif
    Tmp.min[[j]] <- pr.min[[j]] + tmp.dif
    # #Tmp.max[[j]] <- tmp.max + pr.max[[j]] - max(na.omit(values(tmp.max)))
    # Tmp.min[[j]] <- tmp.min + pr.min[[j]] - max(na.omit(values(tmp.min)))
    rm(tmp.max, tmp.min, tmp.av1, tmp.av2, tmp.agg, tmp.ag1, tmp.dif)
  }
  # GCM.tmax.0850to1049[[i+l]] <- Tmp.max
  # GCM.tmin.0850to1049[[i+l]] <- Tmp.min
  # GCM.tmax.1050to1249[[i+l]] <- Tmp.max
  # GCM.tmin.1050to1249[[i+l]] <- Tmp.min
  GCM.tmax.1250to1449[[i+l]] <- Tmp.max
  GCM.tmin.1250to1449[[i+l]] <- Tmp.min
  rm(Tmp.min)
  gc()
  print(paste("Patience. It's", sep=" ", Sys.time(), "and I'm processing", i, "/ 100."))
}
#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0950to1049.Rdata")
#save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0850to0949.Rdata")
#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0950to1049.Rdata")
#save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0950to1049.Rdata")
# # save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1050to1149.Rdata")
#save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1050to1149.Rdata")
# #save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1150to1249.Rdata")
# save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1150to1249.Rdata")
#save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1350to1449.Rdata")
#save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1350to1449.Rdata")

#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0850to0949_em003.Rdata")
#save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0850to0949_em003.Rdata")
#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0950to1049_em003.Rdata")
# save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0950to1049_em003.Rdata")
#save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0850to1049.Rdata")
#save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0850to0949.Rdata")
#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0950to1049.Rdata")
#save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1050to1149_em003.Rdata")
#save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1050to1149_em003.Rdata")
#save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1150to1249_em003.Rdata")
#save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1150to1249_em003.Rdata")
# save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1250to1349_em003.Rdata")
# save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1250to1349_em003.Rdata")
# save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1350to1449_em003.Rdata")
# save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1350to1449_em003.Rdata")

#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0850to0949_em004.Rdata")
#save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0850to0949_em004.Rdata")
#save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0950to1049_em004.Rdata")
# save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0950to1049_em004.Rdata")
# save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1050to1149_em004.Rdata")
#save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1050to1149_em004.Rdata")
# save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1150to1249_em004.Rdata")
# save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1150to1249_em004.Rdata")
# save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1250to1349_em004.Rdata")
# save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1250to1349_em004.Rdata")
# save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1350to1449_em004.Rdata")
# save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1350to1449_em004.Rdata")

# save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0850to0949_em005.Rdata")
# save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0850to0949_em005.Rdata")
# save(GCM.tmax.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax0950to1049_em005.Rdata")
# save(GCM.tmin.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin0950to1049_em005.Rdata")
# save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1050to1149_em005.Rdata")
# save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1050to1149_em005.Rdata")
# save(GCM.tmax.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1150to1249_em005.Rdata")
# save(GCM.tmin.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1150to1249_em005.Rdata")
# save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1250to1349_em005.Rdata")
# save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1250to1349_em005.Rdata")
save(GCM.tmax.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmax1350to1449_em005.Rdata")
save(GCM.tmin.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_tmin1350to1449_em005.Rdata")

gc()

##
#Precipitation
library(ncdf4)
library(raster)
library(qmap)
library(akima)

nc.dir <- dir("/media/zizroc/Extra Drive 1/Data/CPC_Unified_Gauge-based_analysis_station_data/", full.names=TRUE)

dailyPrecip <- list()
for(i in 1:58){
  nc <- nc_open(nc.dir[i])
  dname <- "precip"
  lon    <- ncvar_get(nc, "lon")
  nlon   <- dim(lon)
  lat    <- ncvar_get(nc, "lat")
  nlat   <- dim(lat)
  t      <- ncvar_get(nc, "time")
  tunits <- ncatt_get(nc, "time", "units")
  nt     <- dim(t)
  #ts.array  <- ncvar_get(nc, dname, start = c(1, 1, k*72999), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
  ts.array  <- ncvar_get(nc, dname) #this file is enormous (e.g., reset memory.size)
  dlname    <- ncatt_get(nc ,dname, "long_name")
  dunits    <- ncatt_get(nc, dname, "units")
  fillvalue <- ncatt_get(nc, dname, "_FillValue")
  nc_close(nc)
  rm(nc)
  gc()
  
  temp.r <- list()
  for(j in 1:nt){
    tmp <- raster(t(ts.array[48:108, 100:54, j]))
    extent(tmp) <- c(lon[48], lon[108], lat[54], lat[100])
    temp.r[[j]] <- tmp
  }
  # if(i==1){Tmp <- temp.r}
  # if(i >1){Tmp <- stack(Tmp, temp.r)}
  dailyPrecip[[i+1947]] <- stack(temp.r)
  rm(ts.array, temp.r)
  print(paste("Building dailyPrecip list. It's", sep=" ", Sys.time(), "and I'm processing year", i+1947, "/ 2005."))
}
save(dailyPrecip, file="/media/zizroc/Extra Drive 1/Data/R environments/dailyPrecip_1948to2016.Rdata")

load(file="/media/zizroc/Extra Drive 1/Data/R environments/dailyPrecip_1948to2016.Rdata")

##
#Daily total precipitation
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.PRECT.18500101-20051231.nc")
# dname <- "PRECT"
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.PRECT.18500101-20051231.nc")
# dname <- "PRECT"
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.PRECT.18500101-20051231.nc")
# dname <- "PRECT"
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.PRECT.18500101-20051231.nc")
dname <- "PRECT"

# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.PRECT.08500101-18491231.nc")
# dname <- "PRECT"
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.PRECT.08500101-18491231.nc")
# dname <- "PRECT"
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.PRECT.08500101-18491231.nc")
# dname <- "PRECT"
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.PRECT.08500101-18491231.nc")
dname <- "PRECT"

#Variables
lon    <- ncvar_get(tmp.nc, "lon")
nlon   <- dim(lon)
lat    <- ncvar_get(tmp.nc, "lat")
nlat   <- dim(lat)
dat    <- ncvar_get(tmp.nc, "date") #current date (YYYMMDD)
ndat   <- dim(dat)
t      <- ncvar_get(tmp.nc, "time")
tunits <- ncatt_get(tmp.nc, "time", "units")
nt     <- dim(t)

k = 1/72999 # for start at t=1
#k = 1 #for start at t=200
# ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, 73001), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
ts.array  <- ncvar_get(tmp.nc, dname) #this file is enormous (e.g., reset memory.size)
dlname    <- ncatt_get(tmp.nc ,dname, "long_name")
dunits    <- ncatt_get(tmp.nc, dname, "units")
fillvalue <- ncatt_get(tmp.nc, dname, "_FillValue")
nc_close(tmp.nc)
rm(tmp.nc)
gc()

prect.r <- list()
nt=73000 #850 to 1049 CE
# nt=56940 #1850 to 2005 CE
for(i in 1:nt){
  tmp <- raster(t(ts.array[99:102, 71:67, i]))
  extent(tmp) <- c(lon[99], lon[102], lat[67], lat[71])
  prect.r[[i]] <- tmp
}
rm(ts.array)
gc()
PRECT.1850to2005.dailyRast <- list()
# PRECT.0850to1049.dailyRast <- list()
# PRECT.1050to1249.dailyRast <- list()

n=156
# n=200
for(i in 1:n){
  index <- seq(i+364*(i-1), i+364*(i-1)+364, 1)
  PRECT.1850to2005.dailyRast[[i+1849]] <- prect.r[index]
  # PRECT.0850to1049.dailyRast[[i+849]] <- prect.r[index]
  # PRECT.1050to1249.dailyRast[[i+1049]] <- prect.r[index]
}
rm(prect.r)
#Saves a list of daily GCM values
# save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005.Rdata")
# # save(PRECT.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_0850to1049.Rdata")
# # save(PRECT.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1050to1249.Rdata")

# save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em003.Rdata")
# save(PRECT.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_0850to1049_em003.Rdata")
# save(PRECT.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1050to1249_em003.Rdata")

# save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em004.Rdata")
save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em005.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005.Rdata") #PRECT.1850to2005.dailyRast

load(file="/media/zizroc/Extra Drive 1/Data/R environments/dailyPrecip_1948to2016.Rdata") #dailyPrecip

cal.prec <- list()
for(i in 1:58){ #Calibration set is over 1948 to 2005 period
  for(j in 1:365){
    tmp1.prec  <- dailyPrecip[[i+1947]][[j]]
    tmp2.prec <- resample(tmp1.prec, PRECT.1850to2005.dailyRast[[i+1849+98]][[j]]*(8.64e7), method="bilinear")
    if(j==1){
      tmp3.prec <- tmp2.prec
    }
    if(j>1) {
      tmp3.prec <- stack(tmp3.prec, tmp2.prec)
    }
    rm(tmp1.prec, tmp2.prec)
  }
  cal.prec[[i+1947]] <- tmp3.prec
  rm(tmp3.prec)
  print(paste("Building precip. calibration data list. It's", sep=" ", Sys.time(), "and I'm processing year", i+1947, "/ 2005."))
}
# save(cal.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/calPrec.Rdata")
# save(cal.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/calPrec_em003.Rdata")
# save(cal.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/calPrec_em004.Rdata")
save(cal.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/calPrec_em005.Rdata")
rm(dailyPrecip)
gc()

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/calPrec.Rdata") #cal.prec

gcm.prec.bias <- list()
cal.prec.bias <- list()
for(i in 1:365){
  gcm.prec.m  <- matrix(nrow=58, ncol=20)
  cal.prec.m  <- matrix(nrow=58, ncol=20)
  for(j in 1:58){
    gcm.prec.tmp   <- PRECT.1850to2005.dailyRast[[j+1849+98]][[i]]*(8.64e7)
    cal.prec.tmp   <- cal.prec[[j+1947]][[i]]
    gcm.prec.m[j,] <- as.vector(gcm.prec.tmp)
    cal.prec.m[j,] <- as.vector(cal.prec.tmp)
  }
  gcm.prec.bias[[i]] <- gcm.prec.m
  cal.prec.bias[[i]] <- cal.prec.m
  rm(gcm.prec.tmp, cal.prec.tmp, gcm.prec.m, gcm.prec.m)
  print(paste("Building GCM bias data list. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
}
# save(gcm.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1948to2005.Rdata")
# save(gcm.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1948to2005_em003.Rdata")
# save(gcm.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1948to2005_em004.Rdata")
save(gcm.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1948to2005_em005.Rdata")

# save(cal.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_PRECIP_1948to2005.Rdata")
# save(cal.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_PRECIP_1948to2005_em003.Rdata")
# save(cal.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_PRECIP_1948to2005_em004.Rdata")
save(cal.prec.bias, file="/media/zizroc/Extra Drive 1/Data/R environments/CALIB_PRECIP_1948to2005_em005.Rdata")
gc()

#Tailor the length of the calibration time-series here
# gcm.prec.bias_calib <- list()
# cal.prec.bias_calib <- list()
# for(i in 1:12){
# tmp.prec <- gcm.prec.bias[[i]][-c(seq(1,9,1)),] #removes the first decade from calibration set
# tmp.cali <- cal.prec.bias[[i]][-c(seq(1,9,1)),]
# 
# gcm.prec.bias_calib[[i]] <- tmp.prec[-c(seq(91,110,1)),] #removes the last 2 decades from calibration set
# cal.prec.bias_calib[[i]] <- tmp.cali[-c(seq(91,110,1)),]
# }
#If tailoring of calibration time-series is unnecessary, use the following:
gcm.prec.bias_calib <- gcm.prec.bias
cal.prec.bias_calib <- cal.prec.bias

library(qmap)
BiasCells.prec <- list()
fitQUANT.prec  <- list()
for(i in 1:365){
  biasQUANT.prec <- list()
  biasCells.prec <- list()
  for(j in 1:20){
    mod.prec <- gcm.prec.bias_calib[[i]][,j] #correction to precipitation numbers applied previously
    obs.prec <- cal.prec.bias_calib[[i]][,j]
    tmp1 <- fitQmapQUANT(obs.prec, mod.prec, wet.day=TRUE, qstep=0.01)
    tmp2 <- doQmapQUANT(mod.prec, tmp1)
    biasQUANT.prec[[j]] <- tmp1
    biasCells.prec[[j]] <- tmp2
    rm(tmp1, tmp2, mod.prec, obs.prec)
  }
  fitQUANT.prec[[i]]  <- biasQUANT.prec #Assigned to a list to apply to daily GCM
  BiasCells.prec[[i]] <- biasCells.prec
  rm(biasCells.prec)
  print(paste("Need 365 and I'm at", i, sep=" "))
}
# save(fitQUANT.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec.Rdata")
# save(fitQUANT.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec_em003.Rdata")
# save(fitQUANT.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec_em004.Rdata")
save(fitQUANT.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec_em005.Rdata")

# save(BiasCells.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECIP_1948to2005.Rdata")
# save(BiasCells.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECIP_1948to2005_em003.Rdata")
# save(BiasCells.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECIP_1948to2005_em004.Rdata")
save(BiasCells.prec, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECIP_1948to2005_em005.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec.Rdata") #fitQUANT.prec for precipitation


#Bias-correct complete time-series
gcm.prec.bias.1850to2005 <- list()
for(i in 1:365){
  gcm.prec.m  <- matrix(nrow=156, ncol=20)
  for(j in 1:156){
    gcm.prec.tmp   <- PRECT.1850to2005.dailyRast[[j+1849]][[i]]*(8.64e7)
    gcm.prec.m[j,] <- as.vector(gcm.prec.tmp)
  }
  gcm.prec.bias.1850to2005[[i]] <- gcm.prec.m
  rm(gcm.prec.tmp)
  print(paste("Need 365 and I'm at", i, sep=" "))
}
# save(gcm.prec.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1850to2005.Rdata")
# save(gcm.prec.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1850to2005_em003.Rdata")
# save(gcm.prec.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1850to2005_em004.Rdata")
save(gcm.prec.bias.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1850to2005_em005.Rdata")
rm(gcm.prec.m)
gc()

BiasCells.prec.1850to2005 <- list()
for(i in 1:365){
  biasCells.prec <- list()
  for(j in 1:20){
    mod.prec <- gcm.prec.bias.1850to2005[[i]][,j] #correction applied previously
    tmp <- doQmapQUANT(mod.prec, fitQUANT.prec[[i]][[j]]) #qmap matrix computed for calibration set previously
    biasCells.prec[[j]] <- tmp
    rm(tmp, mod.prec)
  }
  BiasCells.prec.1850to2005[[i]] <- biasCells.prec
  print(paste("Need 365 and I'm at", i, sep=" "))
  rm(biasCells.prec)
}
# save(BiasCells.prec.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1850to2005.Rdata")
# save(BiasCells.prec.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1850to2005_em003.Rdata")
# save(BiasCells.prec.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1850to2005_em004.Rdata")
save(BiasCells.prec.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1850to2005_em005.Rdata")

#Organise data by day interpolate
GCM.prec.1850to2005.to_interp <- list()
for(i in 1:365){
  tmp.prec <- do.call(cbind, BiasCells.prec.1850to2005[[i]])
  rownames(tmp.prec) <- c(1850:2005)
  GCM.prec.1850to2005.to_interp[[i]] <- tmp.prec
  rm(tmp.prec)
}
# save(GCM.prec.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec.Rdata")
# save(GCM.prec.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em003.Rdata")
# save(GCM.prec.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em004.Rdata")
save(GCM.prec.1850to2005.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em005.Rdata")

#Interpolate
xmin = -115
xmax = -107.5
ymin = 35.05263
ymax = 42.63158
xo = seq(xmin, xmax, length.out=45)
yo = seq(ymin, ymax, length.out=46)
d1 <- expand.grid(x = xo, y = yo)
load(file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata") #tmp.xyz
# tmp.xyz <- rasterToPoints(TREFHTMX.1850to2005.dailyRast[[1850]][[1]])
# save(tmp.xyz, file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em003.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em004.Rdata") #GCM.tmax.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em005.Rdata") #GCM.tmax.1850to2005.to_interp

library(akima)
GCM.prec.1850to2005.Interp <- list()
for(j in 1:365){
  Tmp <- list()
  for(i in 1:156){
    tmp.ak <- interp(tmp.xyz[,1]-360, tmp.xyz[,2], GCM.prec.1850to2005.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak.ras <- raster(tmp.ak)
    Tmp[[i]] <- tmp.ak.ras
  }
  GCM.prec.1850to2005.Interp[[j]] <- Tmp
  rm(Tmp, tmp.ak, tmp.ak.ras)
  print(paste("Building spatially interpolated GCM list. It's", sep=" ", Sys.time(), "and I'm processing day", j, "/ 365."))
}
# save(GCM.prec.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT.Rdata")
# save(GCM.prec.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em003.Rdata")
# save(GCM.prec.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em004.Rdata")
save(GCM.prec.1850to2005.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em005.Rdata")
rm(GCM.prec.1850to2005.to_interp, GCM.prec.1850to2005.Interp)
gc()

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT.Rdata") #GCM.prec.1850to2005.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em003.Rdata") #GCM.prec.1850to2005.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em004.Rdata") #GCM.prec.1850to2005.Interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em005.Rdata") #GCM.prec.1850to2005.Interp


prism.ext <- raster()
extent(prism.ext) <- c(xmin, xmax, ymin, ymax)

#The following re-writes PRISM data without aggregating it to GCM-scale
prec.dir <- dir("/media/zizroc/My Passport/Data/PRISMdata/recent_daily/ppt", full.names=TRUE)
pris.ppt     <- list()
for(i in 1:34){ #from 1981 to 2014 CE
  #daily precipitation
  bilfiles  <- dir(prec.dir[i], pattern="bil.bil", ignore.case=FALSE, full.name=TRUE)
  auxfiles  <- dir(prec.dir[i], pattern="bil.aux", ignore.case=FALSE, full.name=TRUE)
  pris.tmp  <- bilfiles[!bilfiles %in% auxfiles] #only BIL files
  rm(bilfiles, auxfiles)

  for(j in 1:365){
    tmp1 <- raster(pris.tmp[j])
    tmp2 <- crop(tmp1, prism.ext)
    if(j==1){tmp3 <- tmp2}
    if(j>1) {tmp3 <- stack(tmp3, tmp2)}
    rm(tmp1, tmp2)
  }
  pris.ppt[[i+1980]] <- tmp3
  rm(tmp3)
  print(paste("Building PRISM data for SD. It's", sep=" ", Sys.time(), "and I'm processing year", i+1980, "/ 2014."))
}
rm(prec.dir)
save(pris.ppt, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMppt_IIASA.Rdata")
gc()

################################
## PRISM resampling sub-block ##
################################
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMppt_IIASA.Rdata") #pris.ppt
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005.Rdata") #PRECT.1850to2005.dailyRast
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em003.Rdata") #PRECT.1850to2005.dailyRast
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em004.Rdata") #PRECT.1850to2005.dailyRast
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em005.Rdata") #PRECT.1850to2005.dailyRast
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec.Rdata") #GCM.prec.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em003.Rdata") #GCM.prec.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em004.Rdata") #GCM.prec.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em005.Rdata") #GCM.prec.1850to2005.to_interp

#Create a library of GCM-scaled PRISM raster data

gcm.ext <- extent(PRECT.1850to2005.dailyRast[[1950]][[180]])
tmp.mod <- PRECT.1850to2005.dailyRast[[1950]][[180]]
mod.ext <- c(xmin(gcm.ext)-360, xmax(gcm.ext)-360, ymin(gcm.ext), ymax(gcm.ext))
extent(tmp.mod) <- mod.ext
values(tmp.mod) <- NA
pris.ppt.resampl <- list()
# pris.ppt.resampl.list <- list() #Single list of PRISM rasters to compare to GCM rasters
for(i in 1:34){
  tmp <- list()
  for(j in 1:365){
    tmp.obs <- pris.ppt[[i+1980]][[j]]
    tmp[[j]] <- resample(tmp.obs, tmp.mod, method="bilinear")
    # pris.ppt.resampl.list[[j+(i-1)*364]] <- tmp[[j]]
    rm(tmp.obs)
  }
  pris.ppt.resampl[[i+1980]] <- tmp
  rm(tmp)
  print(paste("Building resampled PRISM ppt list. It's", sep=" ", Sys.time(), "and I'm processing year", i+1980, "/ 2014."))
}
# save(pris.ppt.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005.Rdata")
# save(pris.ppt.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005_em003.Rdata")
# save(pris.ppt.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005_em004.Rdata")
save(pris.ppt.resampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005_em005.Rdata")

# save(pris.ppt.resampl.list, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPTlist_1981to2005.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005.Rdata") #pris.ppt.resampl

library(hydroGOF)

#The following loop selects for the PRISM ppt raster whose difference from the GCM raster is a minimum. This PRISM raster is then used as the most similar precipiation distribution to the GCM.
norm <- function(x) sqrt(sum(x^2))
GCM.PRISM.matches <- list()
for(i in 1:365){
  for(j in 1:156){
    Tmp <- data.frame()
    for(k in 1:34){
      tmp.obs <- as.vector(pris.ppt.resampl[[k+1980]][[i]])
      tmp.mod <- GCM.prec.1850to2005.to_interp[[i]][j,]
      tmp.cor <- cor(tmp.mod, tmp.obs)
      tmp.rms <- rmse(tmp.mod, tmp.obs)
      if(is.na(tmp.cor) == TRUE){tmp.cor <- 0}
      tmp.all <- cbind(tmp.cor, tmp.rms, k+1980, j+1849, i)
      if(k==1){Tmp <- tmp.all}
      if(k >1){Tmp <- rbind(Tmp, tmp.all)}
      rm(tmp.obs, tmp.mod, tmp.cor, tmp.rms, tmp.all)
    }
    Tmp <- as.data.frame(Tmp)
    names(Tmp) <- c("PEARSON_CORR", "RMSE", "PRISM.YEAR", "GCM.YEAR", "DAY")
    Tmp.max <- Tmp[which.max(Tmp[,1]),]
    if(Tmp.max[,1] == 0) {Tmp.max <- Tmp[which.min(Tmp[,2]),]} #If the correlation test does not apply, select the row with the lowest RMSE.
    
    if(j==1){TMP.max <- Tmp.max}
    if(j >1){TMP.max <- rbind(TMP.max, Tmp.max)}
    rm(Tmp.max)
  }
  GCM.PRISM.matches[[i]] <- TMP.max
  print(paste("Finding similar rasters to GCM. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
  rm(TMP.max)
}
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT_em003.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT_em004.Rdata")
save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT_em005.Rdata")

################################
## Downscaling sub-block      ##
################################
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT.Rdata") #GCM.PRISM.matches
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT_em003.Rdata") #GCM.PRISM.matches
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT_em004.Rdata") #GCM.PRISM.matches
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT_em005.Rdata") #GCM.PRISM.matches

tmp <- pris.ppt[[1999]][[180]]
values(tmp) <- 0
GCM.PRECT.1850to2005 <- list()
for(i in 1:365){
  matches <- GCM.PRISM.matches[[i]]
  obs <- list()
  for(j in 1:156){
    pris.yr  <- as.integer(matches[j,3])
    # mod.val  <- GCM.prec.1850to2005.to_interp[[i]][j,]
    # #mod.val  <- na.omit(values(GCM.prec.1850to2005.Interp[[i]][[j]]))
    # mod.max  <- max(mod.val)
    # obs.rsm  <- na.omit(values(pris.ppt.resampl[[pris.yr]][[i]]))
    # obs.max  <- max(obs.rsm)
    # tmp.fac  <- mod.max/obs.max
    obs[[j]] <- pris.ppt[[pris.yr]][[i]]
    # rm(pris.yr, mod.val, mod.max, obs.rsm, obs.max)
  }
  GCM.PRECT.1850to2005[[i]] <- obs
  print(paste("Patience! Computing BCSD'd GCM. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
}
# save(GCM.PRECT.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1850to2005.Rdata")
# save(GCM.PRECT.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1850to2005_em003.Rdata")
# save(GCM.PRECT.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1850to2005_em004.Rdata")
save(GCM.PRECT.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1850to2005_em005.Rdata")

################################
## Normalisation sub-block    ##
################################

# #This is the mean PRISM data for each of 365 days over normalisation period (1981-2005 CE)
# pris.ppt.nor_res.sampl <- list()
# pris.ppt.ave_res.sampl <- list()
# for(i in 1:365){
#   tmp.pris.nor_res.sampl <- list()
#   tmp.pris.ave_res.sampl <- list()
#   for(j in 1:25){
#     tmp.pris.nor_res.sampl[[j]] <- pris.ppt[[j]][[i]]
#     tmp.pris.ave_res.sampl[[j]] <- pris.ppt[[j]][[i]] - mean(na.omit(values(pris.ppt[[j]][[i]])))
#   }
#   pris.ppt.nor_res.sampl[[i]] <- tmp.pris.nor_res.sampl
#   pris.ppt.ave_res.sampl[[i]] <- tmp.pris.ave_res.sampl
#   rm(tmp.pris.nor_res.sampl, tmp.pris.ave_res.sampl)
#   print(i)
# }
# save(pris.ppt.nor_res.sampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISpptAveSampl.Rdata") #PRISM rasters unchanged but re-indexed
# save(pris.ppt.ave_res.sampl, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISpptAveSampl.Rdata") #residuals from mean of PRISM precipitation rasters
# gc()
# 
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISpptAveSampl.Rdata") #pris.ave_res.sampl
# 
# #Randomly pull days from each of the 25 years
# pris.nor_res.sampl.day <- list() #single raster for each of 25 years for each of 365 days
# pris.ave_res.sampl.day <- list()
# pris.nor_res.sampl.stk <- list() #raster stack containing all rasters for 25 years for each of 365 days
# pris.ave_res.sampl.stk <- list()
# for(i in 1:365){
#   j = sample(c(1:25), 1) #random sample from 1:25 years generated
#   pris.nor_res.sampl.day[[i]] <- pris.ppt.nor_res.sampl[[i]][[j]]
#   pris.ave_res.sampl.day[[i]] <- pris.ppt.ave_res.sampl[[i]][[j]]
# 
#   pris.nor_res.sampl.stk[[i]] <- stack(pris.ppt.nor_res.sampl[[i]])
#   pris.ave_res.sampl.stk[[i]] <- stack(pris.ppt.ave_res.sampl[[i]])
#   print(i)
# }
# 
# pr.norm <- list()
# pr.mean <- list()
# for(i in 1:365){
#   pr.norm[[i]] <- mean(pris.nor_res.sampl.stk[[i]]) #PRISM rasters unchanged
#   pr.mean[[i]] <- mean(pris.ave_res.sampl.stk[[i]]) #PRISM rasters minus mean for each day
# }
# 
# save(pr.norm, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_PPTnormStack.Rdata")
# save(pr.mean, file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_PPTmeanStack.Rdata")
# 
# rm(pr.max, pr.min, pris.max.sampl.stk, pris.min.sampl.stk, pris.ave.sampl.stk, pris.dif.sampl.stk, pris.max.sampl.day, pris.min.sampl.day, pris.ave.sampl.day, pris.dif.sampl.day, pris.max.sampl, pris.min.sampl, pris.ave.sampl, pris.dif.sampl)
# 
# #Dump excess memory
# library(raster)
# 
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_PPTnormStack.Rdata") #pr.norm
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_tminStack.Rdata")    #pr.mean
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT.Rdata") #GCM.prect.1850to2005.Interp
# 
# GCM.prect.1850to2005 <- list()
# for(i in 1:156){
#   Tmp.prect <- list()
#   for(j in 1:365){
#     tmp.prect <- resample(GCM.prec.1850to2005.Interp[[j]][[i]], pr.norm[[j]], method="bilinear")
#     Tmp.prect[[j]] <- tmp.prect + pr.norm[[j]]
#     rm(tmp.prect)
#   }
#   GCM.prect.1850to2005[[i]] <- Tmp.prect
#   rm(Tmp.prect)
#   print(paste("Patience. It's", sep=" ", Sys.time(), "and I'm processing year", i+1849, "/ 2005."))
# }
# save(GCM.prect.1850to2005, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1850to2005.Rdata")
library(ncdf4)
library(raster)
library(qmap)

# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.002.cam.h1.PRECT.08500101-18491231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.003.cam.h1.PRECT.08500101-18491231.nc")
# tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.004.cam.h1.PRECT.08500101-18491231.nc")
tmp.nc <- nc_open("/media/zizroc/Extra Drive 1/Data/CESM/daily/b.e11.BLMTRC5CN.f19_g16.005.cam.h1.PRECT.08500101-18491231.nc")
dname <- "PRECT"


#Variables
lon    <- ncvar_get(tmp.nc, "lon")
nlon   <- dim(lon)
lat    <- ncvar_get(tmp.nc, "lat")
nlat   <- dim(lat)
dat    <- ncvar_get(tmp.nc, "date") #current date (YYYMMDD)
ndat   <- dim(dat)
t      <- ncvar_get(tmp.nc, "time")
tunits <- ncatt_get(tmp.nc, "time", "units")
nt     <- dim(t)

# k = 1/72999 # for start at t=1
k = 2
ts.array  <- ncvar_get(tmp.nc, dname, start = c(1, 1, k*73000+1), count = c(dim(lon), dim(lat), 200*365)) #this file is enormous (e.g., reset memory.size)
#ts.array  <- ncvar_get(tmp.nc, dname) #this file is enormous (e.g., reset memory.size)
dlname    <- ncatt_get(tmp.nc ,dname, "long_name")
dunits    <- ncatt_get(tmp.nc, dname, "units")
fillvalue <- ncatt_get(tmp.nc, dname, "_FillValue")
nc_close(tmp.nc)
rm(tmp.nc)
gc()

prect.r <- list()
nt=73000 #850 to 1049 CE
# nt=56940 #1850 to 2005 CE
for(i in 1:nt){
  tmp <- raster(t(ts.array[99:102, 71:67, i]))
  extent(tmp) <- c(lon[99], lon[102], lat[67], lat[71])
  prect.r[[i]] <- tmp
}
rm(ts.array)
gc()
# PRECT.1850to2005.dailyRast <- list()
# PRECT.0850to1049.dailyRast <- list()
# PRECT.1050to1249.dailyRast <- list()
PRECT.1250to1449.dailyRast <- list()

# n=156
n=200
for(i in 1:n){
  index <- seq(i+364*(i-1), i+364*(i-1)+364, 1)
  # PRECT.0850to1049.dailyRast[[i+849]] <- prect.r[index]
  # PRECT.1050to1249.dailyRast[[i+1049]] <- prect.r[index]
  PRECT.1250to1449.dailyRast[[i+1249]] <- prect.r[index]
}
rm(prect.r)
#Saves a list of daily GCM values
# # save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005.Rdata")
# save(PRECT.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_0850to1049.Rdata")
# # save(PRECT.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1050to1249.Rdata")
# save(PRECT.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1250to1449.Rdata")

# save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em003.Rdata")
# save(PRECT.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_0850to1049_em003.Rdata")
# save(PRECT.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1050to1249_em003.Rdata")
# save(PRECT.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1250to1449_em003.Rdata")

# save(PRECT.1850to2005.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1850to2005_em004.Rdata")
# save(PRECT.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_0850to1049_em004.Rdata")
# save(PRECT.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1050to1249_em004.Rdata")
# save(PRECT.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1250to1449_em004.Rdata")

# save(PRECT.0850to1049.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_0850to1049_em005.Rdata")
# save(PRECT.1050to1249.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1050to1249_em005.Rdata")
save(PRECT.1250to1449.dailyRast, file="/media/zizroc/Extra Drive 1/Data/R environments/PRECT_1250to1449_em005.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec.Rdata") #fitQUANT.prec for precipitation
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec_em003.Rdata") #fitQUANT.prec for precipitation
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec_em004.Rdata") #fitQUANT.prec for precipitation
load(file="/media/zizroc/Extra Drive 1/Data/R environments/fitQUANT_prec_em005.Rdata") #fitQUANT.prec for precipitation

#Bias-correct complete time-series
# gcm.prec.bias.0850to1049 <- list()
# gcm.prec.bias.1050to1249 <- list()
gcm.prec.bias.1250to1449 <- list()

for(i in 1:365){
  gcm.prec.m  <- matrix(nrow=200, ncol=20)
  for(j in 1:200){
   # gcm.prec.tmp   <- PRECT.0850to1049.dailyRast[[j+849]][[i]]*(8.64e7)
    # gcm.prec.tmp   <- PRECT.1050to1249.dailyRast[[j+1049]][[i]]*(8.64e7)
    gcm.prec.tmp   <- PRECT.1250to1449.dailyRast[[j+1249]][[i]]*(8.64e7)
    gcm.prec.m[j,] <- as.vector(gcm.prec.tmp)
  }
  # gcm.prec.bias.0850to1049[[i]] <- gcm.prec.m
  # gcm.prec.bias.1050to1249[[i]] <- gcm.prec.m
  gcm.prec.bias.1250to1449[[i]] <- gcm.prec.m
  rm(gcm.prec.tmp)
  print(paste("Need 365 and I'm at", i, sep=" "))
}
# # save(gcm.prec.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_0850to1049.Rdata")
# save(gcm.prec.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1050to1249.Rdata")
# save(gcm.prec.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1250to1449.Rdata")

# save(gcm.prec.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_0850to1049_em003.Rdata")
# save(gcm.prec.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1050to1249_em003.Rdata")
# save(gcm.prec.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1250to1449_em003.Rdata")

# save(gcm.prec.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_0850to1049_em004.Rdata")
# save(gcm.prec.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1050to1249_em004.Rdata")
# save(gcm.prec.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1250to1449_em004.Rdata")

# save(gcm.prec.bias.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_0850to1049_em005.Rdata")
# save(gcm.prec.bias.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1050to1249_em005.Rdata")
save(gcm.prec.bias.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_PRECT_1250to1449_em005.Rdata")
rm(gcm.prec.m)
gc()

# BiasCells.prec.0850to1049 <- list()
# BiasCells.prec.1050to1249 <- list()
BiasCells.prec.1250to1449 <- list()
for(i in 1:365){
  biasCells.prec <- list()
  for(j in 1:20){
    # mod.prec <- gcm.prec.bias.0850to1049[[i]][,j] #correction applied previously
    # mod.prec <- gcm.prec.bias.1050to1249[[i]][,j] #correction applied previously
    mod.prec <- gcm.prec.bias.1250to1449[[i]][,j] #correction applied previously
    tmp <- doQmapQUANT(mod.prec, fitQUANT.prec[[i]][[j]]) #qmap matrix computed for calibration set previously
    biasCells.prec[[j]] <- tmp
    rm(tmp, mod.prec)
  }
  # BiasCells.prec.0850to1049[[i]] <- biasCells.prec
  # BiasCells.prec.1050to1249[[i]] <- biasCells.prec
  BiasCells.prec.1250to1449[[i]] <- biasCells.prec
  print(paste("Need 365 and I'm at", i, sep=" "))
  rm(biasCells.prec)
}
# #save(BiasCells.prec.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_0850to1049.Rdata")
# #save(BiasCells.prec.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1050to1249.Rdata")
# save(BiasCells.prec.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1250to1449.Rdata")

# save(BiasCells.prec.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_0850to1049_em003.Rdata")
# save(BiasCells.prec.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1050to1249_em003.Rdata")
# save(BiasCells.prec.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1250to1449_em003.Rdata")

# save(BiasCells.prec.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_0850to1049_em004.Rdata")
# save(BiasCells.prec.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1050to1249_em004.Rdata")
# save(BiasCells.prec.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1250to1449_em004.Rdata")

# save(BiasCells.prec.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_0850to1049_em005.Rdata")
# save(BiasCells.prec.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1050to1249_em005.Rdata")
save(BiasCells.prec.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMbias_PRECT_1250to1449_em005.Rdata")

#Organise data by day interpolate
# GCM.prec.0850to1049.to_interp <- list()
# GCM.prec.1050to1249.to_interp <- list()
GCM.prec.1250to1449.to_interp <- list()
for(i in 1:365){
  # tmp.prec <- do.call(cbind, BiasCells.prec.0850to1049[[i]])
  # rownames(tmp.prec) <- c(0850:1049)
  # GCM.prec.0850to1049.to_interp[[i]] <- tmp.prec
  # tmp.prec <- do.call(cbind, BiasCells.prec.1050to1249[[i]])
  # rownames(tmp.prec) <- c(1050:1249)
  # GCM.prec.1050to1249.to_interp[[i]] <- tmp.prec
  tmp.prec <- do.call(cbind, BiasCells.prec.1250to1449[[i]])
  rownames(tmp.prec) <- c(1250:1449)
  GCM.prec.1250to1449.to_interp[[i]] <- tmp.prec
  rm(tmp.prec)
}
# save(GCM.prec.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec.Rdata")
# save(GCM.prec.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec.Rdata")
# save(GCM.prec.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec.Rdata")

# save(GCM.prec.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em003.Rdata")
# save(GCM.prec.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em003.Rdata")
# save(GCM.prec.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em003.Rdata")

# save(GCM.prec.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em004.Rdata")
# save(GCM.prec.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em004.Rdata")
# save(GCM.prec.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em004.Rdata")

# save(GCM.prec.0850to1049.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em005.Rdata")
# save(GCM.prec.1050to1249.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em005.Rdata")
save(GCM.prec.1250to1449.to_interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em005.Rdata")

#Interpolate
xmin = -115
xmax = -107.5
ymin = 35.05263
ymax = 42.63158
xo = seq(xmin, xmax, length.out=45)
yo = seq(ymin, ymax, length.out=46)
d1 <- expand.grid(x = xo, y = yo)
load(file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata") #tmp.xyz
# tmp.xyz <- rasterToPoints(TREFHTMX.1850to2005.dailyRast[[1850]][[1]])
# save(tmp.xyz, file="/media/zizroc/Extra Drive 1/Data/R environments/tmpXYZ.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em003.Rdata") #GCM.tmax.1850to2005.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em004.Rdata") #GCM.tmax.1850to2005.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_prec_em005.Rdata") #GCM.tmax.1850to2005.to_interp

# # load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec.Rdata") #GCM.prec.1050to1249.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec.Rdata") #GCM.prec.1050to1249.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec.Rdata") #GCM.prec.1050to1249.to_interp

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em003.Rdata") #GCM.prec.0850to1049.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em003.Rdata") #GCM.prec.1050to1249.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em003.Rdata") #GCM.prec.1250to1449.to_interp

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em004.Rdata") #GCM.prec.0850to1049.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em004.Rdata") #GCM.prec.1050to1249.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em004.Rdata") #GCM.prec.1250to1449.to_interp

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em005.Rdata") #GCM.prec.0850to1049.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em005.Rdata") #GCM.prec.1050to1249.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em005.Rdata") #GCM.prec.1250to1449.to_interp

library(akima)
# GCM.prec.0850to1049.Interp <- list()
# GCM.prec.1050to1249.Interp <- list()
GCM.prec.1250to1449.Interp <- list()
for(j in 1:365){
  Tmp <- list()
  for(i in 1:200){
    # tmp.ak <- interp(tmp.xyz[,1]-360, tmp.xyz[,2], GCM.prec.0850to1049.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    # tmp.ak <- interp(tmp.xyz[,1]-360, tmp.xyz[,2], GCM.prec.1050to1249.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak <- interp(tmp.xyz[,1]-360, tmp.xyz[,2], GCM.prec.1250to1449.to_interp[[j]][i,], xo=seq(min(d1[,1]), max(d1[,1]), length=45), yo=seq(min(d1[,2]), max(d1[,2]), length=46), linear=TRUE)
    tmp.ak.ras <- raster(tmp.ak)
    Tmp[[i]] <- tmp.ak.ras
  }
  # GCM.prec.0850to1049.Interp[[j]] <- Tmp
  # GCM.prec.1050to1249.Interp[[j]] <- Tmp
  GCM.prec.1250to1449.Interp[[j]] <- Tmp
  rm(Tmp, tmp.ak, tmp.ak.ras)
  print(paste("Building spatially interpolated GCM list. It's", sep=" ", Sys.time(), "and I'm processing day", j, "/ 365."))
}
# # save(GCM.prec.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1050to1249_PRECT.Rdata")
# save(GCM.prec.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1250to1449_PRECT.Rdata")

# save(GCM.prec.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_0850to1049_PRECT_em003.Rdata")
# save(GCM.prec.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1050to1249_PRECT_em003.Rdata")
# save(GCM.prec.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1250to1449_PRECT_em003.Rdata")

# save(GCM.prec.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_0850to1049_PRECT_em004.Rdata")
# save(GCM.prec.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1050to1249_PRECT_em004.Rdata")
# save(GCM.prec.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1250to1449_PRECT_em004.Rdata")

# save(GCM.prec.0850to1049.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_0850to1049_PRECT_em005.Rdata")
# save(GCM.prec.1050to1249.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1050to1249_PRECT_em005.Rdata")
save(GCM.prec.1250to1449.Interp, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_1250to1449_PRECT_em005.Rdata")
# rm(GCM.prec.1250to1449.to_interp, GCM.prec.1250to1449.Interp)
gc()

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT.Rdata") #GCM.prec.1850to2005.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em003.Rdata") #GCM.prec.1850to2005.Interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcorrByDays_PRECT_em004.Rdata") #GCM.prec.1850to2005.Interp

################################
## PRISM resampling sub-block ##
################################
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMppt_IIASA.Rdata") #pris.ppt
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec.Rdata") #GCM.prec.1250to1449.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005.Rdata") #pris.ppt.resampl

load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISMppt_IIASA.Rdata") #pris.ppt

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em003.Rdata") #GCM.prec.0850to1049.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em003.Rdata") #GCM.prec.1050to1249.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em003.Rdata") #GCM.prec.1250to1449.to_interp

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em004.Rdata") #GCM.prec.0850to1049.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em004.Rdata") #GCM.prec.1050to1249.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em004.Rdata") #GCM.prec.1250to1449.to_interp

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_0850to1049_prec_em005.Rdata") #GCM.prec.0850to1049.to_interp
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1050to1249_prec_em005.Rdata") #GCM.prec.1050to1249.to_interp
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMcalDay_1250to1449_prec_em005.Rdata") #GCM.prec.1250to1449.to_interp

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005_em003.Rdata") #pris.ppt.resampl
#load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005_em004.Rdata") #pris.ppt.resampl
load(file="/media/zizroc/Extra Drive 1/Data/R environments/PRISM_resampledPPT_1981to2005_em005.Rdata") #pris.ppt.resampl

library(hydroGOF)

#The following loop selects for the PRISM ppt raster whose difference from the GCM raster is a minimum. This PRISM raster is then used as the most similar precipiation distribution to the GCM.
norm <- function(x) sqrt(sum(x^2))
GCM.PRISM.matches <- list()
for(i in 1:365){
  for(j in 1:200){
    Tmp <- data.frame()
    for(k in 1:34){
      tmp.obs <- as.vector(pris.ppt.resampl[[k+1980]][[i]])
      # tmp.mod <- GCM.prec.0850to1049.to_interp[[i]][j,]
      # tmp.mod <- GCM.prec.1050to1249.to_interp[[i]][j,]
      tmp.mod <- GCM.prec.1250to1449.to_interp[[i]][j,]
      tmp.cor <- cor(tmp.mod, tmp.obs)
      tmp.rms <- rmse(tmp.mod, tmp.obs)
      if(is.na(tmp.cor) == TRUE){tmp.cor <- 0}
      tmp.all <- cbind(tmp.cor, tmp.rms, k+1980, j+1249, i)
      if(k==1){Tmp <- tmp.all}
      if(k >1){Tmp <- rbind(Tmp, tmp.all)}
      rm(tmp.obs, tmp.mod, tmp.cor, tmp.rms, tmp.all)
    }
    Tmp <- as.data.frame(Tmp)
    names(Tmp) <- c("PEARSON_CORR", "RMSE", "PRISM.YEAR", "GCM.YEAR", "DAY")
    Tmp.max <- Tmp[which.max(Tmp[,1]),]
    if(Tmp.max[,1] == 0) {Tmp.max <- Tmp[which.min(Tmp[,2]),]} #If the correlation test does not apply, select the row with the lowest RMSE.
    
    if(j==1){TMP.max <- Tmp.max}
    if(j >1){TMP.max <- rbind(TMP.max, Tmp.max)}
    rm(Tmp.max)
  }
  GCM.PRISM.matches[[i]] <- TMP.max
  print(paste("Finding similar rasters to GCM. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
  rm(TMP.max)
}
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT.Rdata")

# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_0850to1049_PPT_em003.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1050to1249_PPT_em003.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT_em003.Rdata")

# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_0850to1049_PPT_em004.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1050to1249_PPT_em004.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT_em004.Rdata")

# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_0850to1049_PPT_em005.Rdata")
# save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1050to1249_PPT_em005.Rdata")
save(GCM.PRISM.matches, file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT_em005.Rdata")
################################
## Downscaling sub-block      ##
################################
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatchesPPT.Rdata") #GCM.PRISM.matches

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_0850to1049_PPT_em003.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1050to1249_PPT_em003.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT_em003.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_0850to1049_PPT_em004.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1050to1249_PPT_em004.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT_em004.Rdata")

# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_0850to1049_PPT_em005.Rdata")
# load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1050to1249_PPT_em005.Rdata")
load(file="/media/zizroc/Extra Drive 1/Data/R environments/GCMandPRISMmatches_1250to1449_PPT_em005.Rdata")

tmp <- pris.ppt[[1999]][[180]]
values(tmp) <- 0
# GCM.PRECT.0850to1049 <- list()
# GCM.PRECT.1050to1249 <- list()
GCM.PRECT.1250to1449 <- list()
for(i in 1:365){
  matches <- GCM.PRISM.matches[[i]]
  obs <- list()
  for(j in 1:200){
    pris.yr  <- as.integer(matches[j,3])
    # mod.val  <- GCM.prec.1850to2005.to_interp[[i]][j,]
    # #mod.val  <- na.omit(values(GCM.prec.1850to2005.Interp[[i]][[j]]))
    # mod.max  <- max(mod.val)
    # obs.rsm  <- na.omit(values(pris.ppt.resampl[[pris.yr]][[i]]))
    # obs.max  <- max(obs.rsm)
    # tmp.fac  <- mod.max/obs.max
    obs[[j]] <- pris.ppt[[pris.yr]][[i]]
    # rm(pris.yr, mod.val, mod.max, obs.rsm, obs.max)
  }
  # GCM.PRECT.0850to1049[[i]] <- obs
  # GCM.PRECT.1050to1249[[i]] <- obs
  GCM.PRECT.1250to1449[[i]] <- obs
  print(paste("Patience! Computing BCSD'd GCM. It's", sep=" ", Sys.time(), "and I'm processing day", i, "/ 365."))
}
# save(GCM.PRECT.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1050to1249.Rdata")
# save(GCM.PRECT.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1250to1449.Rdata")

# save(GCM.PRECT.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect0850to1049_em003.Rdata")
# save(GCM.PRECT.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1050to1249_em003.Rdata")
# save(GCM.PRECT.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1250to1449_em003.Rdata")

# save(GCM.PRECT.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect0850to1049_em004.Rdata")
#save(GCM.PRECT.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1050to1249_em004.Rdata")
# save(GCM.PRECT.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1250to1449_em004.Rdata")

# save(GCM.PRECT.0850to1049, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect0850to1049_em005.Rdata")
# save(GCM.PRECT.1050to1249, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1050to1249_em005.Rdata")
save(GCM.PRECT.1250to1449, file="/media/zizroc/Extra Drive 1/Data/R environments/GCM_prect1250to1449_em005.Rdata")
