## Marcus J Thomson
## UCLA Geography - 2016
## C14 age batch calibration using Bchron

library(Bchron)
library(dplyr)
library(data.table)

df  <- read.table("...", sep=",", header=T) #Replace ... with the path to your uncalibrated dates
#df  <- na.omit(df)
dates = BchronCalibrate(ages=df$C14_AGE, ageSds=df$C14_ERROR, calCurves=rep("intcal13", length(df[,1]))) #calibration curve intcal13

#Bchron outputs a list() of several vector objects. I use the following loop to rearrange these according to location and convert to YEAR CE. You might want to change YEAR_CE=(1950-dates[[i]]$ageGrid) to YEAR_BP = dates[[i]]$ageGrid, and just omit the LON and LAT columns.
dates.dat <- list()
for(i in 1:length(df[,1])){
  tmp.df <- cbind(LON=rep(df[i,9], length(df[[i]]$ageGrid)), LAT=rep(df[i,10], length(dates[[i]]$ageGrid)), YEAR_CE=(1950-dates[[i]]$ageGrid), DENSITY=dates[[i]]$densities)
  if(i==1) Tmp <- tmp.df
  if(i >1) Tmp <- rbind(Tmp, tmp.df)
  rm(tmp.df)
  print(i)
}
dates.DF <- as.data.frame(Tmp)

#dates.DT <- as.data.table(Tmp) #This is not important to the calibration. However, I left it in here because you may want to start using data tables for radiocarbon dates. These make some things easier.
rm(Tmp)

#dplyr does the rearrangement below. If you don't care about the locations, you can just delete the LON, LAT arguments. The code takes all of the ages which are at the same locations and years and sums them to get a summed distribution.
Dates.DF <- as.data.frame(dates.DF %>%
                            group_by(LON, LAT, YEAR_CE) %>%
                            summarize(sum(DENSITY)))