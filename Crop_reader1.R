#Crop reader script.
library(scales)
library(tidyverse)
library(readr)
library(zoo)
library(data.table)

# sites <- read.csv("I:\\balkovic\\Thomson\\site_data.csv")
# sites <- na.omit(sites[,-c(1)])
range_crk <- data.frame(x=c(-110.057094, -110.215635, -110.271670), y=c(39.305831, 39.434589, 39.528561))
names(range_crk) <- c("LON", "LAT")
range_crk <- data.frame(INDEX=c(149, 150, 151), range_crk)
load(file="/media/zizroc/Extra Drive 1/Data/R environments/iiasa_xy_sites.Rdata") #xy.ind. Definitive xy index for Juraj.
subtract <- c(10, 11, 14, 15, 22, 23, 25, 27, 30, 31, 41, 42, 43, 48, 55, 60, 61, 79, 99, 102, 104, 110, 111, 113, 114, 116, 118, 124, 126, 127, 128, 129, 130, 132, 137, 139, 140, 142, 143, 144, 145, 146, 147, 148) #These distributions are not significant after 850 CE.
xy.ind <- rbind(xy.ind, data.frame(INDEX=c(144, 145, 146, 147, 148), LON=c(rep(NA, 5)), LAT=c(rep(NA, 5))), range_crk)
xy <- xy.ind[-subtract,]
sites <- xy

#Read in site occupation distributions
# load("C:\\Users\\thomson\\Documents\\IIASA\\Dissertation\\cGDD\\Fremont_sites_1sigma_ages.Rdata") #time.1sig.occ.ord
# load("C:\\Users\\thomson\\Documents\\IIASA\\Dissertation\\cGDD\\Fremont_sites_2sigma_ages.Rdata") #time.2sig.occ.ord
load("/media/zizroc/Extra Drive 1/Data/R environments/Fremont_sites_1sigma_ages.Rdata") #time.1sig.occ.ord
load("/media/zizroc/Extra Drive 1/Data/R environments/Fremont_sites_2sigma_ages.Rdata") #time.2sig.occ.ord

occ.1sig <- time.1sig.occ.ord
occ.2sig <- time.2sig.occ.ord
rm(time.1sig.occ.ord, time.2sig.occ.ord)

site.spd.1sig <- list()
site.spd.2sig <- list()
names <- as.numeric(rownames(sites))
for(i in names){
	tmp1 <- occ.1sig[occ.1sig$INDEX==i, 2:5]
	tmp2 <- occ.2sig[occ.2sig$INDEX==i, 2:5]
	site.spd.1sig[[i]] <- tmp1
	site.spd.2sig[[i]] <- tmp2
	rm(tmp1, tmp2)
}

#Adds information for Range Creek sites
range_crk <- data.frame(x=c(-110.057094, -110.215635, -110.271670), y=c(39.305831, 39.434589, 39.528561))
range_crk <- cbind(c(1063, 1064, 1065), range_crk)
calAD_ave <- c(1821.365459,1124.529463,1100.30692,1100.429045,1097.504471,1100.666867,1097.748721,1094.827361,1097.992971,1098.115096,1098.234007,1095.312647,1095.434772,1095.560111,1095.682236,1095.801147,1080.705846,1080.824757,1050.515244,1041.506913,1032.495369,1020.446767,1002.304766,999.3834061,972.114164,929.6274956,923.6594363,890.3032237,388.2534967)
calAD_min <- c(1675.278167,1042.355361,1021.173089,1033.469155,1021.417339,1024.579736,1036.879016,1030.91417,1021.90584,1022.027965,1025.190361,1019.225516,1025.434611,1022.513251,1025.678862,1022.757501,995.4882592,995.6071705,992.6890241,1014.115546,983.7996047,983.9217298,904.9132384,905.0353635,905.1542747,835.2794529,783.6623288,783.7844538,263.4706016)
calAD_max <- c(1937.017899,1206.703564,1206.825689,1161.29875,1210.113425,1188.927939,1158.618426,1158.737337,1146.688735,1158.984801,1159.103712,1180.530234,1177.608874,1180.774484,1150.461757,1147.537183,1162.876734,1163.002073,1160.077499,1160.202838,1160.321749,1041.747949,1029.696134,1023.731288,1017.766443,981.363531,993.6628109,969.4370539,525.2103329)
range_crk <- cbind(calAD_max, calAD_min, cbind(c(rep(-110.215635, 29)), c(rep(39.434589,29))))

rc1 <- data.frame(INDEX=seq(1, 29, 1), LON=rep(-110.057094, 29), LAT=rep(39.305831, 29), WEIGHT=rep(68.27, 29), MIN_YEAR_CE=range_crk[,2], MAX_YEAR_CE=range_crk[,1], REFERNCE=rep("Metcalfe_PC", 29), EVIDENCE_OF_FARMING=rep("direct", 29))

rc2 <- data.frame(INDEX=seq(30, 58, 1), LON=rep(-110.215635, 29), LAT=rep(39.434589, 29), WEIGHT=rep(68.27, 29), MIN_YEAR_CE=range_crk[,2], MAX_YEAR_CE=range_crk[,1], REFERNCE=rep("Metcalfe_PC", 29), EVIDENCE_OF_FARMING=rep("direct", 29))

rc3 <- data.frame(INDEX=seq(59, 87, 1), LON=rep(-110.271670, 29), LAT=rep(39.528561, 29), WEIGHT=rep(68.27, 29), MIN_YEAR_CE=range_crk[,2], MAX_YEAR_CE=range_crk[,1], REFERNCE=rep("Metcalfe_PC", 29), EVIDENCE_OF_FARMING=rep("direct", 29))

range_creek.df <- rbind(rc1, rc2, rc3)

x <- seq(1,2000,length=2000)
y <- list()
for(i in 1:length(rc1[,1])){
  tmp <- dnorm(x, mean = (rc1[i,5]+rc1[i,6])/2, sd = (rc1[i,6]-rc1[i,5])/2)
  y[[i]] <- tmp
  if(i==1){Tmp <- tmp}
  if(i >1){Tmp <- cbind(Tmp, tmp)}
}
YNorm <- rowSums(Tmp)

for(i in 149:151){
  tmp1 <- data.frame(YEAR_CE=c(1:2000), SPD=YNorm)
  tmp2 <- data.frame(YEAR_CE=c(1:2000), SPD=YNorm)
  lon  <- range_crk[i-148, 3]
  lat  <- range_crk[i-148, 4]
  tmp3 <- data.frame(LON=rep(lon, length(YNorm)), LAT=rep(lat, length(YNorm)))
  tmp4 <- data.frame(tmp3, tmp1)
  tmp5 <- data.frame(tmp3, tmp2)
  site.spd.1sig[[i]] <- tmp4
  site.spd.2sig[[i]] <- tmp5
  rm(tmp1, tmp2)
}

# rc.spd <- rbind(data.frame(INDEX=rep(149, 2000), site.spd.1sig[[151]]), data.frame(INDEX=rep(150, 2000), site.spd.1sig[[151]]), data.frame(INDEX=rep(151, 2000), site.spd.1sig[[151]]))

# pdf("C:\\Users\\thomson\\Documents\\IIASA\\Dissertation\\EPIC outputs\\Fremont_rainfed.pdf")

save(site.spd.1sig, file="/media/zizroc/Extra Drive 1/Data/SiteSPD1sigma.Rdata")
save(site.spd.2sig, file="/media/zizroc/Extra Drive 1/Data/SiteSPD2sigma.Rdata")

nsites <- sites$INDEX
path.em <- dir("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002", full.names=T)
# path.em <- dir("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em002", full.names=T)

for(j in 0:9){
  
  test_ir0 <- read.table(path.em[j+1], sep=",", header=T)
  
  #ensemble member 002
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER0_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER1_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER2_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER3_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER4_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER5_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER6_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER7_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER8_IR.txt", sep=",", header=T)
  # test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em002/EPIC_ITER9_IR.txt", sep=",", header=T)

#ensemble member 003
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER0_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER1_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER2_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER3_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER4_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER5_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER6_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER7_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER8_em003_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em003/EPIC_ITER9_em003_IR.txt", sep=",", header=T)

# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER0_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER1_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER2_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER3_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER4_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER5_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER6_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER7_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER8_em003_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em003/EPIC_ITER9_em003_RF.txt", sep=",", header=T)

#ensemble member 004
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER0_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER1_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER2_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER3_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER4_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER5_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER6_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER7_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER8_em004_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em004/EPIC_ITER9_em004_IR.txt", sep=",", header=T)

# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER0_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER1_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER2_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER3_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER4_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER5_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER6_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER7_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER8_em004_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em004/EPIC_ITER9_em004_RF.txt", sep=",", header=T)

#ensemble member 005
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER0_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER1_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER2_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER3_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER4_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER5_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER6_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER7_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER8_em005_IR.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Irrigated/em005/EPIC_ITER9_em005_IR.txt", sep=",", header=T)

# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER0_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER1_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER2_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER3_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER4_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER5_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER6_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER7_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER8_em005_RF.txt", sep=",", header=T)
# test_ir0 <- read.table("/media/zizroc/Extra Drive 1/Data/EPIC/Rainfed/em005/EPIC_ITER9_em005_RF.txt", sep=",", header=T)

##
#EPIC crop model block. This reads in the data from the crop model.
##
  Corn.DF <- data.frame()
  Bean.DF <- data.frame()
  Rest.DF <- data.frame()
  for(nsite in nsites){

    #nsite=nsites[12]
    # f <- function(x, pos) subset(x, Site_ID == nsite)
    # test_ir <- read_delim_chunked("I:\\balkovic\\Thomson\\OUT_24082017\\EPIC_SoilRank1_RF.txt", delim=",", DataFrameCallback$new(f), chunk_size = 1e5)

    test_ir <- test_ir0[test_ir0$Site_ID==nsite,]

    #Isolate by crop type
    test_ir.corn <- test_ir[test_ir$CROP=="COR2",] #Indian corn hard coded into EPIC.
    test_ir.bean <- test_ir[test_ir$CROP=="DRYB",] #dry beans from EPIC.
    test_ir.rest <- test_ir[test_ir$CROP=="FALW",] #fallow.

    #Isolate by crop spacing
    test_ir.corn.narr <- test_ir.corn[test_ir.corn$SPACING==0.75,]
    test_ir.corn.wide <- test_ir.corn[test_ir.corn$SPACING==1.5,]
    test_ir.corn.narr <- test_ir.corn[test_ir.corn$SPACING==0.75,]
    test_ir.corn.wide <- test_ir.corn[test_ir.corn$SPACING==1.5,]

    test_ir.bean.narr <- test_ir.bean[test_ir.bean$SPACING==0.75,]
    test_ir.bean.wide <- test_ir.bean[test_ir.bean$SPACING==1.5,]
    test_ir.bean.narr <- test_ir.bean[test_ir.bean$SPACING==0.75,]
    test_ir.bean.wide <- test_ir.bean[test_ir.bean$SPACING==1.5,]

    test_ir.rest.narr <- test_ir.rest[test_ir.rest$SPACING==0.75,]
    test_ir.rest.wide <- test_ir.rest[test_ir.rest$SPACING==1.5,]
    test_ir.rest.narr <- test_ir.rest[test_ir.rest$SPACING==0.75,]
    test_ir.rest.wide <- test_ir.rest[test_ir.rest$SPACING==1.5,]

    #Isolate by GDD high/low
    test_ir.corn.narr.hgdd <- test_ir.corn.narr[test_ir.corn.narr$GDD=="high",]
    test_ir.corn.wide.hgdd <- test_ir.corn.wide[test_ir.corn.wide$GDD=="high",]
    test_ir.corn.narr.lgdd <- test_ir.corn.narr[test_ir.corn.narr$GDD=="low",]
    test_ir.corn.wide.lgdd <- test_ir.corn.wide[test_ir.corn.wide$GDD=="low",]

    test_ir.bean.narr.hgdd <- test_ir.bean.narr[test_ir.bean.narr$GDD=="high",]
    test_ir.bean.wide.hgdd <- test_ir.bean.wide[test_ir.bean.wide$GDD=="high",]
    test_ir.bean.narr.lgdd <- test_ir.bean.narr[test_ir.bean.narr$GDD=="low",]
    test_ir.bean.wide.lgdd <- test_ir.bean.wide[test_ir.bean.wide$GDD=="low",]

    test_ir.rest.narr.hgdd <- test_ir.rest.narr[test_ir.rest.narr$GDD=="high",]
    test_ir.rest.wide.hgdd <- test_ir.rest.wide[test_ir.rest.wide$GDD=="high",]
    test_ir.rest.narr.lgdd <- test_ir.rest.narr[test_ir.rest.narr$GDD=="low",]
    test_ir.rest.wide.lgdd <- test_ir.rest.wide[test_ir.rest.wide$GDD=="low",]

    #Set conditions
    corn <- list()
    bean <- list()
    rest <- list()
    
    Corn.Tmp <- data.frame()
    Bean.Tmp <- data.frame()
    Rest.Tmp <- data.frame()
    stratvec <- c("MS", "IC1", "IC2", "IC3", "IC4", "MC1", "MC2") #vector of potential strategies
    corn.strat <- list()
    bean.strat <- list()
    rest.strat <- list()
    for(k in 1:7){
    
    	corn[[1]] <- test_ir.corn.narr.hgdd[test_ir.corn.narr.hgdd$SUIT==stratvec[k] & test_ir.corn.narr.hgdd$NUT==1,]
  	  corn[[2]] <- test_ir.corn.narr.hgdd[test_ir.corn.narr.hgdd$SUIT==stratvec[k] & test_ir.corn.narr.hgdd$NUT==2,]
  	  corn[[3]] <- test_ir.corn.narr.lgdd[test_ir.corn.narr.lgdd$SUIT==stratvec[k] & test_ir.corn.narr.lgdd$NUT==1,]
  	  corn[[4]] <- test_ir.corn.narr.lgdd[test_ir.corn.narr.lgdd$SUIT==stratvec[k] & test_ir.corn.narr.lgdd$NUT==2,]
  	  corn[[5]] <- test_ir.corn.wide.hgdd[test_ir.corn.wide.hgdd$SUIT==stratvec[k] & test_ir.corn.wide.hgdd$NUT==1,]
  	  corn[[6]] <- test_ir.corn.wide.hgdd[test_ir.corn.wide.hgdd$SUIT==stratvec[k] & test_ir.corn.wide.hgdd$NUT==2,]
  	  corn[[7]] <- test_ir.corn.wide.lgdd[test_ir.corn.wide.lgdd$SUIT==stratvec[k] & test_ir.corn.wide.lgdd$NUT==1,]
  	  corn[[8]] <- test_ir.corn.wide.lgdd[test_ir.corn.wide.lgdd$SUIT==stratvec[k] & test_ir.corn.wide.lgdd$NUT==2,]

  	  bean[[1]] <- test_ir.bean.narr.hgdd[test_ir.bean.narr.hgdd$SUIT==stratvec[k] & test_ir.bean.narr.hgdd$NUT==1,]
  	  bean[[2]] <- test_ir.bean.narr.hgdd[test_ir.bean.narr.hgdd$SUIT==stratvec[k] & test_ir.bean.narr.hgdd$NUT==2,]
  	  bean[[3]] <- test_ir.bean.narr.lgdd[test_ir.bean.narr.lgdd$SUIT==stratvec[k] & test_ir.bean.narr.lgdd$NUT==1,]
  	  bean[[4]] <- test_ir.bean.narr.lgdd[test_ir.bean.narr.lgdd$SUIT==stratvec[k] & test_ir.bean.narr.lgdd$NUT==2,]
    	bean[[5]] <- test_ir.bean.wide.hgdd[test_ir.bean.wide.hgdd$SUIT==stratvec[k] & test_ir.bean.wide.hgdd$NUT==1,]
  	  bean[[6]] <- test_ir.bean.wide.hgdd[test_ir.bean.wide.hgdd$SUIT==stratvec[k] & test_ir.bean.wide.hgdd$NUT==2,]
  	  bean[[7]] <- test_ir.bean.wide.lgdd[test_ir.bean.wide.lgdd$SUIT==stratvec[k] & test_ir.bean.wide.lgdd$NUT==1,]
  	  bean[[8]] <- test_ir.bean.wide.lgdd[test_ir.bean.wide.lgdd$SUIT==stratvec[k] & test_ir.bean.wide.lgdd$NUT==2,]

      rest[[1]] <- test_ir.rest.narr.hgdd[test_ir.rest.narr.hgdd$SUIT==stratvec[k] & test_ir.rest.narr.hgdd$NUT==1,]
      rest[[2]] <- test_ir.rest.narr.hgdd[test_ir.rest.narr.hgdd$SUIT==stratvec[k] & test_ir.rest.narr.hgdd$NUT==2,]
      rest[[3]] <- test_ir.rest.narr.lgdd[test_ir.rest.narr.lgdd$SUIT==stratvec[k] & test_ir.rest.narr.lgdd$NUT==1,]
      rest[[4]] <- test_ir.rest.narr.lgdd[test_ir.rest.narr.lgdd$SUIT==stratvec[k] & test_ir.rest.narr.lgdd$NUT==2,]
      rest[[5]] <- test_ir.rest.wide.hgdd[test_ir.rest.wide.hgdd$SUIT==stratvec[k] & test_ir.rest.wide.hgdd$NUT==1,]
      rest[[6]] <- test_ir.rest.wide.hgdd[test_ir.rest.wide.hgdd$SUIT==stratvec[k] & test_ir.rest.wide.hgdd$NUT==2,]
      rest[[7]] <- test_ir.rest.wide.lgdd[test_ir.rest.wide.lgdd$SUIT==stratvec[k] & test_ir.rest.wide.lgdd$NUT==1,]
      rest[[8]] <- test_ir.rest.wide.lgdd[test_ir.rest.wide.lgdd$SUIT==stratvec[k] & test_ir.rest.wide.lgdd$NUT==2,]

  	  #Summarises results by means over all possibilities.
  	  corn.df <- list()
  	  bean.df <- list()
  	  rest.df <- list()
  	  for(i in 1:8){
  	  	tmp1 <- corn[[i]]
  		  tmp2 <- tmp1[order(tmp1$YR),]
  		  tmp3 <- as.data.frame(tmp2 %>%
                      			group_by(YR) %>%
		                      	summarize(YIELD=mean(YLDG)))
  		  corn.df[[i]]  <- tmp3[tmp3$YR >= 850,]
	  	  rm(tmp1, tmp2, tmp3)

	      tmp1 <- bean[[i]]
	      tmp2 <- tmp1[order(tmp1$YR),]
	      tmp3 <- as.data.frame(tmp2 %>%
		                        group_by(YR) %>%
		                        summarize(YIELD=mean(YLDG)))
	      bean.df[[i]]  <- tmp3[tmp3$YR >= 850,]
	      rm(tmp1, tmp2, tmp3)

	      tmp1 <- rest[[i]]
	      tmp2 <- tmp1[order(tmp1$YR),]
	      tmp3 <- as.data.frame(tmp2 %>%
	                          group_by(YR) %>%
	                          summarize(YIELD=mean(YLDG)))
	      rest.df[[i]]  <- tmp3[tmp3$YR >= 850,]
	      rm(tmp1, tmp2, tmp3)
	    }

  	  CORN.DF <- corn.df %>%
	       Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="YR"), .)
	    CORN.DF <- data.frame(INDEX=rep(nsite, length(CORN.DF[,1])), LON=rep(sites[sites$INDEX==nsite,2], length(CORN.DF[,1])), LAT=rep(sites[sites$INDEX==nsite,3], length(CORN.DF[,1])), YEAR=CORN.DF[,1], YIELD_AV=rowMeans(CORN.DF[,-c(1)]))

	    Corn.tmp <- data.frame(STRATEGY=rep(stratvec[k], length(CORN.DF[,1])), CORN.DF)
	    Corn.Tmp <- rbind(Corn.Tmp, Corn.tmp)
	  
	    corn.strat[[k]] <- CORN.DF
	    rm(CORN.DF)

	    BEAN.DF <- corn.df %>%
	      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="YR"), .)
	    BEAN.DF <- data.frame(INDEX=rep(nsite, length(BEAN.DF[,1])), LON=rep(sites[sites$INDEX==nsite,2], length(BEAN.DF[,1])), LAT=rep(sites[sites$INDEX==nsite,3], length(BEAN.DF[,1])), YEAR=BEAN.DF[,1], YIELD_AV=rowMeans(BEAN.DF[,-c(1)]))

	    Bean.tmp <- data.frame(STRATEGY=rep(stratvec[k], length(BEAN.DF[,1])), BEAN.DF)
	    Bean.Tmp <- rbind(Bean.Tmp, Bean.tmp)

	    bean.strat[[k]] <- BEAN.DF
	    rm(BEAN.DF)

	    REST.DF <- rest.df %>%
	      Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="YR"), .)
	    REST.DF <- data.frame(INDEX=rep(nsite, length(REST.DF[,1])), LON=rep(sites[sites$INDEX==nsite,2], length(REST.DF[,1])), LAT=rep(sites[sites$INDEX==nsite,3], length(REST.DF[,1])), YEAR=REST.DF[,1], YIELD_AV=rowMeans(REST.DF[,-c(1)]))

	    Rest.tmp <- data.frame(STRATEGY=rep(stratvec[k], length(REST.DF[,1])), REST.DF)
	    Rest.Tmp <- rbind(Rest.Tmp, Rest.tmp)

	    rest.strat[[k]] <- REST.DF
	    rm(REST.DF)
	  
    }
    Corn.DF <- rbind(Corn.DF, Corn.Tmp)
    Bean.DF <- rbind(Bean.DF, Bean.Tmp)
    Rest.DF <- rbind(Rest.DF, Rest.Tmp)
  }

  m = 2
# path.tmp1 <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", m, "/EPIC_em00", m, "_irr_iter", j, "_corn_ave.csv", sep="")
# path.tmp2 <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", m, "/EPIC_em00", m, "_irr_iter", j, "_wbean_ave.csv", sep="")
# path.tmp3 <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", m, "/EPIC_em00", m, "_irr_iter", j, "_wfalw_ave.csv", sep="")
  path.tmp1 <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", m, "/EPIC_em00", m, "_rfd_iter", j, "_corn_ave.csv", sep="")
  path.tmp2 <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", m, "/EPIC_em00", m, "_rfd_iter", j, "_wbean_ave.csv", sep="")
  path.tmp3 <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", m, "/EPIC_em00", m, "_rfd_iter", j, "_wfalw_ave.csv", sep="")


  write.csv(Corn.DF, path.tmp1)
  write.csv(Bean.DF, path.tmp2)
  write.csv(Rest.DF, path.tmp3)

  rm(test_ir0, path.tmp1, path.tmp2, path.tmp3)
  gc()
  print(j)
  # write.csv(Corn.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em004/EPIC_em004_rfd_iter1_corn_ave.csv")
  # write.csv(Bean.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em004/EPIC_em004_rfd_iter1_wbean_ave.csv")
  # write.csv(Rest.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em004/EPIC_em004_rfd_iter1_wfalw_ave.csv")

}
#end

# write.csv(Corn.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em004/EPIC_em004_irr_iter9_corn_ave.csv")
# write.csv(Bean.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em004/EPIC_em004_irr_iter9_wbean_ave.csv")
# write.csv(Rest.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em004/EPIC_em004_irr_iter9_wfalw_ave.csv")

# write.csv(Corn.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em003/EPIC_em003_rfd_iter9_corn_ave.csv")
# write.csv(Bean.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em003/EPIC_em003_rfd_iter9_wbean_ave.csv")
# write.csv(Rest.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em003/EPIC_em003_rfd_iter9_wfalw_ave.csv")

# write.csv(Corn.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em003/EPIC_em003_irr_iter9_corn_ave.csv")
# write.csv(Bean.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em003/EPIC_em003_irr_iter9_wbean_ave.csv")
# write.csv(Rest.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em003/EPIC_em003_irr_iter9_wfalw_ave.csv")

# write.csv(Corn.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em002/EPIC_em002_irr_iter9_corn_ave.csv")
# write.csv(Bean.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em002/EPIC_em002_irr_iter9_wbean_ave.csv")
# write.csv(Rest.DF, "/media/zizroc/Extra Drive 1/Data/Output/CESM_em002/EPIC_em002_irr_iter9_wfalw_ave.csv")



CORN.DF <- data.frame()
BEAN.DF <- data.frame()
FALW.DF <- data.frame()
# nlist <- c(2,3,4,5)
nlist <- c(3,4,5) #3,4,5 looks fine
for(l in nlist){
  CORN.df <- data.frame()
  BEAN.df <- data.frame()
  FALW.df <- data.frame()
  for(i in 0:9){
    # corn.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em002/EPIC_em002_irr_iter", i,"_corn_ave.csv", sep="")
    # bean.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em002/EPIC_em002_irr_iter", i,"_wbean_ave.csv", sep="")
    # falw.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em002/EPIC_em002_irr_iter", i,"_wfalw_ave.csv", sep="")
  
    corn.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", l, "/EPIC_em00", l, "_irr_iter", i,"_corn_ave.csv", sep="")
    bean.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", l, "/EPIC_em00", l, "_irr_iter", i,"_wbean_ave.csv", sep="")
    falw.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", l, "/EPIC_em00", l, "_irr_iter", i,"_wfalw_ave.csv", sep="")
    # corn.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", l, "/EPIC_em00", l, "_rfd_iter", i,"_corn_ave.csv", sep="")
    # bean.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", l, "/EPIC_em00", l, "_rfd_iter", i,"_wbean_ave.csv", sep="")
    # falw.path <- paste("/media/zizroc/Extra Drive 1/Data/Output/CESM_em00", l, "/EPIC_em00", l, "_rfd_iter", i,"_wfalw_ave.csv", sep="")
    
    corn.df <- read.csv(corn.path, header=T)
    bean.df <- read.csv(bean.path, header=T)
    falw.df <- read.csv(falw.path, header=T)
    corn.df <- data.frame(ITER= rep(i, length(corn.df[,1])), corn.df[,-c(1)])
    bean.df <- data.frame(ITER= rep(i, length(bean.df[,1])), bean.df[,-c(1)])
    falw.df <- data.frame(ITER= rep(i, length(falw.df[,1])), falw.df[,-c(1)])
    CORN.df <- rbind(CORN.df, corn.df)
    BEAN.df <- rbind(BEAN.df, bean.df)
    FALW.df <- rbind(FALW.df, falw.df)
  }
  CORN.DF <- rbind(CORN.DF, CORN.df)
  BEAN.DF <- rbind(BEAN.DF, BEAN.df)
  FALW.DF <- rbind(FALW.DF, FALW.df)
}

# pdf("/media/zizroc/Extra Drive 1/IIASA/Fremont_ensemblemean_irrigated_ch2.pdf")
# pdf("/media/zizroc/Extra Drive 1/IIASA/Fremont_ensemblemean_rainfed_ch2.pdf")

corn.strat <- list()
halflife   <- data.frame()
for(nsite in nsites){
  
  corn.strat[[1]] <- CORN.DF[CORN.DF$STRATEGY=="MS" & CORN.DF$INDEX==nsite,]  #Milpa strategy
  corn.strat[[2]] <- CORN.DF[CORN.DF$STRATEGY=="IC1" & CORN.DF$INDEX==nsite,] #Intensive cropping strategy 1
  corn.strat[[3]] <- CORN.DF[CORN.DF$STRATEGY=="IC2" & CORN.DF$INDEX==nsite,] #Intensive cropping strategy 2
  corn.strat[[4]] <- CORN.DF[CORN.DF$STRATEGY=="IC3" & CORN.DF$INDEX==nsite,] #Intensive cropping strategy 3
  corn.strat[[5]] <- CORN.DF[CORN.DF$STRATEGY=="IC4" & CORN.DF$INDEX==nsite,] #Intensive cropping strategy 4
  corn.strat[[6]] <- CORN.DF[CORN.DF$STRATEGY=="MC1" & CORN.DF$INDEX==nsite,] #Mixed cropping strategy 1
  corn.strat[[7]] <- CORN.DF[CORN.DF$STRATEGY=="MC2" & CORN.DF$INDEX==nsite,] #Mixed cropping strategy 2
  
  test <- data.frame()
  for(j in 1:7){
    test <- rbind(test, corn.strat[[j]][7])
  }
  ymax <- max(test)
  if(ymax < 1) ymax = 1
  
  site.spd.col <- alpha(c("purple", "blue"), 0.1)
  line.cols <- alpha(c(1, 2, 3, 4, 5, 6, 7), 0.7)
  par(mfrow=c(2,2), mar=c(4,4,2,2))
  plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
  points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
  for(k in 1:7){
    points(corn.strat[[2]][corn.strat[[2]]$ITER==k, 6:7], col=alpha(2, 0.1), lwd=0.5, pch=17, cex=0.3)
    points(corn.strat[[3]][corn.strat[[3]]$ITER==k, 6:7], col=alpha(3, 0.1), lwd=0.5, pch=19, cex=0.3)
  }
  corn.tmp1 <- as.data.frame(corn.strat[[2]] %>%
                             group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp2 <- as.data.frame(corn.strat[[3]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  
  #finds out the time of the maximum, 1/2 max and 1/4 max of yields
  corn.tmp1.lo <- predict(loess(corn.tmp1$`mean(YIELD_AV)` ~ corn.tmp1$YEAR, span=0.2), se=T)
  corn.tmp2.lo <- predict(loess(corn.tmp2$`mean(YIELD_AV)` ~ corn.tmp2$YEAR, span=0.2), se=T)
  tmp1.fit <- data.frame(YEAR=corn.tmp1$YEAR, FIT=corn.tmp1.lo$fit)
  tmp2.fit <- data.frame(YEAR=corn.tmp2$YEAR, FIT=corn.tmp2.lo$fit)
  yr.start1     <- tmp1.fit[tmp1.fit$FIT==max(tmp1.fit$FIT), 1]
  yr.start2     <- tmp2.fit[tmp2.fit$FIT==max(tmp2.fit$FIT), 1]
  yr.halflife1  <- min(tmp1.fit[tmp1.fit$FIT <= (0.5*max(tmp1.fit$FIT)), 1])
  yr.halflife2  <- min(tmp2.fit[tmp2.fit$FIT <= (0.5*max(tmp2.fit$FIT)), 1])
  yr.2halflife1 <- min(tmp1.fit[tmp1.fit$FIT <= (0.25*max(tmp1.fit$FIT)), 1])
  yr.2halflife2 <- min(tmp2.fit[tmp2.fit$FIT <= (0.25*max(tmp2.fit$FIT)), 1])
  halflife.tmp1 <- data.frame(SITE=corn.strat[[2]][1,3], LON=corn.strat[[2]][1,]$LON, LAT=corn.strat[[2]][1,]$LAT, STRATEGY="IC1", START_YR=yr.start1, HALFLIFE_YR=yr.halflife1, QUARTLIFE_YR=yr.2halflife1)
  halflife.tmp2 <- data.frame(SITE=corn.strat[[3]][1,3], LON=corn.strat[[3]][1,]$LON, LAT=corn.strat[[3]][1,]$LAT, STRATEGY="IC2", START_YR=yr.start2, HALFLIFE_YR=yr.halflife2, QUARTLIFE_YR=yr.2halflife2)
  halflife <- rbind(halflife, halflife.tmp1, halflife.tmp2)
  
  lines(rollapply(corn.tmp1, 21, mean), col=line.cols[2], lwd=3)
  lines(rollapply(corn.tmp2, 21, mean), col=line.cols[3], lwd=3)
  legend("topright", col=line.cols[2:3], lty=c(1,1), c("IC1", "IC2"), bty="n", lwd=c(2,2))
  # legend("topleft", paste("IRRIGATED YIELD, site", corn.strat[[1]][1,3], "at", round(corn.strat[[1]][1,]$LON, 2), "W", round(corn.strat[[1]][1,]$LAT, 2), "N", sep=" "), bty="n", cex=0.7)
  # legend("topleft", paste("IRRIGATED  YIELD, site", corn.strat[[1]][1,3], sep=" "), bty="n", cex=0.7)
  legend("topleft", paste("RAINFED  YIELD, site", corn.strat[[1]][1,3], sep=" "), bty="n", cex=0.7)
  rm(halflife.tmp1, halflife.tmp2)
  
  plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
  points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
  for(k in 1:7){
    points(corn.strat[[4]][corn.strat[[4]]$ITER==k, 6:7], col=alpha(4, 0.1), lwd=0.5, pch=17, cex=0.3)
    points(corn.strat[[5]][corn.strat[[5]]$ITER==k, 6:7], col=alpha(5, 0.1), lwd=0.5, pch=19, cex=0.3)
  }
  corn.tmp1 <- as.data.frame(corn.strat[[4]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp2 <- as.data.frame(corn.strat[[5]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  
  ##
  #Additional analysis of yield half-lives.
  
  #finds out the time of the maximum, 1/2 max and 1/4 max of yields
  corn.tmp1.lo <- predict(loess(corn.tmp1$`mean(YIELD_AV)` ~ corn.tmp1$YEAR, span=0.2), se=T)
  corn.tmp2.lo <- predict(loess(corn.tmp2$`mean(YIELD_AV)` ~ corn.tmp2$YEAR, span=0.2), se=T)
  tmp1.fit <- data.frame(YEAR=corn.tmp1$YEAR, FIT=corn.tmp1.lo$fit)
  tmp2.fit <- data.frame(YEAR=corn.tmp2$YEAR, FIT=corn.tmp2.lo$fit)
  yr.start1     <- tmp1.fit[tmp1.fit$FIT==max(tmp1.fit$FIT), 1]
  yr.start2     <- tmp2.fit[tmp2.fit$FIT==max(tmp2.fit$FIT), 1]
  yr.halflife1  <- min(tmp1.fit[tmp1.fit$FIT <= (0.5*max(tmp1.fit$FIT)), 1])
  yr.halflife2  <- min(tmp2.fit[tmp2.fit$FIT <= (0.5*max(tmp2.fit$FIT)), 1])
  yr.2halflife1 <- min(tmp1.fit[tmp1.fit$FIT <= (0.25*max(tmp1.fit$FIT)), 1])
  yr.2halflife2 <- min(tmp2.fit[tmp2.fit$FIT <= (0.25*max(tmp2.fit$FIT)), 1])
  halflife.tmp1 <- data.frame(SITE=corn.strat[[4]][1,3], LON=corn.strat[[4]][1,]$LON, LAT=corn.strat[[4]][1,]$LAT, STRATEGY="IC3", START_YR=yr.start1, HALFLIFE_YR=yr.halflife1, QUARTLIFE_YR=yr.2halflife1)
  halflife.tmp2 <- data.frame(SITE=corn.strat[[5]][1,3], LON=corn.strat[[5]][1,]$LON, LAT=corn.strat[[5]][1,]$LAT, STRATEGY="IC4", START_YR=yr.start2, HALFLIFE_YR=yr.halflife2, QUARTLIFE_YR=yr.2halflife2)
  halflife <- rbind(halflife, halflife.tmp1, halflife.tmp2)
  
  lines(rollapply(corn.tmp1, 21, mean), col=line.cols[4], lwd=3)
  lines(rollapply(corn.tmp2, 21, mean), col=line.cols[5], lwd=3)
  legend("topright", col=line.cols[4:5], lty=c(1,1), c("IC3", "IC4"), bty="n", lwd=c(2,2))
  rm(halflife.tmp1, halflife.tmp2)
  
  plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
  points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
  for(k in 1:7){
    points(corn.strat[[6]][corn.strat[[6]]$ITER==k, 6:7], col=alpha(6, 0.1), lwd=0.5, pch=17, cex=0.3)
    points(corn.strat[[7]][corn.strat[[7]]$ITER==k, 6:7], col=alpha(7, 0.1), lwd=0.5, pch=19, cex=0.3)
  }
  corn.tmp1 <- as.data.frame(corn.strat[[6]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp2 <- as.data.frame(corn.strat[[7]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  
  #finds out the time of the maximum, 1/2 max and 1/4 max of yields
  corn.tmp1.lo <- predict(loess(corn.tmp1$`mean(YIELD_AV)` ~ corn.tmp1$YEAR, span=0.2), se=T)
  corn.tmp2.lo <- predict(loess(corn.tmp2$`mean(YIELD_AV)` ~ corn.tmp2$YEAR, span=0.2), se=T)
  tmp1.fit <- data.frame(YEAR=corn.tmp1$YEAR, FIT=corn.tmp1.lo$fit)
  tmp2.fit <- data.frame(YEAR=corn.tmp2$YEAR, FIT=corn.tmp2.lo$fit)
  yr.start1     <- tmp1.fit[tmp1.fit$FIT==max(tmp1.fit$FIT), 1]
  yr.start2     <- tmp2.fit[tmp2.fit$FIT==max(tmp2.fit$FIT), 1]
  yr.halflife1  <- min(tmp1.fit[tmp1.fit$FIT <= (0.5*max(tmp1.fit$FIT)), 1])
  yr.halflife2  <- min(tmp2.fit[tmp2.fit$FIT <= (0.5*max(tmp2.fit$FIT)), 1])
  yr.2halflife1 <- min(tmp1.fit[tmp1.fit$FIT <= (0.25*max(tmp1.fit$FIT)), 1])
  yr.2halflife2 <- min(tmp2.fit[tmp2.fit$FIT <= (0.25*max(tmp2.fit$FIT)), 1])
  halflife.tmp1 <- data.frame(SITE=corn.strat[[6]][1,3], LON=corn.strat[[6]][1,]$LON, LAT=corn.strat[[6]][1,]$LAT, STRATEGY="MC1", START_YR=yr.start1, HALFLIFE_YR=yr.halflife1, QUARTLIFE_YR=yr.2halflife1)
  halflife.tmp2 <- data.frame(SITE=corn.strat[[7]][1,3], LON=corn.strat[[7]][1,]$LON, LAT=corn.strat[[7]][1,]$LAT, STRATEGY="MC2", START_YR=yr.start2, HALFLIFE_YR=yr.halflife2, QUARTLIFE_YR=yr.2halflife2)
  halflife <- rbind(halflife, halflife.tmp1, halflife.tmp2)
  ##
  
  lines(rollapply(corn.tmp1, 21, mean), col=line.cols[6], lwd=3)
  lines(rollapply(corn.tmp2, 21, mean), col=line.cols[7], lwd=3)
  legend("topright", col=line.cols[6:7], lty=c(1,1), c("MC1", "MC2"), bty="n", lwd=c(2,2))
  rm(halflife.tmp1, halflife.tmp2)
  
  plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
  points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
  for(k in 1:7){
    points(corn.strat[[1]][corn.strat[[1]]$ITER==k, 6:7], col=alpha(1, 0.1), lwd=0.5, pch=20, cex=0.3)
  }
  corn.tmp <- as.data.frame(corn.strat[[1]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  
  #finds out the time of the maximum, 1/2 max and 1/4 max of yields
  corn.tmp.lo  <- predict(loess(corn.tmp1$`mean(YIELD_AV)` ~ corn.tmp1$YEAR, span=0.2), se=T)
  tmp1.fit     <- data.frame(YEAR=corn.tmp1$YEAR, FIT=corn.tmp1.lo$fit)
  yr.start     <- tmp1.fit[tmp1.fit$FIT==max(tmp1.fit$FIT), 1]
  yr.halflife  <- min(tmp1.fit[tmp1.fit$FIT <= (0.5*max(tmp1.fit$FIT)), 1])
  # yr.halflife  <- min(tmp1.fit[tmp1.fit$FIT <= (0.5*max(tmp1.fit$FIT)), 1])
  yr.2halflife <- min(tmp1.fit[tmp1.fit$FIT <= (0.25*max(tmp1.fit$FIT)), 1])
  halflife.tmp <- data.frame(SITE=corn.strat[[1]][1,3], LON=corn.strat[[1]][1,]$LON, LAT=corn.strat[[1]][1,]$LAT, STRATEGY="MS", START_YR=yr.start, HALFLIFE_YR=yr.halflife, QUARTLIFE_YR=yr.2halflife)
  halflife     <- rbind(halflife, halflife.tmp)
  
  lines(rollapply(corn.tmp, 21, mean), col=line.cols[1], lwd=3)
  legend("topright", lty=1, col=1, "MS", bty="n", lwd=2)
  legend("topleft", fill=site.spd.col, c("2-sigma fit", "1-sigma fit"), bty="n")

  
  #Data for Tamas
  corn.tmp1 <- as.data.frame(corn.strat[[1]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp2 <- as.data.frame(corn.strat[[2]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp3 <- as.data.frame(corn.strat[[3]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp4 <- as.data.frame(corn.strat[[4]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp5 <- as.data.frame(corn.strat[[5]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp6 <- as.data.frame(corn.strat[[6]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  corn.tmp7 <- as.data.frame(corn.strat[[7]] %>%
                               group_by(YEAR) %>%
                               summarize(mean(YIELD_AV)))
  
  
  har.tmp1 <- data.frame(SITE = as.vector(corn.strat[[1]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[1]]$STRATEGY[1]), HARVEST_100YR = sum(corn.tmp1[1:100,2]), HARVEST_200YR = sum(corn.tmp1[1:200,2]))
  har.tmp2 <- data.frame(SITE = as.vector(corn.strat[[2]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[2]]$STRATEGY[2]), HARVEST_100YR = sum(corn.tmp2[1:100,2]), HARVEST_200YR = sum(corn.tmp2[1:200,2]))
  har.tmp3 <- data.frame(SITE = as.vector(corn.strat[[3]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[3]]$STRATEGY[3]), HARVEST_100YR = sum(corn.tmp3[1:100,2]), HARVEST_200YR = sum(corn.tmp3[1:200,2]))
  har.tmp4 <- data.frame(SITE = as.vector(corn.strat[[4]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[4]]$STRATEGY[4]), HARVEST_100YR = sum(corn.tmp4[1:100,2]), HARVEST_200YR = sum(corn.tmp4[1:200,2]))
  har.tmp5 <- data.frame(SITE = as.vector(corn.strat[[5]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[5]]$STRATEGY[5]), HARVEST_100YR = sum(corn.tmp5[1:100,2]), HARVEST_200YR = sum(corn.tmp5[1:200,2]))
  har.tmp6 <- data.frame(SITE = as.vector(corn.strat[[6]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[6]]$STRATEGY[6]), HARVEST_100YR = sum(corn.tmp6[1:100,2]), HARVEST_200YR = sum(corn.tmp6[1:200,2]))
  har.tmp7 <- data.frame(SITE = as.vector(corn.strat[[7]]$INDEX[1]), LON = as.vector(corn.strat[[1]]$LON[1]), LAT = as.vector(corn.strat[[1]]$LAT[1]), STRATEGY = as.vector(corn.strat[[7]]$STRATEGY[7]), HARVEST_100YR = sum(corn.tmp7[1:100,2]), HARVEST_200YR = sum(corn.tmp7[1:200,2]))
  
  harvest.df <- rbind(har.tmp1, har.tmp2, har.tmp3, har.tmp4, har.tmp5, har.tmp6, har.tmp7)

  data.df <- data.frame(INDEX=rep(corn.strat[[4]][1,3], length(corn.tmp1[,1])), LON=rep(corn.strat[[4]][1,4], length(corn.tmp1[,1])), LAT=rep(corn.strat[[4]][1,5], length(corn.tmp1[,1])), YEAR=corn.tmp1[,1], YIELD_AV_MS=corn.tmp1[,2], YIELD_AV_IC1=corn.tmp2[,2], YIELD_AV_IC2=corn.tmp3[,2], YIELD_AV_IC3=corn.tmp4[,2], YIELD_AV_IC4=corn.tmp5[,2], YIELD_AV_MC1=corn.tmp6[,2], YIELD_AV_MC2=corn.tmp7[,2], SPD_2SIGMA=site.spd.2sig[[nsite]][1:length(corn.tmp1[,1]),4])
  
  if(nsite==min(nsites)){
    Data.df <- data.df
    Harvest.df <- harvest.df
  }
  if(nsite >min(nsites)){
    Data.df <- rbind(Data.df, data.df)
    Harvest.df <- rbind(Harvest.df, harvest.df)
  }
  
  # data.df[[nsite]] <- data.frame(SITE_ID=rep(corn.strat[[1]]$INDEX[1], length(corn.tmp$YEAR)), LON=rep(corn.strat[[1]]$LON[1], length(corn.tmp$YEAR)), LAT=rep(corn.strat[[1]]$LAT, length(corn.tmp$YEAR)), YEAR=corn.tmp$YEAR, YIELD_AV_MS=corn.tmp$`mean(YIELD_AV)`, YIELD_AV_IC1=corn.tmp1$`mean(YIELD_AV)`, YIELD_AV_IC2=corn.tmp2$`mean(YIELD_AV)`, YIELD_AV_IC3=corn.tmp2$`mean(YIELD_AV)`, YIELD_AV_IC4=corn.tmp2$`mean(YIELD_AV)`, YIELD_AV_MC1=corn.tmp2$`mean(YIELD_AV)`, YIELD_AV_MC2=corn.tmp2$`mean(YIELD_AV)`)
  
  
  rm(halflife.tmp)
  rm(test)
}
# dev.off()

# tmp <- vector()
# for(p in 1:dim(halflife)[1]){
#   tmp0 <- halflife[p, 2:3]
#   if(tmp0$LON < -111.5) tmp[p] = "GB" #Great Basin
#   if(tmp0$LON >= -111.5) tmp[p] = "COP" #Colorado Plateau
#   if(tmp0$LAT > 40 & tmp0$LON < -111) tmp[p] =  "GSL" #Great Salt Lake
#   if(tmp0$LAT > 38.5 & tmp0$LON >= -111) tmp[p] = "UNB" #Uinta Basin
#   if(tmp0$LAT <= 40 & tmp0$LAT > 38 & tmp0$LON < -111) tmp[p] = "SEVPAR" #Sevier and Parowan
#   if(tmp0$LAT <= 38) tmp[p] = "SOUTH" #Southern Fremont-Anasazi area
# }

Data.rfd <- data.rfd[data.rfd$LON >= -111.45 & data.rfd$LAT > 40,] #Uinta Basin
Data.irr <- data.irr[data.irr$LON >= -111.45 & data.irr$LAT > 40,] #Uinta Basin


# save(Data.df, file="/home/zizroc/Desktop/data_rfd.Rdata")
save(Data.df, file="/home/zizroc/Desktop/data_irr.Rdata")

# data.rfd <- Data.df

save(Harvest.irr, file="/home/zizroc/Desktop/Harvest_IRR.Rdata")
save(Harvest.rfd, file="/home/zizroc/Desktop/Harvest_RFD.Rdata")

load(file="/home/zizroc/Desktop/Harvest_IRR.Rdata")
load(file="/home/zizroc/Desktop/Harvest_RFD.Rdata")

Data.rfd <- data.rfd[data.rfd$LON < -111 & data.rfd$LAT > 40,] #Great Salt Lake
Data.irr <- data.irr[data.irr$LON < -111 & data.irr$LAT > 40,] #Great Salt Lake

Data.rfd <- data.rfd[data.rfd$SITE >= 149,] #Range Creek
Data.irr <- data.irr[data.irr$SITE >= 149,] #Range Creek



Harvest.df <- Harvest.rfd[Harvest.rfd$LON >= -111.45 & Harvest.rfd$LAT > 38.5,] #Uinta Basin
Harvest.df <- Harvest.irr[Harvest.irr$LON >= -111.45 & Harvest.irr$LAT > 38.5,] #Uinta Basin

Harvest.df <- Harvest.rfd[Harvest.rfd$LON < -111 & Harvest.rfd$LAT > 40,] #Great Salt Lake
Harvest.df <- Harvest.irr[Harvest.irr$LON < -111 & Harvest.irr$LAT > 40,] #Great Salt Lake

Harvest.df <- Harvest.rfd[Harvest.rfd$SITE >= 149,] #Range Creek
Harvest.df <- Harvest.irr[Harvest.irr$SITE >= 149,] #Range Creek


par(mfrow=c(3,2), mar=c(4,5,1,1))
col1 = alpha("#CC66FF", 0.5)
col2 = alpha("#339966", 0.5)

ymaxs = c(18, 40, 75)

site.names <- c("LOWER RANGE CREEK", "MIDDLE RANGE CREEK", "UPPER RANGE CREEK")
for(j in 151:149){
  rc.rfd    <- data.frame(Harvest.rfd[Harvest.rfd$SITE==j, 5], Harvest.rfd[Harvest.rfd$SITE==j, 6]-Harvest.rfd[Harvest.rfd$SITE==j, 5])
  rownames(rc.rfd) <- as.vector(Harvest.rfd[Harvest.rfd$SITE==j, 4])
  rc.rfd    <- rc.rfd[c(2:7,1),]
  tr.rc.rfd <- t(rc.rfd)
  barplot(tr.rc.rfd, col=c(col1, col2), beside=T, xlab="STRATEGY", ylab=expression(TOTAL ~ HARVEST ~ (tonnes ~ ha^-1 ~ century ^-1)), border="red", ylim=c(0, ymaxs[j-148]))
  legend("topleft", c("RFD HARVEST, 1-100 YRS", "RFD HARVEST, 101-200 YRS"), fill=c(col1, col2), bty="n", bg="n")
  legend("topright", site.names[j-148], bty="n", bg="n", cex=1.5)
  
  rc.irr    <- data.frame(Harvest.irr[Harvest.irr$SITE==j, 5], Harvest.irr[Harvest.irr$SITE==j, 6]-Harvest.irr[Harvest.irr$SITE==j, 5])
  rownames(rc.irr) <- as.vector(Harvest.irr[Harvest.irr$SITE==j, 4])
  rc.irr    <- rc.irr[c(2:7,1),]
  tr.rc.irr <- t(rc.irr)
  barplot(tr.rc.irr, col=c(col1, col2), beside=T, xlab="STRATEGY", ylab=expression(TOTAL ~ HARVEST ~ (tonnes ~ ha^-1 ~ century ^-1)), border="blue", ylim=c(0, ymaxs[j-148]))
  legend("topleft", c("IRR HARVEST, 1-100 YRS", "IRR HARVEST, 101-200 YRS"), fill=c(col1, col2), bty="n", bg="n")
  legend("topright", site.names[j-148], bty="n", bg="n", cex=1.5)
  
}


Harvest.df <- Harvest.rfd[Harvest.rfd$LON <= -111.45 & Harvest.rfd$LAT < 41.1 & Harvest.rfd$LAT > 37,] #Great Basin
Harvest.df <- Harvest.irr[Harvest.irr$LON <= -111.45 & Harvest.irr$LAT < 41.1 & Harvest.irr$LAT > 37,] #Great Basin

Harvest.df <- Harvest.rfd[Harvest.rfd$LON > -111.45 & Harvest.rfd$LAT < 41.1 & Harvest.rfd$LAT > 37,] #Colorado Plateau
Harvest.df <- Harvest.irr[Harvest.irr$LON > -111.45 & Harvest.irr$LAT < 41.1 & Harvest.irr$LAT > 37,] #Colorado Plateau

par(mfrow=c(7,2), mar=c(3,4,0,0))
col1 = alpha("#CC66FF", 0.5)
col2 = alpha("#339966", 0.5)
col3 = alpha("#877DBA", 0.5)
col4 = alpha("purple", 0.7)
#xmax= 140 #for irrigated case
# xmax= 30 #for rfd UB case
# xmax = 90 #irr UB case
xmax= 90
k = -5
ymax = 10+k
# xlim1 = c(-80,20) #for irrigated case
# xlim1 = c(-100,50) #for rainfed case
hist(Harvest.df[Harvest.df$STRATEGY=="IC1",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="IC1",6]-Harvest.df[Harvest.df$STRATEGY=="IC1",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "IC1", bty="n", cex=1.8)
legend("bottomright", c("HARVEST, YRS 1-100", "HARVEST, YRS 101-200", "NO CHANGE"), fill=c(col1, col2, col3), bty="n", bg="n")
hist(100*(Harvest.df[Harvest.df$STRATEGY=="IC1",6]-2*Harvest.df[Harvest.df$STRATEGY=="IC1",5])/Harvest.df[Harvest.df$STRATEGY=="IC1",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "IC1", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")
ymax = 10+k
hist(Harvest.df[Harvest.df$STRATEGY=="IC2",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="IC2",6]-Harvest.df[Harvest.df$STRATEGY=="IC2",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "IC2", bty="n", cex=1.8)
hist(100*(Harvest.df[Harvest.df$STRATEGY=="IC2",6]-2*Harvest.df[Harvest.df$STRATEGY=="IC2",5])/Harvest.df[Harvest.df$STRATEGY=="IC2",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "IC2", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")
ymax = 10+k
hist(Harvest.df[Harvest.df$STRATEGY=="IC3",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="IC3",6]-Harvest.df[Harvest.df$STRATEGY=="IC3",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "IC3", bty="n", cex=1.8)
hist(100*(Harvest.df[Harvest.df$STRATEGY=="IC3",6]-2*Harvest.df[Harvest.df$STRATEGY=="IC3",5])/Harvest.df[Harvest.df$STRATEGY=="IC3",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "IC3", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")
ymax = 10+k
hist(Harvest.df[Harvest.df$STRATEGY=="IC4",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="IC4",6]-Harvest.df[Harvest.df$STRATEGY=="IC4",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "IC4", bty="n", cex=1.8)
hist(100*(Harvest.df[Harvest.df$STRATEGY=="IC4",6]-2*Harvest.df[Harvest.df$STRATEGY=="IC4",5])/Harvest.df[Harvest.df$STRATEGY=="IC4",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "IC4", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")
ymax = 10+k
hist(Harvest.df[Harvest.df$STRATEGY=="MC1",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="MC1",6]-Harvest.df[Harvest.df$STRATEGY=="MC1",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "MC1", bty="n", cex=1.8)
hist(100*(Harvest.df[Harvest.df$STRATEGY=="MC1",6]-2*Harvest.df[Harvest.df$STRATEGY=="MC1",5])/Harvest.df[Harvest.df$STRATEGY=="MC1",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "MC1", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")
ymax = 10+k
hist(Harvest.df[Harvest.df$STRATEGY=="MC2",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="MC2",6]-Harvest.df[Harvest.df$STRATEGY=="MC2",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "MC2", bty="n", cex=1.8)
hist(100*(Harvest.df[Harvest.df$STRATEGY=="MC2",6]-2*Harvest.df[Harvest.df$STRATEGY=="MC2",5])/Harvest.df[Harvest.df$STRATEGY=="MC2",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "MC2", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")
ymax = 10+k
hist(Harvest.df[Harvest.df$STRATEGY=="MS",5], xlim=c(0,xmax), main="", xlab="", col=col1, ylim=c(0,ymax))
hist(Harvest.df[Harvest.df$STRATEGY=="MS",6]-Harvest.df[Harvest.df$STRATEGY=="MS",5], col=col2, add=T)
legend("topleft", "TOTAL HARVEST (tonnes)", bty="n", bg="n")
legend("topright", "MS", bty="n", cex=1.8)
hist(100*(Harvest.df[Harvest.df$STRATEGY=="MS",6]-2*Harvest.df[Harvest.df$STRATEGY=="MS",5])/Harvest.df[Harvest.df$STRATEGY=="MS",5], main="", xlab="", col=col4, xlim=xlim1)
abline(v=0)
legend("topright", "MS", bty="n", cex=1.8)
legend("topleft", "PERCENT DIFFERENCE, 101-200 TO 1-100 YR HARVEST", bty="n")

###


write.csv(Data.df,  file="/home/zizroc/Desktop/data1_IRR.csv")
write.csv(halflife,  file="/home/zizroc/Desktop/data2.csv")

spd.sites <- data.frame()
for(nsite in nsites[-c(105:107)]){ #Drop the Range Creek sites for now
  min.spd.1sig.tmp <- min(site.spd.1sig[[nsite]][,3])
  max.spd.1sig.tmp <- max(site.spd.1sig[[nsite]][,3])
  
  min.spd.2sig.tmp <- min(site.spd.1sig[[nsite]][,3])
  max.spd.2sig.tmp <- max(site.spd.2sig[[nsite]][,3])
  
  # if(min.spd.1sig.tmp <= 850) min.spd.1sig.tmp = 850
  # if(max.spd.1sig.tmp <= 850) max.spd.1sig.tmp = 850  
  # if(min.spd.2sig.tmp <= 850) min.spd.2sig.tmp = 850
  # if(max.spd.2sig.tmp <= 850) max.spd.2sig.tmp = 850
  # if(min.spd.1sig.tmp <= 850) min.spd.1sig.tmp = 0
  # if(max.spd.1sig.tmp <= 850) max.spd.1sig.tmp = 0  
  # if(min.spd.2sig.tmp <= 850) min.spd.2sig.tmp = 0
  # if(max.spd.2sig.tmp <= 850) max.spd.2sig.tmp = 0
  
  tmp <- data.frame(SITE=nsite, MIN_YR_1SIG=min.spd.1sig.tmp, MAX_YR_1SIG=max.spd.1sig.tmp, MIN_YR_2SIG=min.spd.2sig.tmp, MAX_YR_2SIG=max.spd.2sig.tmp)
  spd.sites <- rbind(spd.sites, tmp)
  rm(tmp)
}

diff.1sig = spd.sites$MAX_YR_1SIG-spd.sites$MIN_YR_1SIG
diff.2sig = spd.sites$MAX_YR_2SIG-spd.sites$MIN_YR_2SIG

sites.temp <- data.frame(SITE=spd.sites$SITE, DIFF_1SIG=diff.1sig, DIFF_2SIG=diff.2sig)
# sites.ord <- sites.temp[order(sites.temp$DIFF_1SIG),]
sites.ord <- sites.temp[order(sites.temp$DIFF_2SIG),]

tmp <- vector()
for(p in 1:dim(halflife)[1]){
  tmp0 <- halflife[p, 2:3]
  if(tmp0$LON < -111.5) tmp[p] = "GB" #Great Basin
  if(tmp0$LON >= -111.5) tmp[p] = "COP" #Colorado Plateau
  if(tmp0$LAT > 40 & tmp0$LON < -111) tmp[p] =  "GSL" #Great Salt Lake
  if(tmp0$LAT > 38.5 & tmp0$LON >= -111) tmp[p] = "UNB" #Uinta Basin
  if(tmp0$LAT <= 40 & tmp0$LAT > 38 & tmp0$LON < -111) tmp[p] = "SEVPAR" #Sevier and Parowan
  if(tmp0$LAT <= 38) tmp[p] = "SOUTH" #Southern Fremont-Anasazi area
}

halflife <- data.frame(halflife, GROUP=tmp)

ic1 <- halflife[halflife$STRATEGY=="IC1" & halflife$HALFLIFE_YR != "Inf",]
ic2 <- halflife[halflife$STRATEGY=="IC2" & halflife$HALFLIFE_YR != "Inf",]
ic3 <- halflife[halflife$STRATEGY=="IC3" & halflife$HALFLIFE_YR != "Inf",]
ic4 <- halflife[halflife$STRATEGY=="IC4" & halflife$HALFLIFE_YR != "Inf",]
mc1 <- halflife[halflife$STRATEGY=="MC1" & halflife$HALFLIFE_YR != "Inf",]
mc2 <- halflife[halflife$STRATEGY=="MC2" & halflife$HALFLIFE_YR != "Inf",]
ms  <- halflife[halflife$STRATEGY=="MS"  & halflife$HALFLIFE_YR != "Inf",]

ic1.df <- merge(sites.temp, ic1)
ic2.df <- merge(sites.temp, ic2)
ic3.df <- merge(sites.temp, ic3)
ic4.df <- merge(sites.temp, ic4)
mc1.df <- merge(sites.temp, mc1)
mc2.df <- merge(sites.temp, mc2)
ms.df  <- merge(sites.temp, ms)

ic1.df <- ic1.df[ic1.df$START_YR>850,]
ic2.df <- ic2.df[ic2.df$START_YR>850,]
ic3.df <- ic3.df[ic3.df$START_YR>850,]
ic4.df <- ic4.df[ic4.df$START_YR>850,]
mc1.df <- mc1.df[mc1.df$START_YR>850,]
mc2.df <- mc2.df[mc2.df$START_YR>850,]
ms.df  <- ms.df[ms.df$START_YR>850,]

# Find the duration of the "decay" to half maize-productivity
ic1.df <- data.frame(ic1.df, LAMBDA=(ic1.df$HALFLIFE_YR - ic1.df$START_YR))
ic2.df <- data.frame(ic2.df, LAMBDA=(ic2.df$HALFLIFE_YR - ic2.df$START_YR))
ic3.df <- data.frame(ic3.df, LAMBDA=(ic3.df$HALFLIFE_YR - ic3.df$START_YR))
ic4.df <- data.frame(ic4.df, LAMBDA=(ic4.df$HALFLIFE_YR - ic4.df$START_YR))
mc1.df <- data.frame(mc1.df, LAMBDA=(mc1.df$HALFLIFE_YR - mc1.df$START_YR))
mc2.df <- data.frame(mc2.df, LAMBDA=(mc2.df$HALFLIFE_YR - mc2.df$START_YR))
ms.df  <- data.frame(ms.df,  LAMBDA=(ms.df$HALFLIFE_YR  - ms.df$START_YR))

ic1.df <- ic1.df[ic1.df$LAMBDA>0,]
ic2.df <- ic2.df[ic2.df$LAMBDA>0,]
ic3.df <- ic3.df[ic3.df$LAMBDA>0,]
ic4.df <- ic4.df[ic4.df$LAMBDA>0,]
mc1.df <- mc1.df[mc1.df$LAMBDA>0,]
mc2.df <- mc2.df[mc2.df$LAMBDA>0,]
ms.df  <-   ms.df[ms.df$LAMBDA>0,]

plot(ic1.df$LAMBDA, ic1.df$DIFF_2SIG, pch=15, cex=2, col=alpha(2,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,700))
points(ic2.df$LAMBDA, ic2.df$DIFF_2SIG, pch=16, cex=1.8, col=alpha(3,0.4))
points(ic3.df$LAMBDA, ic3.df$DIFF_2SIG, pch=17, cex=1.8, col=alpha(4,0.4))
points(ic4.df$LAMBDA, ic4.df$DIFF_2SIG, pch=18, cex=1.8, col=alpha(5,0.4))
points(mc1.df$LAMBDA, mc1.df$DIFF_2SIG, pch=15, cex=1.8, col=alpha(6,0.4))
points(mc2.df$LAMBDA, mc2.df$DIFF_2SIG, pch=16, cex=1.8, col=alpha(7,0.4))
points(ms.df$LAMBDA,  ms.df$DIFF_2SIG,  pch=19, cex=1.8, col=alpha(1,0.4))

plot(ic1.df$LAMBDA, ic1.df$DIFF_2SIG, pch=1, cex=2, col=alpha(as.numeric(ic1.df$GROUP),0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,300), ylim=c(0,700))
points(ic2.df$LAMBDA, ic2.df$DIFF_2SIG, pch=2, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4))
points(ic3.df$LAMBDA, ic3.df$DIFF_2SIG, pch=3, cex=1.8, col=alpha(as.numeric(ic3.df$GROUP),0.4))
points(ic4.df$LAMBDA, ic4.df$DIFF_2SIG, pch=4, cex=1.8, col=alpha(as.numeric(ic4.df$GROUP),0.4))
points(mc1.df$LAMBDA, mc1.df$DIFF_2SIG, pch=5, cex=1.8, col=alpha(as.numeric(mc1.df$GROUP),0.4))
points(mc2.df$LAMBDA, mc2.df$DIFF_2SIG, pch=6, cex=1.8, col=alpha(as.numeric(mc2.df$GROUP),0.4))
points(ms.df$LAMBDA,  ms.df$DIFF_2SIG,  pch=7, cex=1.8, col=alpha(as.numeric(ms.df$GROUP),0.4))
legend("topright", c("IC1", "IC2", "IC3", "IC4", "MC1", "MC2", "MS"), pch=c(1,2,3,4,5,6,7), bty="n")
legend("topleft", c("COLORADO PLATEAU SITES", "GREAT SALT LAKE SITES", "SEVIER-PAROWAN SITES", "SOUTHERN-ANASAZI SITES", "UINTA BASIN SITES"), pch=c(20,20,20,20,20,20,20), bty="n", col=alpha(c(1,2,3,4,5), c(0.4,0.4,0.4,0.4,0.4)))

par(mfrow=c(2,2), mar=c(4,4,2,2))
#Frame 1
plot(ic1.df$LAMBDA, ic1.df$DIFF_2SIG, pch=15, cex=2, col=alpha(2,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 2 sigma", xlim=c(0,600), ylim=c(0,700))
points(ic2.df$LAMBDA, ic2.df$DIFF_2SIG, pch=16, cex=1.8, col=alpha(3,0.4))

# ic1.fit <- data.frame(LAMBDA=ic1.df$LAMBDA, DIFF_2SIG=ic1.df$DIFF_2SIG)
# ic1.ord <- ic1.fit[order(ic1.fit$LAMBDA),]
# ic1.lo <- lm(ic1.ord$DIFF_2SIG ~ ic1.ord$LAMBDA)
# abline(ic1.lo, col=2, lwd=2, lty=2)
# 
# ic2.fit <- data.frame(LAMBDA=ic2.df$LAMBDA, DIFF_2SIG=ic2.df$DIFF_2SIG)
# ic2.ord <- ic2.fit[order(ic2.fit$LAMBDA),]
# ic2.lo <- lm(ic2.ord$DIFF_2SIG ~ ic2.ord$LAMBDA)
# abline(ic2.lo, col=3, lwd=2, lty=2)

legend("topright", c("IC1", "IC2"), pch=c(15,16), col=alpha(c(2,3), c(0.4,0.4)), bty="n")
legend("topleft", "IRRIGATED", bty="n")

#Frame 2
plot(ic3.df$LAMBDA, ic3.df$DIFF_2SIG, pch=17, cex=1.8, col=alpha(4,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 2 sigma", xlim=c(0,600), ylim=c(0,700))
points(ic4.df$LAMBDA, ic4.df$DIFF_2SIG, pch=18, cex=1.8, col=alpha(5,0.4))

# ic3.fit <- data.frame(LAMBDA=ic3.df$LAMBDA, DIFF_2SIG=ic3.df$DIFF_2SIG)
# ic3.ord <- ic3.fit[order(ic3.fit$LAMBDA),]
# ic3.lo <- lm(ic3.ord$DIFF_2SIG ~ ic3.ord$LAMBDA)
# abline(ic3.lo, col=4, lwd=2, lty=2)
# 
# ic4.fit <- data.frame(LAMBDA=ic4.df$LAMBDA, DIFF_2SIG=ic4.df$DIFF_2SIG)
# ic4.ord <- ic4.fit[order(ic4.fit$LAMBDA),]
# ic4.lo <- lm(ic4.ord$DIFF_2SIG ~ ic4.ord$LAMBDA)
# abline(ic4.lo, col=5, lwd=2, lty=2)

legend("topright", c("IC3", "IC4"), pch=c(17,18), col=alpha(c(4,5), c(0.4,0.4)), bty="n")
legend("topleft", "IRRIGATED", bty="n")

#Frame 3
plot(mc1.df$LAMBDA, mc1.df$DIFF_2SIG, pch=15, cex=1.8, col=alpha(6,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 2 sigma", xlim=c(0,600), ylim=c(0,700))
points(mc2.df$LAMBDA, mc2.df$DIFF_2SIG, pch=16, cex=1.8, col=alpha(7,0.4))

# mc1.fit <- data.frame(LAMBDA=mc1.df$LAMBDA, DIFF_2SIG=mc1.df$DIFF_2SIG)
# mc1.ord <- mc1.fit[order(mc1.fit$LAMBDA),]
# mc1.lo <- lm(mc1.ord$DIFF_2SIG ~ mc1.ord$LAMBDA)
# abline(mc1.lo, col=6, lwd=2, lty=2)
# 
# mc2.fit <- data.frame(LAMBDA=mc2.df$LAMBDA, DIFF_2SIG=mc2.df$DIFF_2SIG)
# mc2.ord <- mc2.fit[order(mc2.fit$LAMBDA),]
# mc2.lo <- lm(mc2.ord$DIFF_2SIG ~ mc2.ord$LAMBDA)
# abline(mc2.lo, col=7, lwd=2, lty=2)

legend("topright", c("MC1", "MC2"), pch=c(15,16), col=alpha(c(6,7), c(0.4,0.4)), bty="n")
legend("topleft", "IRRIGATED", bty="n")

#Frame 4
plot(ms.df$LAMBDA,  ms.df$DIFF_2SIG,  pch=19, cex=1.8, col=alpha(1,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 2 sigma", xlim=c(0,600), ylim=c(0,700))

# ms.fit <- data.frame(LAMBDA=ms.df$LAMBDA, DIFF_2SIG=ms.df$DIFF_2SIG)
# ms.ord <- ms.fit[order(ms.fit$LAMBDA),]
# ms.lo <- lm(ms.ord$DIFF_2SIG ~ ms.ord$LAMBDA)
# abline(ms.lo, col=1, lwd=2, lty=2)

legend("topright", "MS", pch=19, col=alpha(1, 0.4), bty="n")
legend("topleft", "IRRIGATED", bty="n")

write.csv(ic1.df, "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/IC1_IRR.csv")
write.csv(ic2.df, "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/IC2_IRR.csv")
write.csv(ic3.df, "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/IC3_IRR.csv")
write.csv(ic4.df, "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/IC4_IRR.csv")
write.csv(mc1.df, "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/MC1_IRR.csv")
write.csv(mc2.df, "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/MC2_IRR.csv")
write.csv(ms.df,  "/media/zizroc/Extra Drive 1/IIASA/Data for Tamas/MS_IRR.csv")


ic1.kmeans <- kmeans(data.frame(ic1.df$LAMBDA, ic1.df$DIFF_2SIG), 6)
ic2.kmeans <- kmeans(data.frame(ic2.df$LAMBDA, ic2.df$DIFF_2SIG), 3)
ic3.kmeans <- kmeans(data.frame(ic3.df$LAMBDA, ic3.df$DIFF_2SIG), 4)
ic4.kmeans <- kmeans(data.frame(ic4.df$LAMBDA, ic4.df$DIFF_2SIG), 6)
mc1.kmeans <- kmeans(data.frame(mc1.df$LAMBDA, mc1.df$DIFF_2SIG), 3)
mc2.kmeans <- kmeans(data.frame(mc2.df$LAMBDA, mc2.df$DIFF_2SIG), 3)
ms.kmeans  <- kmeans(data.frame(ms.df$LAMBDA,  ms.df$DIFF_2SIG),  3)

plot(ic1.df$LON, ic1.df$LAT, pch=20, col=alpha(ic1.kmeans$cluster, 0.5), cex=3)
plot(ic2.df$LON, ic2.df$LAT, pch=20, col=alpha(ic2.kmeans$cluster, 0.5), cex=3)
plot(ic3.df$LON, ic3.df$LAT, pch=20, col=alpha(ic3.kmeans$cluster, 0.5), cex=3)
plot(ic4.df$LON, ic4.df$LAT, pch=20, col=alpha(ic4.kmeans$cluster, 0.5), cex=3)


#Colour by location
par(mfrow=c(2,2), mar=c(4,4,2,2))
#Frame 1
plot(ic1.df$LAMBDA, ic1.df$DIFF_2SIG, pch=15, cex=2, col=alpha(as.numeric(ic2.df$GROUP),0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,700))
points(ic2.df$LAMBDA, ic2.df$DIFF_2SIG, pch=16, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4))

# ic1.fit <- data.frame(LAMBDA=ic1.df$LAMBDA, DIFF_2SIG=ic1.df$DIFF_2SIG)
# ic1.ord <- ic1.fit[order(ic1.fit$LAMBDA),]
# ic1.lo <- lm(ic1.ord$DIFF_2SIG ~ ic1.ord$LAMBDA)
# abline(ic1.lo, col=2, lwd=2, lty=2)
# 
# ic2.fit <- data.frame(LAMBDA=ic2.df$LAMBDA, DIFF_2SIG=ic2.df$DIFF_2SIG)
# ic2.ord <- ic2.fit[order(ic2.fit$LAMBDA),]
# ic2.lo <- lm(ic2.ord$DIFF_2SIG ~ ic2.ord$LAMBDA)
# abline(ic2.lo, col=3, lwd=2, lty=2)

legend("topright", c("IC1", "IC2"), pch=c(15,16), col=alpha(c(1,1), c(0.4,0.4)), bty="n")
legend("topleft", c("GSL", "SEVIER-PAROWAN", "SOUTH", "UINTA BASIN"), pch=15, col=alpha(c(2,3,4,5), c(0.4,0.4,0.4,0.4)), bty="n")

#Frame 2
plot(ic3.df$LAMBDA, ic3.df$DIFF_2SIG, pch=17, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,700))
points(ic4.df$LAMBDA, ic4.df$DIFF_2SIG, pch=18, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4))

# ic3.fit <- data.frame(LAMBDA=ic3.df$LAMBDA, DIFF_2SIG=ic3.df$DIFF_2SIG)
# ic3.ord <- ic3.fit[order(ic3.fit$LAMBDA),]
# ic3.lo <- lm(ic3.ord$DIFF_2SIG ~ ic3.ord$LAMBDA)
# abline(ic3.lo, col=4, lwd=2, lty=2)
# 
# ic4.fit <- data.frame(LAMBDA=ic4.df$LAMBDA, DIFF_2SIG=ic4.df$DIFF_2SIG)
# ic4.ord <- ic4.fit[order(ic4.fit$LAMBDA),]
# ic4.lo <- lm(ic4.ord$DIFF_2SIG ~ ic4.ord$LAMBDA)
# abline(ic4.lo, col=5, lwd=2, lty=2)

legend("topright", c("IC3", "IC4"), pch=c(17,18), col=alpha(c(1,1), c(0.4,0.4)), bty="n")
legend("topleft", c("GSL", "SEVIER-PAROWAN", "SOUTH", "UINTA BASIN"), pch=15, col=alpha(c(2,3,4,5), c(0.4,0.4,0.4,0.4)), bty="n")

#Frame 3
plot(mc1.df$LAMBDA, mc1.df$DIFF_2SIG, pch=15, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,700))
points(mc2.df$LAMBDA, mc2.df$DIFF_2SIG, pch=16, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4))

# mc1.fit <- data.frame(LAMBDA=mc1.df$LAMBDA, DIFF_2SIG=mc1.df$DIFF_2SIG)
# mc1.ord <- mc1.fit[order(mc1.fit$LAMBDA),]
# mc1.lo <- lm(mc1.ord$DIFF_2SIG ~ mc1.ord$LAMBDA)
# abline(mc1.lo, col=6, lwd=2, lty=2)
# 
# mc2.fit <- data.frame(LAMBDA=mc2.df$LAMBDA, DIFF_2SIG=mc2.df$DIFF_2SIG)
# mc2.ord <- mc2.fit[order(mc2.fit$LAMBDA),]
# mc2.lo <- lm(mc2.ord$DIFF_2SIG ~ mc2.ord$LAMBDA)
# abline(mc2.lo, col=7, lwd=2, lty=2)

legend("topright", c("MC1", "MC2"), pch=c(15,16), col=alpha(c(1,1), c(0.4,0.4)), bty="n")
legend("topleft", c("GSL", "SEVIER-PAROWAN", "SOUTH", "UINTA BASIN"), pch=15, col=alpha(c(2,3,4,5), c(0.4,0.4,0.4,0.4)), bty="n")

#Frame 4
plot(ms.df$LAMBDA,  ms.df$DIFF_2SIG,  pch=19, cex=1.8, col=alpha(as.numeric(ic2.df$GROUP),0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,700))

# ms.fit <- data.frame(LAMBDA=ms.df$LAMBDA, DIFF_2SIG=ms.df$DIFF_2SIG)
# ms.ord <- ms.fit[order(ms.fit$LAMBDA),]
# ms.lo <- lm(ms.ord$DIFF_2SIG ~ ms.ord$LAMBDA)
# abline(ms.lo, col=1, lwd=2, lty=2)

legend("topright", "MS", pch=19, col=alpha(1, 0.4), bty="n")
legend("topleft", c("GSL", "SEVIER-PAROWAN", "SOUTH", "UINTA BASIN"), pch=15, col=alpha(c(2,3,4,5), c(0.4,0.4,0.4,0.4)), bty="n")







#Draw lines of best fit through points
ic1.fit <- data.frame(LAMBDA=ic1.df$LAMBDA, DIFF_2SIG=ic1.df$DIFF_2SIG)
ic1.ord <- ic1.fit[order(ic1.fit$LAMBDA),]
ic1.lo <- lm(ic1.ord$DIFF_2SIG ~ ic1.ord$LAMBDA)
abline(ic1.lo, col=2, lwd=2)

ic2.fit <- data.frame(LAMBDA=ic2.df$LAMBDA, DIFF_2SIG=ic2.df$DIFF_2SIG)
ic2.ord <- ic2.fit[order(ic2.fit$LAMBDA),]
ic2.lo <- lm(ic2.ord$DIFF_2SIG ~ ic2.ord$LAMBDA)
abline(ic2.lo, col=3, lwd=2)

ic3.fit <- data.frame(LAMBDA=ic3.df$LAMBDA, DIFF_2SIG=ic3.df$DIFF_2SIG)
ic3.ord <- ic3.fit[order(ic3.fit$LAMBDA),]
ic3.lo <- lm(ic3.ord$DIFF_2SIG ~ ic3.ord$LAMBDA)
abline(ic3.lo, col=4, lwd=2)

ic4.fit <- data.frame(LAMBDA=ic4.df$LAMBDA, DIFF_2SIG=ic4.df$DIFF_2SIG)
ic4.ord <- ic4.fit[order(ic4.fit$LAMBDA),]
ic4.lo <- lm(ic4.ord$DIFF_2SIG ~ ic4.ord$LAMBDA)
abline(ic4.lo, col=5, lwd=2)

mc1.fit <- data.frame(LAMBDA=mc1.df$LAMBDA, DIFF_2SIG=mc1.df$DIFF_2SIG)
mc1.ord <- mc1.fit[order(mc1.fit$LAMBDA),]
mc1.lo <- lm(mc1.ord$DIFF_2SIG ~ mc1.ord$LAMBDA)
abline(mc1.lo, col=6, lwd=2)

mc2.fit <- data.frame(LAMBDA=mc2.df$LAMBDA, DIFF_2SIG=mc2.df$DIFF_2SIG)
mc2.ord <- mc2.fit[order(mc2.fit$LAMBDA),]
mc2.lo <- lm(mc2.ord$DIFF_2SIG ~ mc2.ord$LAMBDA)
abline(mc2.lo, col=7, lwd=2)

ms.fit <- data.frame(LAMBDA=ms.df$LAMBDA, DIFF_2SIG=ms.df$DIFF_2SIG)
ms.ord <- ms.fit[order(ms.fit$LAMBDA),]
ms.lo <- lm(ms.ord$DIFF_2SIG ~ ms.ord$LAMBDA)
abline(ms.lo, col=1, lwd=2)

legend("topright", c("IC1", "IC2", "IC3", "IC4", "MC1", "MC2", "MS"), pch=c(15,16,17,18,15,16,19), col=alpha(c(2,3,4,5,6,7,1), c(0.4,0.4,0.4,0.4,0.4,0.4,0.4)), bty="n")
legend("topleft", "IRRIGATED", bty="n")

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(ic1.df$LAMBDA, ic1.df$DIFF_1SIG, pch=15, cex=2, col=alpha(2,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,1500))
points(ic2.df$LAMBDA, ic2.df$DIFF_1SIG, pch=16, cex=1.8, col=alpha(3,0.4))

plot(ic3.df$LAMBDA, ic3.df$DIFF_1SIG, pch=17, cex=2, col=alpha(4,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,1500))
points(ic4.df$LAMBDA, ic4.df$DIFF_1SIG, pch=18, cex=1.8, col=alpha(5,0.4))

plot(mc1.df$LAMBDA, mc1.df$DIFF_1SIG, pch=15, cex=2, col=alpha(6,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,1500))
points(mc2.df$LAMBDA, mc2.df$DIFF_1SIG, pch=16, cex=1.8, col=alpha(7,0.4))

plot(ms.df$LAMBDA, ms.df$DIFF_1SIG, pch=19, cex=2, col=alpha(1,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,600), ylim=c(0,1500))

# #Find the duration of the "decay" to half maize-productivity
# halflife.ic1 <- data.frame(INDEX=ic1$SITE, DIFF_1SIG=(ic1$HALFLIFE_YR-ic1$START_YR))
# halflife.ic2 <- data.frame(INDEX=ic2$SITE, DIFF_1SIG=(ic2$HALFLIFE_YR-ic2$START_YR))
# halflife.ic3 <- data.frame(INDEX=ic3$SITE, DIFF_1SIG=(ic3$HALFLIFE_YR-ic3$START_YR))
# halflife.ic4 <- data.frame(INDEX=ic4$SITE, DIFF_1SIG=(ic4$HALFLIFE_YR-ic4$START_YR))
# halflife.mc1 <- data.frame(INDEX=mc1$SITE, DIFF_1SIG=(mc1$HALFLIFE_YR-mc1$START_YR))
# halflife.mc2 <- data.frame(INDEX=mc2$SITE, DIFF_1SIG=(mc2$HALFLIFE_YR-mc2$START_YR))
# halflife.ms  <- data.frame(INDEX=ms$SITE,  DIFF_1SIG=(ms$HALFLIFE_YR - ms$START_YR))
# 
# #Only take sites for which there are numbers for SPD and crop productivity
# halflife.ic1 <- na.omit(halflife.ic1[halflife.ic1$INDEX %in% sites.temp$INDEX,])
# halflife.ic2 <- na.omit(halflife.ic2[halflife.ic2$INDEX %in% sites.temp$INDEX,])
# halflife.ic3 <- na.omit(halflife.ic3[halflife.ic3$INDEX %in% sites.temp$INDEX,])
# halflife.ic4 <- na.omit(halflife.ic4[halflife.ic4$INDEX %in% sites.temp$INDEX,])
# halflife.mc1 <- na.omit(halflife.mc1[halflife.mc1$INDEX %in% sites.temp$INDEX,])
# halflife.mc1 <- na.omit(halflife.mc2[halflife.mc2$INDEX %in% sites.temp$INDEX,])
# halflife.ms  <- na.omit(halflife.ms[halflife.ms$INDEX %in% sites.temp$INDEX,])
# 
# halflife.ic1 <- halflife.ic1[halflife.ic1$DIFF_1SIG >= 0,]
# halflife.ic2 <- halflife.ic2[halflife.ic2$DIFF_1SIG >= 0,]
# halflife.ic3 <- halflife.ic3[halflife.ic3$DIFF_1SIG >= 0,]
# halflife.ic4 <- halflife.ic4[halflife.ic4$DIFF_1SIG >= 0,]
# halflife.mc1 <- halflife.mc1[halflife.mc1$DIFF_1SIG >= 0,]
# halflife.mc2 <- halflife.mc2[halflife.mc2$DIFF_1SIG >= 0,]
# halflife.ms  <- halflife.ms[halflife.ms$DIFF_1SIG >= 0,]
# 
# halflife.ic1.ord <- halflife.ic1[order(halflife.ic1$DIFF_1SIG),]
# halflife.ic2.ord <- halflife.ic2[order(halflife.ic2$DIFF_1SIG),]
# halflife.ic3.ord <- halflife.ic3[order(halflife.ic3$DIFF_1SIG),]
# halflife.ic4.ord <- halflife.ic4[order(halflife.ic4$DIFF_1SIG),]
# halflife.mc1.ord <- halflife.mc1[order(halflife.mc1$DIFF_1SIG),]
# halflife.mc2.ord <- halflife.mc2[order(halflife.mc2$DIFF_1SIG),]
# halflife.ms.ord  <- halflife.ms[order(halflife.ms$DIFF_1SIG),]

sites.ic1     <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.ic1.ord$INDEX,])
sites.ic1.ord <- sites.ic1[order(sites.ic1$DIFF_2SIG),]

sites.ic2     <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.ic2.ord$INDEX,])
sites.ic2.ord <- sites.ic2[order(sites.ic2$DIFF_2SIG),]

sites.ic3     <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.ic3.ord$INDEX,])
sites.ic3.ord <- sites.ic3[order(sites.ic3$DIFF_2SIG),]

sites.ic4     <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.ic4.ord$INDEX,])
sites.ic4.ord <- sites.ic4[order(sites.ic4$DIFF_2SIG),]

sites.mc1     <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.mc1.ord$INDEX,])
sites.mc1.ord <- sites.mc1[order(sites.mc1$DIFF_2SIG),]

sites.mc2     <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.mc2.ord$INDEX,])
sites.mc2.ord <- sites.mc2[order(sites.mc2$DIFF_2SIG),]

sites.ms      <- na.omit(sites.temp[sites.temp$INDEX %in% halflife.ms.ord$INDEX,])
sites.ms.ord  <- sites.ms[order(sites.ms$DIFF_2SIG),]

# cor.test(halflife.ic1.ord$INDEX, sites.ic1.ord$INDEX, method="kendall")
# cor.test(halflife.ic2.ord$INDEX, sites.ic2.ord$INDEX, method="kendall")
# cor.test(halflife.ic3.ord$INDEX, sites.ic3.ord$INDEX, method="kendall")
# cor.test(halflife.ic4.ord$INDEX, sites.ic4.ord$INDEX, method="kendall")
# cor.test(halflife.mc1.ord$INDEX, sites.mc1.ord$INDEX, method="kendall")
# cor.test(halflife.mc2.ord$INDEX, sites.mc2.ord$INDEX, method="kendall")
# cor.test(halflife.ms.ord$INDEX, sites.ms.ord$INDEX, method="kendall")

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(halflife.ic1.ord[,2], sites.ic1.ord[,2], pch=17, cex=2, col=alpha(2,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,150), ylim=c(0,600))
points(halflife.ic2.ord[,2], sites.ic2.ord[,2], pch=19, cex=1.8, col=alpha(3,0.4))
legend("topleft", c("IC1", "IC2"), pch=c(17,19), col=alpha(c(2, 3), c(0.8, 0.8)), bty="n")
legend("topright", "RAINFED", bty="n")
plot(halflife.ic3.ord[,2], sites.ic3.ord[,2], pch=17, cex=1.8, col=alpha(4,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,150), ylim=c(0,600))
points(halflife.ic4.ord[,2], sites.ic4.ord[,2], pch=19, cex=1.8, col=alpha(5,0.4))
legend("topleft", c("IC3", "IC4"), pch=c(17,19), col=alpha(c(4, 5), c(0.8, 0.8)), bty="n")
plot(halflife.mc1.ord[,2], sites.mc1.ord[,2], pch=17, cex=1.8, col=alpha(6,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,150), ylim=c(0,600))
points(halflife.mc2.ord[,2], sites.mc2.ord[,2], pch=19, cex=1.8, col=alpha(7,0.4))
legend("topleft", c("MC1", "MC2"), pch=c(17,19), col=alpha(c(6, 7), c(0.8, 0.8)), bty="n")
plot(halflife.ms.ord[,2], sites.ms.ord[,2], pch=19, cex=1.8, col=alpha(1,0.4), xlab="Time to 50% crop output for EPIC", ylab="SPD occupation duration 1 sigma", xlim=c(0,150), ylim=c(0,600))
legend("topleft", "MS", pch=20, col=alpha(1, 0.8), bty="n")



test <- unlist(corn.strat)
ymax <- max(test[test < 100])
if(ymax < 1) ymax = 1

#plotname <- paste("C:\\Users\\thomson\\Documents\\IIASA\\Dissertation\\EPIC outputs\\site", nsite, ".png", sep="") 
#png(filename=plotname)

site.spd.col <- alpha(c("purple", "blue"), 0.1)
line.cols <- alpha(c(1, 2, 3, 4, 5, 6, 7), 0.7)
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
lines(corn.strat[[2]], col=line.cols[2], lwd=2)
lines(corn.strat[[3]], col=line.cols[3], lwd=2)
legend("topright", col=line.cols[2:3], lty=c(1,1), c("IC1", "IC2"), bty="n", lwd=c(2,2))
legend("topleft", paste("Yield/ha, site", test_ir[1,1], "at", round(test_ir[1,]$LON, 2), "W", round(test_ir[1,]$LAT, 2), "N", sep=" "), bty="n", cex=0.7)

plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
lines(corn.strat[[4]], col=line.cols[4], lwd=2)
lines(corn.strat[[5]], col=line.cols[5], lwd=2)
legend("topright", col=line.cols[4:5], lty=c(1,1), c("IC3", "IC4"), bty="n", lwd=c(2,2))

plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
lines(corn.strat[[6]], col=line.cols[6], lwd=2)
lines(corn.strat[[7]], col=line.cols[7], lwd=2)
legend("topright", col=line.cols[6:7], lty=c(1,1), c("MC1", "MC2"), bty="n", lwd=c(2,2))

plot(site.spd.2sig[[nsite]][,3], (1/max(site.spd.2sig[[nsite]][,4]))*site.spd.2sig[[nsite]][,4], type="h", col=site.spd.col[1], xlab="YEAR CE", ylab="tonnes/ha", ylim=c(0, ymax), xlim=c(850, 1449))
points(site.spd.1sig[[nsite]][,3], (1/max(site.spd.1sig[[nsite]][,4]))*site.spd.1sig[[nsite]][,4], type="h", col=site.spd.col[2])
lines(corn.strat[[1]], col=line.cols[1], lwd=2)
legend("topright", lty=1, col=1, "MS", bty="n", lwd=2)
legend("topleft", fill=site.spd.col, c("2-sigma fit", "1-sigma fit"), bty="n")

# rm(test)
# #dev.off()
# }
# dev.off()

line.cols <- alpha(c(1, 2, 3, 4, 5, 2, 3), 0.7)
line.styl <- c(2, 1, 1, 1, 1, 4, 4)
par(mfrow=c(2,1), mar=c(4,4,2,2))
test <- as.numeric(unlist(corn.strat))
test <- rollapply(test[test < 100], 5, mean)
#max(as.data.table(corn.strat)[, unlist(YIELD_AV), by = YIELD_AV]$YIELD_AV)
plot(rollapply(corn.strat[[1]], 5, mean), type="l", lty=2, lwd=2, ylim=c(0, max(test)), xlim=c(850,1449), xlab="YEAR CE", ylab="tonnes/ha", col=line.cols[1])
for(i in 2:7){
	lines(rollapply(corn.strat[[i]], 5, mean), col=line.cols[i], lty=line.styl[i], lwd=2)
}
legend("topright", lty=line.styl, col=line.cols, lwd=rep(2,7), stratvec, bty="n")
legend("topleft", paste("Maize yield per hectare for site", test_ir[1,1], "at coordinates", round(test_ir[1,]$LON, 2), "W", round(test_ir[1,]$LAT, 2), "N", sep=" "), bty="n")
rm(test)

test <- as.numeric(unlist(bean.strat))
test <- rollapply(test[test < 100], 5, mean)
plot(rollapply(bean.strat[[1]], 5, mean), type="l", lty=2, lwd=2, ylim=c(0, max(test)), xlim=c(850,1449), xlab="YEAR CE", ylab="tonnes/ha", col=line.cols[1])
for(i in 2:7){
	lines(rollapply(bean.strat[[i]], 5, mean), col=line.cols[i], lty=line.styl[i], lwd=2)
}
legend("topright", lty=line.styl, col=line.cols, lwd=rep(2,7), stratvec, bty="n")
legend("topleft", paste("Bean yield per hectare for site", test_ir[1,1], "at coordinates", round(test_ir[1,]$LON, 2), "W", round(test_ir[1,]$LAT, 2), "N", sep=" "), bty="n")
rm(test)



plot(tmp0a$ITER, tmp0a$YLDG, type="l", col=alpha("black", 0.4), xlab="YEAR CE", ylab="YLDG")
lines(tmp0b$ITER, tmp0a$YLDG, col=alpha("black", 0.4))
lines(tmp1a$ITER, tmp0a$YLDG, col=alpha("black", 0.4))
lines(tmp1b$ITER, tmp0a$YLDG, col=alpha("black", 0.4))
lines(tmp2a$ITER, tmp0a$YLDG, col=alpha("black", 0.4))
lines(tmp2b$ITER, tmp0a$YLDG, col=alpha("black", 0.4))
lines(tmp3a$ITER, tmp0a$YLDG, col=alpha("black", 0.4))
lines(tmp3b$ITER, tmp0a$YLDG, col=alpha("black", 0.4))

for(i in 850:10){
	tmp0A <- tmp0a[tmp0a$ITER==i,]
	tmp0B <- tmp0b[tmp0b$ITER==i,]
	tmp1A <- tmp1a[tmp1a$ITER==i,]
	tmp1B <- tmp1b[tmp1b$ITER==i,]
	tmp2A <- tmp2a[tmp2a$ITER==i,]
	tmp2B <- tmp2b[tmp2b$ITER==i,]
	tmp3A <- tmp3a[tmp3a$ITER==i,]
	tmp3B <- tmp3b[tmp3b$ITER==i,]

	tmp0Aa <- data.frame(YR=tmp0A$YR, YLDG=tmp0A$YLDG)
	tmp0Bb <- data.frame(YR=tmp0B$YR, YLDG=tmp0B$YLDG)
	tmp1Aa <- data.frame(YR=tmp1A$YR, YLDG=tmp1A$YLDG)
	tmp1Bb <- data.frame(YR=tmp1B$YR, YLDG=tmp1B$YLDG)
	tmp2Aa <- data.frame(YR=tmp2A$YR, YLDG=tmp2A$YLDG)
	tmp2Bb <- data.frame(YR=tmp2B$YR, YLDG=tmp2B$YLDG)
	tmp3Aa <- data.frame(YR=tmp3A$YR, YLDG=tmp3A$YLDG)
	tmp3Bb <- data.frame(YR=tmp3B$YR, YLDG=tmp3B$YLDG)
	if(i==1){
		Tmp0Aa <- tmp0Aa
		Tmp0Bb <- tmp0Bb
		Tmp1Aa <- tmp1Aa
		Tmp1Bb <- tmp1Bb
		Tmp2Aa <- tmp2Aa
		Tmp2Bb <- tmp2Bb
		Tmp3Aa <- tmp3Aa
		Tmp3Bb <- tmp3Bb
	}
	if(i >1){
		Tmp0Aa <- rbind(Tmp0Aa, tmp0Aa)
		Tmp0Bb <- rbind(Tmp0Bb, tmp0Bb)
		Tmp1Aa <- rbind(Tmp1Aa, tmp1Aa)
		Tmp1Bb <- rbind(Tmp1Bb, tmp1Bb)
		Tmp2Aa <- rbind(Tmp2Aa, tmp2Aa)
		Tmp2Bb <- rbind(Tmp2Bb, tmp2Bb)
		Tmp3Aa <- rbind(Tmp3Aa, tmp3Aa)
		Tmp3Bb <- rbind(Tmp3Bb, tmp3Bb)
	}
}

yield.0Aa <- aggregate(Tmp0Aa$YLDG, by=list(Tmp0Aa$YR), FUN=mean)
yield.0Bb <- aggregate(Tmp0Bb$YLDG, by=list(Tmp0Bb$YR), FUN=mean)
yield.1Aa <- aggregate(Tmp1Aa$YLDG, by=list(Tmp1Aa$YR), FUN=mean)
yield.1Bb <- aggregate(Tmp1Bb$YLDG, by=list(Tmp1Bb$YR), FUN=mean)
yield.2Aa <- aggregate(Tmp2Aa$YLDG, by=list(Tmp2Aa$YR), FUN=mean)
yield.2Bb <- aggregate(Tmp2Bb$YLDG, by=list(Tmp2Bb$YR), FUN=mean)
yield.3Aa <- aggregate(Tmp3Aa$YLDG, by=list(Tmp3Aa$YR), FUN=mean)
yield.3Bb <- aggregate(Tmp3Bb$YLDG, by=list(Tmp3Bb$YR), FUN=mean)
names(yield.0Aa) <- c("YEAR_CE", "YIELD")
names(yield.0Bb) <- c("YEAR_CE", "YIELD")
names(yield.1Aa) <- c("YEAR_CE", "YIELD")
names(yield.1Bb) <- c("YEAR_CE", "YIELD")
names(yield.2Aa) <- c("YEAR_CE", "YIELD")
names(yield.2Bb) <- c("YEAR_CE", "YIELD")
names(yield.3Aa) <- c("YEAR_CE", "YIELD")
names(yield.3Bb) <- c("YEAR_CE", "YIELD")

library(zoo)
library(scales)
plot(rollapply(yield.0Aa, 11, mean), type="l", lwd=2, lty=2, ylim=c(0, 7), col=alpha(4, 0.5))
lines(rollapply(yield.0Bb, 11, mean), lwd=2, lty=2, col=alpha(4, 0.5))
lines(rollapply(yield.1Aa, 11, mean), lwd=2, lty=3, col=alpha(4, 0.5))
lines(rollapply(yield.1Bb, 11, mean), lwd=2, lty=3, col=alpha(4, 0.5))
lines(rollapply(yield.2Aa, 11, mean), lwd=2, lty=4, col=alpha(4, 0.5))
lines(rollapply(yield.2Bb, 11, mean), lwd=2, lty=4, col=alpha(4, 0.5))
lines(rollapply(yield.2Aa, 11, mean), lwd=2, lty=5, col=alpha(4, 0.5))
lines(rollapply(yield.2Bb, 11, mean), lwd=2, lty=5, col=alpha(4, 0.5))


test_rf <- read.table("I:\\thomson\\FromJuraj\\Test_RF.txt", sep=";", header=T) #RAIN-FED

#Isolate by crop type
test_rf.corn <- test_rf[test_rf$CROP=="CORN",]
test_rf.bean <- test_rf[test_rf$CROP=="DRYB",]

#Isolate by crop spacing
test_rf.corn.narr <- test_rf.corn[test_rf.corn$SPACING==0.75,]
test_rf.corn.wide <- test_rf.corn[test_rf.corn$SPACING==1.5,]
test_rf.bean.narr <- test_rf.bean[test_rf.bean$SPACING==0.75,]
test_rf.bean.wide <- test_rf.bean[test_rf.bean$SPACING==1.5,]

test_rf.corn.narr <- test_rf.corn[test_rf.corn$SPACING==0.75,]
test_rf.corn.wide <- test_rf.corn[test_rf.corn$SPACING==1.5,]
test_rf.bean.narr <- test_rf.bean[test_rf.bean$SPACING==0.75,]
test_rf.bean.wide <- test_rf.bean[test_rf.bean$SPACING==1.5,]

#Isolate by GDD high/low
test_rf.corn.narr.hgdd <- test_rf.corn.narr[test_rf.corn.narr$GDD=="high",]
test_rf.corn.wide.hgdd <- test_rf.corn.wide[test_rf.corn.wide$GDD=="high",]
test_rf.corn.narr.lgdd <- test_rf.corn.narr[test_rf.corn.narr$GDD=="low",]
test_rf.corn.wide.lgdd <- test_rf.corn.wide[test_rf.corn.wide$GDD=="low",]

test_rf.bean.narr.hgdd <- test_rf.bean.narr[test_rf.bean.narr$GDD=="high",]
test_rf.bean.wide.hgdd <- test_rf.bean.wide[test_rf.bean.wide$GDD=="high",]
test_rf.bean.narr.lgdd <- test_rf.bean.narr[test_rf.bean.narr$GDD=="low",]
test_rf.bean.wide.lgdd <- test_rf.bean.wide[test_rf.bean.wide$GDD=="low",]


#Set conditions
tmp0a <- test_rf.corn.narr.hgdd[test_rf.corn.narr.hgdd$SUIT==stratvec[k] & test_rf.corn.narr.hgdd$NUT==1,]
tmp0b <- test_rf.corn.narr.hgdd[test_rf.corn.narr.hgdd$SUIT==stratvec[k] & test_rf.corn.narr.hgdd$NUT==2,]

tmp1a <- test_rf.corn.narr.lgdd[test_rf.corn.narr.lgdd$SUIT==stratvec[k] & test_rf.corn.narr.lgdd$NUT==1,]
tmp1b <- test_rf.corn.narr.lgdd[test_rf.corn.narr.lgdd$SUIT==stratvec[k] & test_rf.corn.narr.lgdd$NUT==2,]

tmp2a <- test_rf.corn.wide.hgdd[test_rf.corn.wide.hgdd$SUIT==stratvec[k] & test_rf.corn.wide.hgdd$NUT==1,]
tmp2b <- test_rf.corn.wide.hgdd[test_rf.corn.wide.hgdd$SUIT==stratvec[k] & test_rf.corn.wide.hgdd$NUT==2,]

tmp3a <- test_rf.corn.wide.lgdd[test_rf.corn.wide.lgdd$SUIT==stratvec[k] & test_rf.corn.wide.lgdd$NUT==1,]
tmp3b <- test_rf.corn.wide.lgdd[test_rf.corn.wide.lgdd$SUIT==stratvec[k] & test_rf.corn.wide.lgdd$NUT==2,]

for(i in 1:10){
	tmp0A <- tmp0a[tmp0a$ITER==i,]
	tmp0B <- tmp0b[tmp0b$ITER==i,]
	tmp1A <- tmp1a[tmp1a$ITER==i,]
	tmp1B <- tmp1b[tmp1b$ITER==i,]
	tmp2A <- tmp2a[tmp2a$ITER==i,]
	tmp2B <- tmp2b[tmp2b$ITER==i,]
	tmp3A <- tmp3a[tmp3a$ITER==i,]
	tmp3B <- tmp3b[tmp3b$ITER==i,]

	tmp0Aa <- data.frame(YR=tmp0A$YR, YLDG=tmp0A$YLDG)
	tmp0Bb <- data.frame(YR=tmp0B$YR, YLDG=tmp0B$YLDG)
	tmp1Aa <- data.frame(YR=tmp1A$YR, YLDG=tmp1A$YLDG)
	tmp1Bb <- data.frame(YR=tmp1B$YR, YLDG=tmp1B$YLDG)
	tmp2Aa <- data.frame(YR=tmp2A$YR, YLDG=tmp2A$YLDG)
	tmp2Bb <- data.frame(YR=tmp2B$YR, YLDG=tmp2B$YLDG)
	tmp3Aa <- data.frame(YR=tmp3A$YR, YLDG=tmp3A$YLDG)
	tmp3Bb <- data.frame(YR=tmp3B$YR, YLDG=tmp3B$YLDG)
	if(i==1){
		Tmp0Aa <- tmp0Aa
		Tmp0Bb <- tmp0Bb
		Tmp1Aa <- tmp1Aa
		Tmp1Bb <- tmp1Bb
		Tmp2Aa <- tmp2Aa
		Tmp2Bb <- tmp2Bb
		Tmp3Aa <- tmp3Aa
		Tmp3Bb <- tmp3Bb
	}
	if(i >1){
		Tmp0Aa <- rbind(Tmp0Aa, tmp0Aa)
		Tmp0Bb <- rbind(Tmp0Bb, tmp0Bb)
		Tmp1Aa <- rbind(Tmp1Aa, tmp1Aa)
		Tmp1Bb <- rbind(Tmp1Bb, tmp1Bb)
		Tmp2Aa <- rbind(Tmp2Aa, tmp2Aa)
		Tmp2Bb <- rbind(Tmp2Bb, tmp2Bb)
		Tmp3Aa <- rbind(Tmp3Aa, tmp3Aa)
		Tmp3Bb <- rbind(Tmp3Bb, tmp3Bb)
	}
}

yield.0Aa_rf <- aggregate(Tmp0Aa$YLDG, by=list(Tmp0Aa$YR), FUN=mean)
yield.0Bb_rf <- aggregate(Tmp0Bb$YLDG, by=list(Tmp0Bb$YR), FUN=mean)
yield.1Aa_rf <- aggregate(Tmp1Aa$YLDG, by=list(Tmp1Aa$YR), FUN=mean)
yield.1Bb_rf <- aggregate(Tmp1Bb$YLDG, by=list(Tmp1Bb$YR), FUN=mean)
yield.2Aa_rf <- aggregate(Tmp2Aa$YLDG, by=list(Tmp2Aa$YR), FUN=mean)
yield.2Bb_rf <- aggregate(Tmp2Bb$YLDG, by=list(Tmp2Bb$YR), FUN=mean)
yield.3Aa_rf <- aggregate(Tmp3Aa$YLDG, by=list(Tmp3Aa$YR), FUN=mean)
yield.3Bb_rf <- aggregate(Tmp3Bb$YLDG, by=list(Tmp3Bb$YR), FUN=mean)
names(yield.0Aa_rf) <- c("YEAR_CE", "YIELD")
names(yield.0Bb_rf) <- c("YEAR_CE", "YIELD")
names(yield.1Aa_rf) <- c("YEAR_CE", "YIELD")
names(yield.1Bb_rf) <- c("YEAR_CE", "YIELD")
names(yield.2Aa_rf) <- c("YEAR_CE", "YIELD")
names(yield.2Bb_rf) <- c("YEAR_CE", "YIELD")
names(yield.3Aa_rf) <- c("YEAR_CE", "YIELD")
names(yield.3Bb_rf) <- c("YEAR_CE", "YIELD")

library(zoo)
library(scales)
plot(rollapply(yield.0Aa, 11, mean), type="l", lwd=2, lty=2, ylim=c(0, 7), col=alpha(4, 0.5), main="IC1 strategy")
lines(rollapply(yield.0Bb, 11, mean), lwd=2, lty=2, col=alpha(4, 0.5))
lines(rollapply(yield.1Aa, 11, mean), lwd=2, lty=3, col=alpha(4, 0.5))
lines(rollapply(yield.1Bb, 11, mean), lwd=2, lty=3, col=alpha(4, 0.5))
lines(rollapply(yield.2Aa, 11, mean), lwd=2, lty=4, col=alpha(4, 0.5))
lines(rollapply(yield.2Bb, 11, mean), lwd=2, lty=4, col=alpha(4, 0.5))
lines(rollapply(yield.2Aa, 11, mean), lwd=2, lty=5, col=alpha(4, 0.5))
lines(rollapply(yield.2Bb, 11, mean), lwd=2, lty=5, col=alpha(4, 0.5))

lines(rollapply(yield.0Aa_rf, 11, mean), lwd=2, lty=2, col=alpha(2, 0.5))
lines(rollapply(yield.0Bb_rf, 11, mean), lwd=2, lty=2, col=alpha(2, 0.5))
lines(rollapply(yield.1Aa_rf, 11, mean), lwd=2, lty=3, col=alpha(2, 0.5))
lines(rollapply(yield.1Bb_rf, 11, mean), lwd=2, lty=3, col=alpha(2, 0.5))
lines(rollapply(yield.2Aa_rf, 11, mean), lwd=2, lty=4, col=alpha(2, 0.5))
lines(rollapply(yield.2Bb_rf, 11, mean), lwd=2, lty=4, col=alpha(2, 0.5))
lines(rollapply(yield.2Aa_rf, 11, mean), lwd=2, lty=5, col=alpha(2, 0.5))
lines(rollapply(yield.2Bb_rf, 11, mean), lwd=2, lty=5, col=alpha(2, 0.5))
