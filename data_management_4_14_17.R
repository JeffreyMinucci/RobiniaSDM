#####
# Scripts for spatial data management
#
# Goal: output a csv file that can be read by other scripts for analysis
#####


library(dismo)
library(maptools)
library(raster)
library(maps)
library(tmap)
library(ggmap)
library(rgdal)
library(caret)
library(SDMTools)
library(dplyr)


#read data
data <- read.csv("RawQueries/rops_stems_BA_4_14_17.csv")
data <- subset(data,LON<-60&LAT>24.5) # only continental US
data$pres <- factor(ifelse( data$ROPS_COUNT > 0, "Present", "Absent"),levels=c("Absent","Present"))
#PA$DSTRBCD1 <- as.factor(PA$DSTRBCD1)
data$fire <- factor(ifelse(data$DSTRBCD1 %in% c(30,31,32),"Yes","No"))
data$DSTRBCD1 <- NULL #Take out disturbance code for now
data$ASPECT[data$ASPECT==0] <- NA #remove aspect 0 which means no aspect because slope is low
data$asp_val <- (cos((data$ASPECT-30)/180*pi)+1)/2 #convert aspect into aspect value index - represents 'coolness'
# See  Roberts and Cooper (1989)
data$ASPECT <- NULL
data$LIVE_CANOPY_CVR_PCT <- NULL
data$BA[is.na(data$BA)]<- 0


## Code to remove duplicate plots and keep newest records
data <- data[data$INVYR!=9999,]#remove INVYR ==9999 (code for certain phase 3 measurements)
data$PlotID <- paste(data$STATECD,data$COUNTYCD,data$PLOT,sep="_")
data <- data[order(data$INVYR,decreasing=T),]
data <- data[!duplicated(data$PlotID),]
data <- data[,-c(8,9,10,11)]

## Add plot level data (stem count and basal area for all species)
data_plot_raw <- read.csv("RawQueries/all_spp.csv")
data_plot <- aggregate(cbind(STEMS,BA) ~ CN,data=data_plot_raw,FUN=sum)
colnames(data_plot) <- c("CN","ALL_STEMS","ALL_BA")
data <- left_join(data,data_plot,by="CN")

#Calculate importance value = relative dominance + relative density
data$IV <-50*(data$ROPS_COUNT/data$ALL_STEMS) + 50*(data$BA/data$ALL_BA)

#remove columns we don't need anymore
toDrop <- c("CN","BA","ROPS_COUNT","ALL_STEMS","ALL_BA")
data <- data[,!colnames(data)%in%toDrop]



#get bioclim data and graph
bioclim <- getData('worldclim', var='bio', res=.5,lon=-80,lat=35.5)
bioclim2 <- getData('worldclim',var='bio',res=.5,lon=-100,lat=35.5)
bioclim4 <- getData('worldclim',var='bio',res=.5,lon=-82,lat=25.5)
bioclim3 <- getData('worldclim',var='bio',res=.5,lon=-95,lat=25.5)
bioclim <- mosaic(bioclim,bioclim2,bioclim3,bioclim4,fun=mean)
names(bioclim) <- c(paste("bio",1:19,sep=""))
saveRDS(bioclim,"Objects/bioclim.rds")
#plot(bioclim$bio5,xlim=c(-100,-60),ylim=(c(25,50)),axes=TRUE)
#plot(usa,add=TRUE)

#extract climate variables from bioclim stack
bioclim <- readRDS("Objects/bioclim.rds")
bioclim.pred <- raster::extract(bioclim,data[,c(1,2)])
data <- cbind(data,bioclim.pred)

#Lithology data
lithology <- raster("Layers/us_lithology_1km_dd83.img")
lith.pred <- raster::extract(lithology,data[,c(1,2)])
data <- cbind(data,lith.pred)
data$lith.pred <- factor(data$lith.pred)


#Make raster from MODIS max green vegetation fraction
mgvf <- raster("Layers/Average.tif")
mgvf.pred <- raster::extract(mgvf,data[c(1,2)])
data <- cbind(data,mgvf.pred)





#NRCS geology data
geol1 <- raster("Layers/test3") # primary rocktype
geol2 <- raster("Layers/tocktype1") #secondary rocktype
geology <- stack(geol1,geol2)
plot(geology,add=T)
geology.pred <- raster::extract(geology,data[,c(1,2)])
colnames(geology.pred) <- c("Primary rocktype", "Secondary rocktype")
geology.pred[,1] <- factor(geology.pred[,1]) #convert to factors
geology.pred[,2] <- factor(geology.pred[,2])
data <- cbind(data,geology.pred)


# Soils data - HWSD
hwsd.ph <- raster("Layers/ph.copy.tif")
hwsd.cec <- raster("Layers/cec.copy.tif")
hwsd.usda <- raster("Layers/usda_tex.copy.tif")
hwsd.oc <- raster("Layers/oc.copy.tif")
hwsd <- stack(hwsd.cec,hwsd.ph,hwsd.usda,hwsd.oc)
soils.pred <- raster::extract(hwsd,data[,c(1,2)])
colnames(soils.pred) <- c("cec","ph","usda_tex","OC")
data <- cbind(data,soils.pred)
data$usda_tex <- factor(data$usda_tex)


### Export FULL datafile for analysis
write.csv(data,"Datafiles/data.national.6.0.csv",row.names=FALSE)

#Only keep 40 most common rocktypes - reduce number of levels
data <- read.csv("Datafiles/data.national.6.0.csv")
primary <- aggregate(pres~Primary.rocktype,data=data,length)
keep1 <- primary[order(-primary$pres),"Primary.rocktype"][1:40]
secondary <- aggregate(pres~Secondary.rocktype,data=data,length)
keep2 <- secondary[order(-secondary$pres),"Secondary.rocktype"][1:40]
data.r <- data
data.r$Primary.rocktype <- factor(ifelse(data$Primary.rocktype %in% keep1, data$Primary.rocktype,999))
data.r$Secondary.rocktype <- factor(ifelse(data$Secondary.rocktype %in% keep1, data$Secondary.rocktype,999))
data.r$usda_tex <- factor(data.r$usda_tex)

### Export datafile for anlysis - with reduced factor levels 
write.csv(data.r,"Datafiles/data.national.6.5.csv",row.names=FALSE)


#Make dataframe without IV values (for initial pres/abs )
PA <- data[,colnames(data) !="IV"] 


### Export P/A data file for anlysis - with reduced factor levels 
write.csv(PA,"Datafiles/PA.national.6.5.csv",row.names=FALSE)

