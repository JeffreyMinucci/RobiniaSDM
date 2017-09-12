#######
# Script to download and format new climate projections
#
# By: Jeff Minucci
# 2/24/17
#######
library(raster)

#Read original data
PA <- read.csv("Datafiles/data.national.6.5.csv") #now using reduced factors & elevation
PA$lith.pred <- factor(PA$lith.pred)
PA$Primary.rocktype <- factor(PA$Primary.rocktype)
PA$Secondary.rocktype <- factor(PA$Secondary.rocktype)
PA$usda_tex <- factor(PA$usda_tex)
PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
PA <- PA[,colnames(PA)!="PlotID"]
PA <- PA[,c(6,9,1:5,7,8,10:length(PA))] #reorder to put responses first
PA <- PA[,-2]



#Vector of all climate projection names for rcp 8.5
projs <- c("AC","BC","CC","CN","GF","GS","HD","HG","HE",
           "IN","IP","MI","MR","MC","MP","MG","NO")
# Note GF not working

#download all projections
#for(i in 1:length(projs)){
for(i in 1:4){
  
  bioclim.proj <- raster::getData('CMIP5',var='bio',res=.5,lon=-80,lat=35.5,rcp=85,year=50,model=projs[i])
  names(bioclim.proj) <- c(paste("bio",1:19,sep=""))
  saveRDS(bioclim.proj,paste("Objects/GCM_projections_raw/bioclim_",projs[i],".rds",sep=""))
  
  #extract climate variables from bioclim stacks (and remove current bioclim data)
  PA.reduced<- PA[,-c(9:27)]
  bioclim.pred <- raster::extract(bioclim.proj,PA.reduced[,c(2,3)])
  PA.out <- cbind(PA.reduced,bioclim.pred)
  PA.out <- PA.out[,c(1:8,17:35,9:16)] #reorder to same column order as original data
  saveRDS(PA.out,paste("Objects/GCM_proj_data/PA_climate_proj2_",projs[i],".rds",sep=""))
  
}


#Match plots to projections
#for(i in 6:length(projs)){
#  bioclim.proj <- readRDS(paste("OldObjects/bioclim_",projs[i],".rds",sep=""))
#  PA.reduced<- PA[,-c(9:27)]
# bioclim.pred <- raster::extract(bioclim.proj,PA.reduced[,c(2,3)])
#  PA.out <- cbind(PA.reduced,bioclim.pred)
#  PA.out <- PA.out[,c(1:8,17:35,9:16)] #reorder to same column order as original data
#  saveRDS(PA.out,paste("Objects/GCM_proj_data/PA_climate_proj2_",projs[i],".rds",sep=""))
#}

