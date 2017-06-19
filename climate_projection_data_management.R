#######
# Script to download and format new climate projections
#
# By: Jeff Minucci
# 2/24/17
#######

#Read original data
PA <- read.csv("Datafiles/PA.national.3.5.csv") #now using reduced factors
PA$lith.pred <- factor(PA$lith.pred)
PA$Primary.rocktype <- factor(PA$Primary.rocktype)
PA$Secondary.rocktype <- factor(PA$Secondary.rocktype)
PA$usda_tex <- factor(PA$usda_tex)
PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
PA <- PA[,c(-1,-5)] #removing rops count and CN


#Vector of all climate projection names for rcp 8.5
projs <- c("AC","BC","CC","CN","GF","GS","HD","HG","HE",
           "IN","IP","MI","MR","MC","MP","MG","NO")
# Note GF not working

#download all projections
for(i in 6:length(projs)){
  
  bioclim.proj <- getData('CMIP5',var='bio',res=.5,lon=-80,lat=35.5,rcp=85,year=50,model=projs[i])
  names(bioclim.proj) <- c(paste("bio",1:19,sep=""))
  saveRDS(bioclim.proj,paste("Objects/bioclim_",projs[i],".rds",sep=""))
  
  #extract climate variables from bioclim stacks (and remove current bioclim data)
  PA.reduced<- PA[,-c(9:27)]
  bioclim.pred <- raster::extract(bioclim.proj,PA.reduced[,c(1,2)])
  PA.out <- cbind(PA.reduced,bioclim.pred)
  PA.out <- PA.out[,c(1:8,17:35,9:16)] #reorder to same column order as original data
  saveRDS(PA.out,paste("Objects/PA_climate_proj_",projs[i],".rds",sep=""))
  
}

