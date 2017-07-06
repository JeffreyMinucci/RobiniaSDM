#####
# Code to take best gbm model and make predictions for future climate
# Using several general circumation models (GCMs) and model averaging
####



library(dismo)
library(maptools)
library(raster)
library(maps)
library(tmap)
library(ggmap)
library(rgdal)
library(caret)
library(SDMTools)
library(gstat)
library(doParallel)



### Load the best model
#sdm1 <- readRDS("Objects/gbmTune_3_7_17v2.rds")
sdm1 <- readRDS("Objects/PA_GBM_Models/randBest_6_26_17.rds")
#hurdle1 <- readRDSW

#Read data
PA2 <- read.csv("Datafiles/PA.national.6.5.csv") #now using reduced factors
PA2$lith.pred <- factor(PA$lith.pred)
PA2$Primary.rocktype <- factor(PA$Primary.rocktype)
PA2$Secondary.rocktype <- factor(PA$Secondary.rocktype)
PA2$usda_tex <- factor(PA$usda_tex)
PA2$pres <- factor(PA$pres,levels=c("Present",'Absent'))
PA2 <- PA2[,colnames(PA)!="PlotID"]
PA2 <- PA2[,c(6,9,1:5,7,8,10:length(PA2))] #reorder to put responses first


#make predictions based on GCMs
gcms <- c("AC","BC","CC","CN","GS","HD","HG","HE",
           "IN","IP","MI","MR","MC","MP","MG","NO") #list all GCMs
# Note GF not working

results_list <- vector("list",length(gcms))
raster_list <- vector("list",length(gcms))
d_raster_list <- vector("list",length(gcms))
for (i in 1:length(gcms)){
  data <- readRDS(paste("Objects/GCM_proj_data/PA_climate_proj_",gcms[i],".rds",sep="")) #load climate proj. data
  predictions <- predict(sdm1,newdata=data[,-c(6)],type="prob",na.action=na.pass) #predict on projection
  data <- cbind(data,predictions)
  #data$pres.abs.p <- ifelse(data$Present>thresh,"Present","Absent")
  results_list[[i]] <- data #save results
  names(results_list)[i] <- gcms[i]
  
  #create and save raster of results as probability
  prob.spatial <- data[,c("LON","LAT","Present")]
  names(prob.spatial) <- c("x","y","z")
  e <- extent(prob.spatial[,1:2])
  r <- raster(e,ncol=130,100)
  x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
  raster_list[[i]] <- x
  names(raster_list)[i] <- gcms[i]
  
  #create and save raster of results as change from current
  current_raster <- readRDS("RasterOutput/current.prob_unique.rds")
  delta.raster <- x - current_raster
  d_raster_list[[i]] <- delta.raster
  names(d_raster_list)[i]<- gcms[i]
}
  
#saveRDS(results_list,"Objects/gcm_predictions_unique.rds")
#saveRDS(raster_list,"RasterOutput/gcm_predict_rasters.rds")
#saveRDS(d_raster_list,"RasterOutput/gcm_predict_rasters_delta.rds")

#convert rasters to rasterstacks
raster_stack <- stack(raster_list)
d_raster_stack <- stack(d_raster_list)
#saveRDS(raster_stack,"RasterOutput/gcm_predict_rasters_unique.rds")
#saveRDS(d_raster_stack,"RasterOutput/gcm_predict_rasters_delta_unique.rds") #for change (delta) from present

########
## 


#Explore results
#X11(w=10,h=10)
par(mfrow=c(4,2))
for(i in 1:8){
  plot(raster_list[[i]])
  plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
}


raster_stack <- readRDS("RasterOutput/gcm_predict_rasters_unique.rds")


#Presence/absence probability results (averaged for all GCMs)
gcm_predict_avg <- overlay(raster_stack,fun=mean)
plot(gcm_predict_avg,interpolate=T)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


#plot Pres/abs using optimum threshold (averaged for all GCMs)
threshold <- .37 #from training data and ROC 
breaks <- c(0,threshold,1)
colors <- c("wheat1","darkgreen")
plot(gcm_predict_avg,breaks=breaks,col=colors)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


#plot CURRENT Pres/abs using threshold
prob.spatial <- PA.predict[,c("LON","LAT","Present")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
currentPreds <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
threshold <- .37 #from training data and ROC 
breaks <- c(0,threshold,1)
colors <- c("wheat1","darkgreen")
plot(currentPreds,breaks=breaks,col=colors)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


#plot change from current probability (averaged for all GCMs)
d_raster_stack <- readRDS("RasterOutput/gcm_predict_rasters_delta_unique.rds")
gcm_d_predict_avg <- overlay(d_raster_stack,fun=mean)
plot(gcm_d_predict_avg,interpolate=T)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


### TODO Get averaged predictors for gcms
###   so that we can explore future climate 


#In averaged projections, where is bio1 over 130 (inflection point for pres. prob.)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE)
points(LAT~LON,data=subset(PA,bio1>130),pch=3,cex=.1)