####
# Code to take best P/A model and 1) extract predicted habitat, 2) map current presence absence 
###

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




#Read data and format
PA <- read.csv("Datafiles/data.national.6.5.csv") #now using reduced factors & elevation
PA$lith.pred <- factor(PA$lith.pred)
PA$Primary.rocktype <- factor(PA$Primary.rocktype)
PA$Secondary.rocktype <- factor(PA$Secondary.rocktype)
PA$usda_tex <- factor(PA$usda_tex)
PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
PA <- PA[,colnames(PA)!="PlotID"]
PA <- PA[,c(6,9,1:5,7,8,10:length(PA))] #reorder to put responses first
set.seed(1337)
intrain <- createDataPartition(y=PA$pres,p=0.8,list=FALSE)
PA.train.full <- PA[intrain,]
PA.test.full <-  PA[-intrain,]
PA.train <- PA.train.full[,c(-2)]
PA.train <- PA.train[c(2,1:nrow(PA.train)),]  ### Put an absent as the first value
PA.test <- PA.test.full[,c(-2)]
PA.train.c <- PA.train[complete.cases(PA.train),]


## save test set for use in future model if needed
saveRDS(PA.test.full,"Datafiles/Test_data_6.5.csv")


### Load the best model found so far (from PA_gbm_tuning code)
### Depth = 21, shrinkage=0.01, n.min.obsinnode = 5, n.trees=1300

sdm1 <- readRDS("Objects/gbmTune_3_7_17v2.rds")



#Find optimum threshold - base on training data
pres.abs <- ifelse(PA.train$pres == "Present",1,0)
predicted.PA.train <- predict(sdm1,newdata=PA.train[,-1],type="prob",na.action=na.pass) #predict on train
t.opt <- optim.thresh((obs=as.numeric(pres.abs)),pred=predicted.PA.train$Present)
thresh <- t.opt$`sensitivity=specificity`
predicted.PA.train.class <- ifelse(predicted.PA.train$Present> thresh,"Present","Absent")
confusionMatrix(predicted.PA.train.class,PA.train[,1])


#training ROC curve
roc.obj <- roc(pres.abs,pred=predicted.PA.train$Present)
plot.roc(roc.obj,thresholds="best",print.thres="best") 
auc(roc.obj)


#Test set performance
predicted.PA.test <- predict(sdm1,newdata=PA.test[,-1],type="prob",na.action=na.pass) #predict on test set
roc.obj <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.test$Present)
auc(roc.obj)
plot.roc(roc.obj,thresholds="best",print.thres="best") 
predicted.PA.test.class <- ifelse(predicted.PA.test$Present> thresh,"Present","Absent")
confusionMatrix(predicted.PA.test.class,PA.test[,1])


#Examine model
varImp(sdm1)
plot(sdm1$finalModel,i.var='Primary.rocktype',type="response")
rock1 <- plot(sdm1$finalModel,i.var='Primary.rocktype',type="response",return.grid=T)
rock1[order(rock1$y,decreasing=T),]
#112 = granulite, 15 = chert, 114 = tuff,  74 = felsic metavolcanic rock, 72 = biotite schist
#16 = schist, #45 = silt

plot(sdm1$finalModel,i.var='bio3',type="response") #isothermality (large = more stable temp)
rug(PA.train$bio3)
plot(sdm1$finalModel,i.var='bio15',type="response") #Precipitation seasonality
rug(PA.train$bio15)
plot(sdm1$finalModel,i.var='bio5',type="response") #max temp of warmest month
rug(PA.train$bio5)
plot(sdm1$finalModel,i.var='bio10',type="response") # Mean temp of warmest quarter
rug(PA.train$bio10)
plot(sdm1$finalModel,i.var='bio4',type="response") #temp seasonality
rug(PA.train$bio4)
plot(sdm1$finalModel,i.var='bio9',type="response") #temp of driest quarter
rug(PA.train$bio9)
plot(sdm1$finalModel,i.var='bio11',type="response") #mean temp of coldest quarter
rug(PA.train$bio11)
plot(plot(sdm1$finalModel,i.var='OC',type="response",return.grid=T),xlim=c(0,5),type='l',ylab="Predicted probability")
rug(PA.train$OC)
plot(sdm1$finalModel,i.var='lith.pred',type="response")
plot(sdm1$finalModel,i.var='LAT',type="response",continuous.resolution=100)
plot(sdm1$finalModel,i.var='LON',type="response")
plot(sdm1$finalModel,i.var='Primary.rocktype',type="response")
plot(sdm1$finalModel,i.var='STDAGE',type="response")# more rops at "hot/sunny" sites?
plot(sdm1$finalModel,i.var='ELEV',type="response")
plot(sdm1$finalModel,i.var='SLOPE',type="response", continuous.resolution=100)
plot(sdm1$finalModel,i.var='asp_val',type="response",continuous.resolution=100)
plot(sdm1$finalModel,i.var='ph',type="response",continuous.resolution=100)
#find.int <- interact.gbm(sdm1$finalModel,PA.train[,-5],i.var=c("bio5","bio15"))
#find.int <- interact.gbm(sdm1$finalModel,PA.train[,-5],i.var=c("LAT","LON"))
#gbm.interactions(sdm1$finalModel) #Not working!
#gbm.plot(sdm2,n.plots=1) # doesnt work


# Get probability predictions for ALL DATA (test+train) 
predicted.PA <- predict(sdm1,newdata=PA[,-c(1,2)],type="prob",na.action=na.pass) 
PA.predict <- cbind(PA,predicted.PA)
PA.predict$predictedClass <- ifelse(PA.predict$Present> thresh,"Present","Absent")
confusionMatrix(PA.predict$predictedClass,PA.predict[,1])


#Save plots where predicted class = "present" for regression analysis 
PresentPlots <- PA.predict[PA.predict$predictedClass == "Present",]
saveRDS(PresentPlots,"Datafiles/PresentPlots_4_14_17.rds")

#Save plots where predicted class = "absent" for later plotting / results
AbsentPlots <- PA.predict[PA.predict$predictedClass == "Absent",]
saveRDS(AbsentPlots,"Datafiles/AbsentPlots_4_14_17.rds")


#Save copy of all predictions
saveRDS(PA.predict,"Datafiles/AllClassPredicts_4_14_17.rds")


######### Make maps of presence/absence

#Plot points based on predicted pres vs abs (based on optimum thresh)
usa <- getData('GADM' , country="USA", level=1)
PA.predict$col <- ifelse(PA.predict$pres.abs.p == "Present","darkgreen","cornflowerblue")
plot(PA.predict$LON,PA.predict$LAT,col=PA.predict$col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
points(LAT~LON,data=subset(PA,pres=="Present"),pch=3,cex=.1)


#Make interpolated plot via inverse distance weighting
idw <- geoIDW(p=PA.predict[PA.predict$pres.abs.p=="Present",c(1,2)],
              a=PA.predict[PA.predict$pres.abs.p=="Absent",c(1,2)])
lithology <- reclassify(raster("Layers/us_lithology_1km_dd83.img"),cbind(0,NA))
idw.p <- predict(lithology,idw,mask=TRUE)
saveRDS(idw.p,"Objects/idw.p_8_31_16.rds")
plot(idw.p,main="Predicted Species Distribution",xlim=c(-100,-65),ylim=c(24.8,45.9))
plot(usa,add=T)
points(LAT~LON,data=subset(PA,pres=="Present"),pch=3,cex=.1)



### Plot probability - as raster
#prob.spatial <- PA.predict[,c("LON","LAT","Present")]
#names(prob.spatial) <- c("x","y","z")
#e <- extent(prob.spatial[,1:2])
#r <- raster(e,ncol=130,100)
#x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
#plot(x,interpolate=T)
#plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
#saveRDS(x,"RasterOutput/current.prob_unique.rds")
x <- readRDS("RasterOutput/current.prob_unique.rds") #loading from memory (code above)
plot(x,interpolate=T)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)




#########BELOW: old code probably not needed. See Predict_future for predictions under future climate

# Map with climate change
#Read data
PA2 <- read.csv("Datafiles/PA.national.6.5.csv") #now using reduced factors
PA2$lith.pred <- factor(PA$lith.pred)
PA2$Primary.rocktype <- factor(PA$Primary.rocktype)
PA2$Secondary.rocktype <- factor(PA$Secondary.rocktype)
PA2$usda_tex <- factor(PA$usda_tex)
PA2$pres <- factor(PA$pres,levels=c("Present",'Absent'))
PA2 <- PA2[,c(-1,-5)] #removing rops count and CN

#Update climate data to projections

#get bioclim data and graph
#bioclim <- getData('CMIP5', var='bio', res=.5,lon=-80,lat=35.5,rcp=85,year=70,model='CC')
#bioclim.low <- getData('CMIP5', var='bio', res=5,rcp=85,year=70,model='HE')
#names(bioclim) <- c(paste("bio",1:19,sep=""))
#plot(bioclim$bio5,xlim=c(-100,-60),ylim=(c(25,50)),axes=TRUE)
#bioclim <- getData('CMIP5', var='bio', res=.5,lon=-80,lat=35.5,rcp=85,year=70,model='CC')
bioclim.he <- getData('CMIP5', var='bio', res=.5,lon=-80,lat=35.5,rcp=85,year=50,model='HE')
names(bioclim.he) <- c(paste("bio",1:19,sep=""))
saveRDS(bioclim.he,"Objects/bioclim_HE.rds")
plot(bioclim.he$bio5,xlim=c(-100,-60),ylim=(c(25,50)),axes=TRUE) #plot bio5 for projection
plot(bioclim$bio5,xlim=c(-100,-60),ylim=(c(25,50)),axes=TRUE) #plot current bio5

#download an alternate projection
bioclim.cc <- getData('CMIP5',var='bio',res=.5,lon=-80,lat=35.5,rcp=85,year=50,model="CC")
names(bioclim.cc) <- c(paste("bio",1:19,sep=""))
saveRDS(bioclim.cc,"Objects/bioclim_CC.rds")
plot(bioclim.cc$bio5,xlim=c(-100,-60),ylim=c(25,50),axes=T)

#extract climate variables from bioclim stack (and remove current bioclim data)
PA2<- PA2[,-c(9:27)]
bioclim.pred <- raster::extract(bioclim.he,PA2[,c(1,2)])
PA2 <- cbind(PA2,bioclim.pred)
PA2 <- PA2[,c(1:8,17:35,9:16)] #reorder to same column order as original data
saveRDS(PA2,"Objects/PA_climate_proj_2.5.rds")

#make df with CC clmiate variables
PA.cc <- PA2[,-c(9:27)]
bioclim.pred <- raster::extract(bioclim.cc,PA.cc[,c(1,2)])
PA.cc <- cbind(PA.cc,bioclim.pred)
PA.cc <- PA.cc[,c(1:8,17:35,9:16)] #reorder to same column order as original data
saveRDS(PA.cc,"Objects/PA_climate_proj_cc_2.5.rds")


#Predict on new climate data and plot
PA2 <- readRDS("Objects/PA_climate_proj_2.5.rds")
predicted.PA2 <- predict(sdm1,newdata=PA2[,-c(6)],type="prob",na.action=na.pass) 
PA2 <- cbind(PA2,predicted.PA2)
PA2$pres.abs.p <- ifelse(PA2$Present> thresh,"Present","Absent")
#Plot points based on predicted pres vs abs (based on optimum thresh)
usa <- getData('GADM' , country="USA", level=1)
PA2$col <- ifelse(PA2$pres.abs.p == "Present","darkgreen","cornflowerblue")
plot(PA2$LON,PA2$LAT,col=PA2$col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


#predict on alternate climate projections 
PA.cc <- readRDS("Objects/PA_climate_proj_cc_2.5.rds")
predicted.PA.cc <- predict(sdm1,newdata=PA.cc[,-c(6)],type="prob",na.action=na.pass)
PA.cc <- cbind(PA.cc,predicted.PA.cc)
PA.cc$pres.abs.p <- ifelse(PA.cc$Present> thresh,"Present","Absent")


#Plot predicted prob of present - as raster cells
pred.spatial <- PA2[,c("LON","LAT","Present")]
names(pred.spatial) <- c("x","y","z")
e <- extent(pred.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(pred.spatial[,1:2],r,pred.spatial[,3],fun=mean)
plot(x)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)

#Plot predicted prob of presence - as raster - FOR alternate proj
pred.spatial <- PA.cc[,c("LON","LAT","Present")]
names(pred.spatial) <- c("x","y","z")
e <- extent(pred.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(pred.spatial[,1:2],r,pred.spatial[,3],fun=mean)
plot(x)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)



#Plot change from current - for all climate projections
PA2$delta <- ifelse(PA2$pres.abs.p == "Absent" & PA.predict$pres.abs.p == "Present","red",
                         ifelse(PA2$pres.abs.p == "Present" & PA.predict$pres.abs.p == "Absent","darkgoldenrod",PA.predict$col))
plot(PA2$LON,PA2$LAT,col=PA2$delta,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)



#Plot change from current as a delta percentage probability
PA2$delta.prob <- PA2$Present - PA.predict$Present #make new variable that is change in prob of present
PA2$delta.prob.col <- colorRampPalette(c('red','green'))(10)[as.numeric(cut(PA2$delta.prob,breaks = 10))] #create color ramp
plot(PA2$LON,PA2$LAT,col=PA2$delta.prob.col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
legend("topright",title="change in probability",legend=c(1:10),col=colorRampPalette(c('red','green'))(10),pch=20,cex=0.8)
#try converting to rasters
delta.spatial <- PA2[,c("LON","LAT","delta.prob")]
names(delta.spatial) <- c("x","y","z")
#r1 <- raster(list(x=delta.spatial$LON,y=delta.spatial$LAT,z=delta.spatial$delta.prob))
e <- extent(delta.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(delta.spatial[,1:2],r,delta.spatial[,3],fun=mean)
plot(x)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


#Predict on new climate data using gbm-package based model - and plot
PA2.test <- PA2[,-c(36:39)]
predicted.PA2.t <- predict(sdm2,newdata=PA2.test[,-c(3,6)],type="response",na.action=na.pass,n.trees=28000) 
PA2.test <- cbind(PA2.test,predicted.PA2.t)
PA2.test$pres.abs.p <- ifelse(PA2$Present> thresh,"Present","Absent")
#plot
PA2.test$col <- ifelse(PA2.test$pres.abs.p == "Present","green","blue")
plot(PA2.test$LON,PA2.test$LAT,col=PA2.test$col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


##############

#Predict with projected precip data but current temp data 
#PA2.precip <- readRDS("Objects/PA_climate_proj_2.rds") ##note column nums have changed
#PA2.precip <- PA2.precip[,-c(17:27)]
#PA2.precip <- cbind(PA2.precip,PA[,c(8:18)])
#PA2.precip<- PA2.precip[,c(1:7,25:35,17:24,8:16)] #reorder to same column order as original data
#saveRDS(PA2.precip,"Objects/PA_climate_proj_precip.rds")
PA2.precip <- readRDS("Objects/PA_climate_proj_precip.rds")
predicted.PA2.p <- predict(sdm1,newdata=PA2.precip[,-c(3,6)],type="prob",na.action=na.pass) 
PA2.precip <- cbind(PA2.precip,predicted.PA2.p)
PA2.precip$pres.abs.p <- ifelse(PA2.precip$Present> thresh,"Present","Absent")
#Plot precip-only projected range
PA2.precip$col <- ifelse(PA2.precip$pres.abs.p == "Present","darkgreen","cornflowerblue")
plot(PA2.precip$LON,PA2.precip$LAT,col=PA2.precip$col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)

#Plot differences from current model - precip only
PA2.precip$delta <- ifelse(PA2.precip$pres.abs.p == "Absent" & PA.predict$pres.abs.p == "Present","red",
                    ifelse(PA2.precip$pres.abs.p == "Present" & PA.predict$pres.abs.p == "Absent","darkgoldenrod",PA.predict$col))
plot(PA2.precip$LON,PA2.precip$LAT,col=PA2.precip$delta,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)


#Try idw for precip projections
interp <- idw(Present~1,LAT~LON,PA2.precip)



#Predict with projected temp data but current precip
#PA2.temp <- readRDS("Objects/PA_climate_proj_2.rds") ##note column nums have changed
#PA2.temp <- PA2.temp[,-c(28:35)]
#PA2.temp <- cbind(PA2.temp,PA[,c(19:26)])
#PA2.temp <- PA2.temp[,c(1:7,17:35,8:16)] #reorder to same column order as original data
#saveRDS(PA2.temp,"Objects/PA_climate_proj_temp.rds")
PA2.temp <- readRDS("Objects/PA_climate_proj_temp.rds")
predicted.PA2.temp <- predict(sdm1,newdata=PA2.temp[,-c(3,6)],type="prob",na.action=na.pass) 
PA2.temp <- cbind(PA2.temp,predicted.PA2.temp)
PA2.temp$pres.abs.p <- ifelse(PA2.temp$Present> thresh,"Present","Absent")
#Plot temp-only projected range
PA2.temp$col <- ifelse(PA2.temp$pres.abs.p == "Present","darkgreen","cornflowerblue")
plot(PA2.temp$LON,PA2.temp$LAT,col=PA2.temp$col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)

#Plot differences from current model
PA2.temp$delta <- ifelse(PA2.temp$pres.abs.p == "Absent" & PA.predict$pres.abs.p == "Present","red",
                         ifelse(PA2.temp$pres.abs.p == "Present" & PA.predict$pres.abs.p == "Absent","darkgoldenrod",PA.predict$col))
plot(PA2.temp$LON,PA2.temp$LAT,col=PA2.temp$delta,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)



#Control test - same methods but original climate data
PA2.ctrl <- readRDS("Objects/PA_climate_proj_2.rds") ##note column nums have changed
PA2.ctrl <- PA2.ctrl[,-c(17:35)]
PA2.ctrl <- cbind(PA2.ctrl,PA[,c(8:26)])
PA2.ctrl <- PA2.ctrl[,c(1:7,17:35,8:16)] #reorder to same column order as original data
predicted.PA2.ctrl2 <- predict(sdm1,newdata=PA2.ctrl[,-c(3,6)],type="prob",na.action=na.pass) 
PA2.ctrl <- cbind(PA2.ctrl,predicted.PA2.ctrl2)
PA2.ctrl$pres.abs.p <- ifelse(PA2.ctrl$Present> thresh,"Present","Absent")
#Plot ctrl-only projected range
PA2.ctrl$col <- ifelse(PA2.ctrl$pres.abs.p == "Present","green","blue")
plot(PA2.ctrl$LON,PA2.ctrl$LAT,col=PA2.ctrl$col,pch=20,xlim=c(-100,-70),ylim=c(25,50),asp=1,cex=.3)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)




##################
# Map climate projected differences
####
bioclim.he <- readRDS("Objects/bioclim_HE.rds")
bioclim <- readRDS("Objects/bioclim.rds")

plot(bioclim.he$bio5 - bioclim$bio5,ylim=c(25,50)) # Change in max temp of warmest month
plot(usa,add=T)

plot(bioclim.he$bio12 - bioclim$bio12,ylim=c(25,50)) # Change in annual preicp
plot(usa,add=T)

plot(bioclim.he$bio10 - bioclim$bio10,ylim=c(25,50)) # Change in annual preicp
plot(usa,add=T)

plot(bioclim$bio4) #temp seasonality
plot(bioclim.he$bio4 - bioclim$bio4,ylim=c(25,50)) # Change in temp seasonality
plot(usa,add=T)

plot(bioclim$bio15,ylim=c(30,45))
plot(usa,add=T)
