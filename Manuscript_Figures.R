######
# Black locust SDM project 
# Code to produce figures for dissertation / manuscript
#   by: Jeffrey Minucci  Date: 8/30/17
#####



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

#Load best model
sdm1 <- readRDS("Objects/PA_GBM_Models/randBest_6_26_17.rds")



######
# Figure 1: 1 panel figure showing location of all FIA plots used in our analysis. Colored by R. pseudoacaia presence/absence 
#####



# Get probability predictions for ALL DATA (test+train) 
predicted.PA <- predict(sdm1,newdata=PA[,-c(1,2)],type="prob",na.action=na.pass) 
PA.predict <- cbind(PA,predicted.PA)
PA.predict$pred.class <- ifelse(PA.predict$Present>=0.40,"Present","Absent")
PA.predict$class.col <- ifelse(PA.predict$pred.class == "Present","red","cornflowerblue")

# Make base raster
prob.spatial <- PA.predict[,c("LON","LAT","Present")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)

#Plot FIA plot locations

#pdf(width=7.5, height=14,file="ManuscriptFigures/Fig_1.0.pdf")
#par(mfrow=c(2,1),mar=c(5,5,2,2))
#PA$col <- ifelse(PA.predict$pres == "Present","red","cornflowerblue")
#lot(x,interpolate=T,xlab="Longitude",ylab="Latitude",col="white",legend=FALSE,cex.lab=1.5,cex.axis=1.5)
#points(LAT~LON, col=col,data=subset(PA,pres=="Absent"),pch=20,cex=.2)
#xlim=c(-102.5,-62),ylim=c(25,50)
#points(LAT~LON, col=col,data=subset(PA,pres!="Absent"),pch=20,cex=.2)
#plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
#legend(-77,30,legend=c("Present","Absent"),col=c("red","cornflowerblue"),pch=20,pt.cex=1,cex=1.5)
#points(LAT~LON,data=PA,pch=3,cex=.01)
#dev.off()


#New Fig 1
pdf(width=5, height=14,file="ManuscriptFigures/Fig_1.0_alt.pdf")
par(mfrow=c(3,1),mar=c(5,5,2,2))

### Subplot A - actual FIA data
PA$col <- ifelse(PA.predict$pres == "Present","red","cornflowerblue")
plot(x,interpolate=T,xlab="Longitude",ylab="Latitude",col="white",legend=FALSE,cex.lab=1.5,cex.axis=1.5)
points(LAT~LON, col=col,data=subset(PA,pres=="Absent"),pch=20,cex=.2)
#xlim=c(-102.5,-62),ylim=c(25,50)
points(LAT~LON, col=col,data=subset(PA,pres!="Absent"),pch=20,cex=.2)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
legend(-77,30,legend=c("Present","Absent"),col=c("red","cornflowerblue"),pch=20,pt.cex=1,cex=1.5)
usr <- par( "usr" )
text( usr[2], usr[4],"(a)",adj=c(2,2),cex=2)
text( usr[2], usr[4],"Actual   ",adj=c(1.75,2),cex=2)
#points(LAT~LON,data=PA,pch=3,cex=.01)


### Subplot B -  current probability - as raster

#pdf(width=7.5, height=7,file="ManuscriptFigures/Fig_2.0b.pdf")
#par(mar=c(5,5,2,2))
prob.spatial <- PA.predict[,c("LON","LAT","Present")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
plot(x,interpolate=T,xlab="Longitude",ylab="Latitude",cex.lab=1.5,cex.axis=1.5,
     axis.args=list(at=c(.2,.4,.6,.8),labels=c(20,40,60,80)))
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(b)",adj=c(2,2),cex=2)
text( usr[2], usr[4],"Predicted",adj=c(1.75,2),cex=2)
#dev.off()



### Subplot C - change from current prob (only precip changing) - as raster

par(mar=c(5,5,2,2))
prob.spatial <- PA.predict[,c("LON","LAT","Present")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
plot(x,interpolate=T,xlab="Longitude",ylab="Latitude",col="white",legend=FALSE,cex.lab=1.5,cex.axis=1.5)
points(LAT~LON, col=class.col,data=subset(PA.predict,pres=="Absent"),pch=20,cex=.2)
points(LAT~LON, col=class.col,data=subset(PA.predict,pres!="Absent"),pch=20,cex=.2)
legend(-77,30,legend=c("Present","Absent"),col=c("red","cornflowerblue"),pch=20,pt.cex=1,cex=1.5)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(c)",adj=c(2,2),cex=2)
text( usr[2], usr[4],"Predicted",adj=c(1.75,2),cex=2)
dev.off()




######
# Figure 2: 2 panel figure showing a) Probability of presence and b) Predicted class (pres/abs)
#####


# Get probability predictions for ALL DATA (test+train) 
predicted.PA <- predict(sdm1,newdata=PA[,-c(1,2)],type="prob",na.action=na.pass) 
PA.predict <- cbind(PA,predicted.PA)
PA.predict$pred.class <- ifelse(PA.predict$Present>=0.40,"Present","Absent")
PA.predict$class.col <- ifelse(PA.predict$pred.class == "Present","red","cornflowerblue")

### Subplot A - Plot probability - as raster

pdf(width=7.5, height=14,file="ManuscriptFigures/Fig_2.0.pdf")
par(mfrow=c(2,1),mar=c(5,5,2,2))
prob.spatial <- PA.predict[,c("LON","LAT","Present")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
plot(x,interpolate=T,xlab="Longitude",ylab="Latitude",cex.lab=1.5,cex.axis=1.5,
     axis.args=list(at=c(.2,.4,.6,.8),labels=c(20,40,60,80)))
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(a)",adj=c(2,2),cex=2)
#dev.off()

### Subplot B

#pdf(width=7.5, height=7,file="ManuscriptFigures/Fig_2.0b.pdf")
par(mar=c(5,5,2,2))
prob.spatial <- PA.predict[,c("LON","LAT","Present")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
plot(x,interpolate=T,xlab="Longitude",ylab="Latitude",col="white",legend=FALSE,cex.lab=1.5,cex.axis=1.5)
points(LAT~LON, col=class.col,data=subset(PA.predict,pres=="Absent"),pch=20,cex=.2)
points(LAT~LON, col=class.col,data=subset(PA.predict,pres!="Absent"),pch=20,cex=.2)
legend(-77,30,legend=c("Present","Absent"),col=c("red","cornflowerblue"),pch=20,pt.cex=1,cex=1.5)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(b)",adj=c(2,2),cex=2)
dev.off()

#######
# Fig 3 - 9 panels of partial dependence plots - pch = 19 for filled
######


pdf(width=11, height=11,file="ManuscriptFigures/Fig_3.pdf")
par(mfrow=c(3,3),mar=c(6.5,3.5,1,2.5),oma=c(0,5,0,1))

plot(plot(sdm1$finalModel,i.var='SLOPE',type="response",return.grid=T),xlab="Slope (m rise per 100 m)",
     ylab="",type="p",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1)
axis(2,at=c(0,.1,.2,.3,.4,.5),labels=c("0","10","20","30","40","50"),cex.axis=1.5)
usr <- par( "usr" )
text( usr[1], usr[4],"(a)",adj=c(-.5,1.5),cex=2)
#text(0,0.45,"(a)",cex=2)
#rug(PA.train$SLOPE)```

plot(plot(sdm1$finalModel,i.var='STDAGE',type="response",return.grid=T),xlab="Stand age (years)",
     ylab="",type="p",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1) 
axis(side=2, labels=FALSE)
usr <- par( "usr" )
text( usr[1], usr[4],"(b)",adj=c(-.5,1.5),cex=2)
#axis(1,at=c(200,250,300,350),labels=c("20","25","30","35"))
#rug(PA.train$bio5)


plot(plot(sdm1$finalModel,i.var='asp_val',type="response",return.grid=T),xlab="Aspect Index",
     ylab="",type="p",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1) 
axis(side=2, labels=FALSE)
usr <- par( "usr" )
text( usr[1], usr[4],"(c)",adj=c(-.5,1.5),cex=2)
#axis(1,at=c(200,250,300,350),labels=c("20","25","30","35"))
#rug(PA.train$bio5)

plot(plot(sdm1$finalModel,i.var='bio12',type="response",return.grid=T),xlab="Mean annual precip. (mm)",
     ylab="",type="p",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1)
axis(2,at=c(0,.1,.2,.3,.4,.5),labels=c("0","10","20","30","40","50"),cex.axis=1.5) 
usr <- par( "usr" )
text( usr[1], usr[4],"(d)",adj=c(-.5,1.5),cex=2)
#axis(1,at=c(0,50,100,150,200),labels=c("0","5","10","15","20"))


plot(plot(sdm1$finalModel,i.var='bio5',type="response",return.grid=T),xlab="Max temp. of warmest month (C)",
     ylab="",type="p",xaxt="n",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1) 
axis(side=2, labels=FALSE) 
axis(1,at=c(200,250,300,350),labels=c("20","25","30","35"),cex=2,cex.lab=2,cex.axis=1.5)
usr <- par( "usr" )
text( usr[1], usr[4],"(e)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='bio1',type="response",return.grid=T),xlab="Mean annual temperature (C)",
     ylab="",type="p",xaxt="n",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1) 
axis(side=2, labels=FALSE) 
axis(1,at=c(0,50,100,150,200),labels=c("0","5","10","15","20"),cex=2,cex.lab=2,cex.axis=1.5)
usr <- par( "usr" )
text( usr[1], usr[4],"(f)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='OC',type="response",continuous.resolution=1400,return.grid=T),xlab="Organic C content (%)",
     ylab="",type="p",ylim=c(0,.5),xlim=c(0,1.95),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1)
axis(2,at=c(0,.1,.2,.3,.4,.5),labels=c("0","10","20","30","40","50"),cex.axis=1.5) 
usr <- par( "usr" )
text( usr[1], usr[4],"(g)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='cec',type="response",continuous.resolution=200,return.grid=T),xlab="CEC (cmol/kg)",
     ylab="",type="p",ylim=c(0,.5),xlim=c(0,45),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1) 
axis(side=2, labels=FALSE) 
usr <- par( "usr" )
text( usr[1], usr[4],"(h)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='ph',type="response",continuous.resolution=100,return.grid=T),xlab="pH",
     ylab="",type="p",ylim=c(0,.5),xlim=c(4,8),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n',pch=1) 
axis(side=2, labels=FALSE) 
usr <- par( "usr" )
text( usr[1], usr[4],"(i)",adj=c(-.5,1.5),cex=2)

#Common y axis label
mtext(text="Probability of occurrence (%)",side=2,line=0,outer=TRUE,cex=1.5)

dev.off()



#######
# Fig 3 ALTERNATE - 9 panels of partial dependence plots - with lines
######


pdf(width=11, height=11,file="ManuscriptFigures/Fig_3alt.0.pdf")
par(mfrow=c(3,3),mar=c(6.5,3.5,1,2.5),oma=c(0,5,0,1))

plot(plot(sdm1$finalModel,i.var='SLOPE',type="response",return.grid=T),xlab="Slope (m rise per 100 m)",
     ylab="",type="l",lwd=5,ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n')
axis(2,at=c(0,.1,.2,.3,.4,.5),labels=c("0","10","20","30","40","50"),cex.axis=1.5)
usr <- par( "usr" )
text( usr[1], usr[4],"(a)",adj=c(-.5,1.5),cex=2)
#text(0,0.45,"(a)",cex=2)
#rug(PA.train$SLOPE)```

plot(plot(sdm1$finalModel,i.var='STDAGE',type="response",return.grid=T),xlab="Stand age (years)",
     ylab="",type="l",lwd=5,ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n') 
axis(side=2, labels=FALSE)
usr <- par( "usr" )
text( usr[1], usr[4],"(b)",adj=c(-.5,1.5),cex=2)
#axis(1,at=c(200,250,300,350),labels=c("20","25","30","35"))
#rug(PA.train$bio5)


plot(plot(sdm1$finalModel,i.var='asp_val',type="response",return.grid=T),xlab="Aspect Index",
     ylab="",type="l",lwd=5,ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n') 
axis(side=2, labels=FALSE)
usr <- par( "usr" )
text( usr[1], usr[4],"(c)",adj=c(-.5,1.5),cex=2)
#axis(1,at=c(200,250,300,350),labels=c("20","25","30","35"))
#rug(PA.train$bio5)

plot(plot(sdm1$finalModel,i.var='bio12',type="response",return.grid=T),xlab="Mean annual precip. (mm)",
     ylab="",type="l",lwd=5,ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n')
axis(2,at=c(0,.1,.2,.3,.4,.5),labels=c("0","10","20","30","40","50"),cex.axis=1.5) 
usr <- par( "usr" )
text( usr[1], usr[4],"(d)",adj=c(-.5,1.5),cex=2)
#axis(1,at=c(0,50,100,150,200),labels=c("0","5","10","15","20"))


plot(plot(sdm1$finalModel,i.var='bio5',type="response",return.grid=T),xlab="Max temp. of warmest month (C)",
     ylab="",type="l",lwd=5,xaxt="n",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n') 
axis(side=2, labels=FALSE) 
axis(1,at=c(200,250,300,350),labels=c("20","25","30","35"),cex=2,cex.lab=2,cex.axis=1.5)
usr <- par( "usr" )
text( usr[1], usr[4],"(e)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='bio1',type="response",return.grid=T),xlab="Mean annual temperature (C)",
     ylab="",type="l",lwd=5,xaxt="n",ylim=c(0,.5),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n') 
axis(side=2, labels=FALSE) 
axis(1,at=c(0,50,100,150,200),labels=c("0","5","10","15","20"),cex=2,cex.lab=2,cex.axis=1.5)
usr <- par( "usr" )
text( usr[1], usr[4],"(f)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='OC',type="response",continuous.resolution=1400,return.grid=T),xlab="Organic C content (%)",
     ylab="",type="l",lwd=5,ylim=c(0,.5),xlim=c(0,1.95),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n')
axis(2,at=c(0,.1,.2,.3,.4,.5),labels=c("0","10","20","30","40","50"),cex.axis=1.5) 
usr <- par( "usr" )
text( usr[1], usr[4],"(g)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='cec',type="response",continuous.resolution=200,return.grid=T),xlab="CEC (cmol/kg)",
     ylab="",type="l",lwd=5,ylim=c(0,.5),xlim=c(0,45),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n') 
axis(side=2, labels=FALSE) 
usr <- par( "usr" )
text( usr[1], usr[4],"(h)",adj=c(-.5,1.5),cex=2)

plot(plot(sdm1$finalModel,i.var='ph',type="response",continuous.resolution=100,return.grid=T),xlab="pH",
     ylab="",type="l",lwd=5,ylim=c(0,.5),xlim=c(4,8),cex=2,cex.lab=2,cex.axis=1.5,yaxt='n') 
axis(side=2, labels=FALSE) 
usr <- par( "usr" )
text( usr[1], usr[4],"(i)",adj=c(-.5,1.5),cex=2)

#Common y axis label
mtext(text="Probability of occurrence (%)",side=2,line=0,outer=TRUE,cex=1.5)

dev.off()


#######
# Fig 4 - 3 panels - a) projected 2050 probability, b) change from current, c) change without temperature
#####



# Get probability predictions for ALL DATA (test+train) 
#predicted.PA <- predict(sdm1,newdata=PA[,-c(1,2)],type="prob",na.action=na.pass) 
#PA.predict <- cbind(PA,predicted.PA)
#PA.predict$pred.class <- ifelse(PA.predict$Present>=0.40,"Present","Absent")
#PA.predict$class.col <- ifelse(PA.predict$pred.class == "Present","red","cornflowerblue")

#load results
d_raster_stack <- readRDS("RasterOutput/gcm_predict_rasters_delta_final.rds")
gcm_d_predict_avg <- overlay(d_raster_stack,fun=mean)
gcm_d_predict_avg <- setExtent(gcm_d_predict_avg,e)
raster_stack <- readRDS("RasterOutput/gcm_predict_rasters_final.rds")
gcm_predict_avg <- overlay(raster_stack,fun=mean)
d_raster_stack_noTemp <- readRDS("RasterOutput/gcm_predict_rasters_delta_noTemp.rds")
gcm_d_predict_avg_noTemp <- overlay(d_raster_stack_noTemp,fun=mean)


pdf(width=5, height=14,file="ManuscriptFigures/Fig_4.0.pdf")
par(mfrow=c(3,1),mar=c(5,5,2,2))

### Subplot A - projected 2050 plot probability - as raster
plot(gcm_predict_avg,interpolate=T,xlab="Longitude",ylab="Latitude",cex.lab=1.5,cex.axis=1.5,zlim=c(.05,.95),
     #legend.args=list(text='Probability (%)', side=4, font=2, line=1, cex=1),
     axis.args=list(at=c(.2,.4,.6,.8),labels=c(20,40,60,80)))
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(a)",adj=c(1.25,1.35),cex=2)
text( usr[2], usr[4],"2050 climate",adj=c(1.75,1.5),cex=2)
#dev.off()

### Subplot B - change from current probability - as raster

#pdf(width=7.5, height=7,file="ManuscriptFigures/Fig_2.0b.pdf")
#par(mar=c(5,5,2,2))
plot(gcm_d_predict_avg,interpolate=T,xlab="Longitude",ylab="Latitude",cex.lab=1.5,cex.axis=1.5,zlim=c(-.5,.5),
    # legend.args=list(text='Change in probability (%)', side=4, font=2, line=3.5, cex=1),
     axis.args=list(at=c(-.4,-.2,0,.2,.4),labels=c(-40,-20,0,20,40)))
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(b)",adj=c(1.25,1.35),cex=2)
text( usr[2], usr[4],"2050 climate",adj=c(1.75,1.5),cex=2)



### Subplot C - change from current prob (only precip changing) - as raster

plot(gcm_d_predict_avg_noTemp,interpolate=T,xlab="Longitude",ylab="Latitude",cex.lab=1.5,cex.axis=1.5,zlim=c(-.5,.5),
     #legend.args=list(text='Change in probability (%)', side=4, font=2, line=3.5, cex=1),
     axis.args=list(at=c(-.4,-.2,0,.2,.4),labels=c(-40,-20,0,20,40)))
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
usr <- par( "usr" )
text( usr[2], usr[4],"(c)",adj=c(1.25,1.35),cex=2)
text( usr[2], usr[4],("2050 precip. only"),adj=c(1.33,1.5),cex=2)
#text( usr[2], usr[4],("only"),adj=c(5,2.0),cex=2)


dev.off()



