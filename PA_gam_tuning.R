###########
### 
### Code to fit a boosted geoadditive GAM to the presence/absence data
### 
###
#########


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



#Read data and format
PA <- read.csv("Datafiles/data.national.6.5.csv") #now using reduced factors & elevation
PA$lith.pred <- factor(PA$lith.pred)
PA$Primary.rocktype <- factor(PA$Primary.rocktype)
PA$Secondary.rocktype <- factor(PA$Secondary.rocktype)
PA$usda_tex <- factor(PA$usda_tex)
PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
PA <- PA[,colnames(PA)!="PlotID"]
PA <- PA[,c(6,9,1:5,7,8,10:length(PA))] #reorder to put responses first
PA <- PA[,colnames(PA)!="asp_val"] #For geoGAM only - remove asp_val due to NA's
PA <- PA[complete.cases(PA),] #for geoGAM only - remove non-complete cases
PA <- PA[PA$usda_tex != 13,]
PA$usda_tex <- factor(PA$usda_tex)
colnames(PA)[3:4] = c("x","y")
set.seed(1337)
intrain <- createDataPartition(y=PA$pres,p=0.8,list=FALSE)
PA.train.full <- PA[intrain,]
PA.test.full <-  PA[-intrain,]
PA.train <- PA.train.full[,c(-2)] #Remove IV values (unused response var)
PA.train <- PA.train[c(2,1:nrow(PA.train)),]  ### Put an absent as the first value
PA.test <- PA.test.full[,c(-2)] #Remove IV values (unused response var)
PA.train.c <- PA.train[complete.cases(PA.train),]
PA.train.smote <- SMOTE(pres~.,data=PA.train,perc.under=300)

#Fit model
c1 <- makeCluster(15)
registerDoParallel(c1)


gam <- geoGAM(response="pres", covariates=names(PA.train.smote)[4:ncol(PA.train.smote)], data=PA.train.smote,
              coords=c("x","y"), non.stationary=T, seed=8081,
              verbose=1, cores=15,validation.data=PA.test)
#saveRDS(gam,"Objects/PA_GAM_Models/geoGam_6_25_17.rds")


stopCluster(c1)
registerDoParallel()

gam <- readRDS("Objects/PA_GAM_Models/geoGam_6_25_17.rds")

summary(gam)
summary(gam,what="path")

