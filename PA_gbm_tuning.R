###########
###   Code for fitting and tuning gbms to the black locust FIA presence/absence data 
###   Goal: End with one best model which can be used for predictions
### 
###   Best model so far: interaction depth = 21, n.minobsinnode = 5, n.trees = 1300, shrinkage = 0.01
###                    CV performance: AUC=0.9297, Sensitivity = 0.7228, Specificity = 0.9231
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
set.seed(1337)
intrain <- createDataPartition(y=PA$pres,p=0.8,list=FALSE)
PA.train.full <- PA[intrain,]
PA.test.full <-  PA[-intrain,]
PA.train <- PA.train.full[,c(-2)] #Remove IV values (unused response var)
PA.train <- PA.train[c(2,1:nrow(PA.train)),]  ### Put an absent as the first value
PA.test <- PA.test.full[,c(-2)] #Remove IV values (unused response var)
PA.train.c <- PA.train[complete.cases(PA.train),]


############################
# Experiment with different hyperparameters
###########

gbmGrid <-  expand.grid(interaction.depth =c(5),
                        n.trees = c(100*(1:60)),
                        shrinkage = c(0.01),
                        n.minobsinnode = c(3))

gbmGrid.2 <-  expand.grid(interaction.depth =c(12),
                        n.trees = c(100*(1:140)),
                        shrinkage = 0.01,
                        n.minobsinnode = c(3,5))

gbmGrid.3 <-  expand.grid(interaction.depth =c(3),
                          n.trees = c(100*(1:90)),
                          shrinkage = 0.05,
                          n.minobsinnode = c(5,10,15))

#CV control settings
ctrlPar <- trainControl(method='repeatedcv', repeats=5,allowParallel=TRUE,classProbs=TRUE,summaryFunction=twoClassSummary,sampling="smote")
ctrlParFast <- trainControl(method='cv',number=5,allowParallel=TRUE,classProbs=TRUE,
                            summaryFunction=twoClassSummary,
                            sampling = "smote")


## For parallel
library(doParallel)
max <- detectCores()
c1 <- makeCluster(round(max*.5))
registerDoParallel(c1)
set.seed(8081)
smote.tune <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=gbmGrid,metric="ROC")
#saveRDS(smote.tune,file="Objects/PA_GBM_Models/gbmTune_3_7_17v6.rds")
smote.tune
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()


#Fit another GBM for comparison
max <- detectCores()
c1 <- makeCluster(round(max*.5))
registerDoParallel(c1)
set.seed(8081)
smote.tune2 <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=gbmGrid.2,metric="ROC")
smote.tune2
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()

##Fit GBM w/o smote
max <- detectCores()
c1 <- makeCluster(round(max*.5))
registerDoParallel(c1)
ctrlParFast_noSmote <- ctrlParFast
ctrlParFast_noSmote$sampling = NULL
set.seed(8081)
orig.tune <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=gbmGrid,metric="ROC")
orig.tune
stopCluster(c1)
registerDoParallel()



#Fit GBM w downsampling
c1 <- makeCluster(round(max*.5))
registerDoParallel(c1)
ctrlParFast_down <- ctrlParFast
ctrlParFast_down$sampling = "down"
set.seed(8081)
down.tune <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=gbmGrid,metric="ROC")
down.tune
stopCluster(c1)
registerDoParallel()
ggplot(down.tune)+theme_bw()


#Compare different sampling methods
models <- list(original = orig.tune, SMOTE = smote.tune, downsampling = down.tune)
model_resampling <- resamples(models)
summary(model_resampling, metric="ROC")



## Test smote default settings
PA.train.smote <- SMOTE(pres~.,PA.train)
prop.table(table(PA.train.smote$pres)) # with default settings we get 57% present and 42% absent


#### 
# Examine the SMOTE-sampled model
###

summary(smote.tune)
ggplot(smote.tune)+theme_bw()
confusionMatrix(smote.tune)
varImp(smote.tune)
plot(smote.tune$finalModel,i.var='Primary.rocktype',type="response")
rock1 <- plot(smote.tune$finalModel,i.var='Primary.rocktype',type="response",return.grid=T)
rock1[order(rock1$y,decreasing=T),]
#72 = biotite schist, 16 = schist, 45 = silt, 112 = granulite, 15 = chert, 114 = tuff,  74 = felsic metavolcanic rock, 72 = biotite schist
plot(smote.tune$finalModel,i.var='bio3',type="response")
plot(smote.tune$finalModel,i.var='bio5',type="response")
plot(smote.tune$finalModel,i.var='bio1',type="response")
plot(smote.tune$finalModel,i.var='bio10',type="response")
plot(smote.tune$finalModel,i.var='OC',type="response")
plot(smote.tune$finalModel,i.var='lith.pred',type="response")
plot(smote.tune$finalModel,i.var='STDAGE',type="response")
plot(smote.tune$finalModel,i.var='SLOPE',type="response")
plot(smote.tune$finalModel,i.var='LAT',type="response")
plot(smote.tune$finalModel,i.var='bio11',type="response")



#Find optimum threshold for class "Present"- based on training data
pres.abs <- ifelse(PA.train$pres == "Present",1,0)
predicted.PA.train <- predict(smote.tune,newdata=PA.train[,-1],type="prob",na.action=na.pass) #predict on train
t.opt <- optim.thresh((obs=as.numeric(pres.abs)),pred=predicted.PA.train$Present)


#training ROC curve
roc.obj <- roc(pres.abs,pred=predicted.PA.train$Present)
plot.roc(roc.obj,thresholds="best",print.thres="best") 
auc(roc.obj)


#Test set performance
#predicted.PA <- predict(smote.tune,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set - probabilities
#predicted.PA.2 <- predict(smote.tune,newdata=PA.test[,-5],type="raw",na.action=na.pass) #predict class - note NOT using opt threshold
#confusionMatrix(predicted.PA.2,PA.test$pres) # check if classes are lined up properly?
#roc.obj <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA$Present)
#auc(roc.obj)
#plot.roc(roc.obj,thresholds="best",print.thres="best") 







################## Experiment 3/8/17 optimizing depth 
##
##   Depths 15-22 appear to have generally equivalent AUC.
##       Note: min node size varies (5 for depths 22-21, 4 for 19 and 2 for 17,15)
##   However, highest AUC with smallest range is depth 21 min 5 (so far)
#####


#compare four different depths from models previously fit
tune1<- readRDS("Objects/PA_GBM_Models/gbmTune_3_6_17.rds")
tune2 <- readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v2.rds")
tune3 <- readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v3.rds")
tune4 <- readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v4.rds")
tune5 <- readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v5.rds")
tune6 <- readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v6.rds")
models <- list("Depth 22, min=5"=tune1,"Depth 21, min=5"=tune2,"Depth 19, min=4"=tune3,"Depth 17, min=2"=tune4, "Depth 15, min=2"=tune5,"Depth 5, min=3"=tune6)
resamps <- resamples(models)
summary(resamps)
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))
dotplot(resamps, metric = "ROC")


##compare performance on test set - so far Depth 21 (min node 5) is best at AUC .9321
predicted.PA.22 <- predict(tune1,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set
roc.22 <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.22$Present)
auc(roc.22) 

predicted.PA.21 <- predict(tune2,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set
roc.21 <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.21$Present)
auc(roc.21) 

predicted.PA.19 <- predict(tune3,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set
roc.19 <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.19$Present)
auc(roc.19) 

predicted.PA.17 <- predict(tune4,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set
roc.17 <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.17$Present)
auc(roc.17) 

predicted.PA.15 <- predict(tune5,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set
roc.15 <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.15$Present)
auc(roc.15) 

predicted.PA.5 <- predict(tune6,newdata=PA.test[,-6],type="prob",na.action=na.pass) #predict on test set
roc.5 <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.5$Present)
auc(roc.5) 




#####
#  6/19/17 - Try bayesian optimization of hyperparameters - optimize Depth and n.minobsinnode, try all ntrees 
#             Give stats for best ntrees value
####
library(rBayesianOptimization)

gbmFit_bayes <- function(interaction.depth, n.minobsinnode){
    set.seed(15325)
    n.trees <- seq(from=100,to=3000,by=100)
    shrinkage <- 0.01
    ctrlParFast <- trainControl(method='cv',number=5,allowParallel=TRUE,classProbs=TRUE,
                                summaryFunction=twoClassSummary,
                                sampling = "smote")
    model <- train(PA.train[,-1],PA.train[,1],method='gbm',metric="ROC",
                   trControl=ctrlParFast, tuneGrid = data.frame(interaction.depth, n.trees, shrinkage, n.minobsinnode))
    list(Score = getTrainPerf(model)[, "TrainROC"], Pred = 0)
    
    
}

## Define the bounds of the search
bounds <- list(interaction.depth = c(1L,34L),
               n.minobsinnode = c(1L, 25L))

bayes_search <- BayesianOptimization(gbmFit_bayes,bounds=bounds,    init_points = 20, #init_grid_dt=rand_search_bayes, 
                                     n_iter=30,acq="ucb", kappa=1, eps=0.0, verbose=T,
                                     kernel=list(type="matern", nu=5/2))
saveRDS(bayes_search3,file="Objects/PA_GBM_Models/6_19_17_bayes.rds")





########
# Best model so far
######
bestMod <- readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v2.rds")

getTrainPerf(bestMod)
#Find optimum threshold for class "Present"- based on training data
pres.abs <- ifelse(PA.train$pres == "Present",1,0)
predicted.PA.train <- predict(bestMod,newdata=PA.train[,-1],type="prob",na.action=na.pass) #predict on train
t.opt <- optim.thresh((obs=as.numeric(pres.abs)),pred=predicted.PA.train$Present)
predicted.PA.train.class <- ifelse(predicted.PA.train$Present> thresh,"Present","Absent")
confusionMatrix(predicted.PA.train.class,pres.abs)





######
# Fit GBM with ONLY climate data
####
#
# Findings - fitting with only bioclim variables is almost as good, but AUC is significantly
#           lower for both train and test data (~.92 vs ~.93)
#
bio.vars <- paste("bio",1:19,sep="")
PA.train.clim <- PA.train[,bio.vars]
PA.test.clim <- PA.test[,bio.vars]


gbmGrid2 <-  expand.grid(interaction.depth =c(3,5,10,15,21),
                        n.trees = c(100*(1:60)),
                        shrinkage = c(0.01),
                        n.minobsinnode = c(1,3,5))


## For parallel
library(doParallel)
max <- detectCores()
c1 <- makeCluster(round(max*.7))
registerDoParallel(c1)
#ctrlPar <- trainControl(method='repeatedcv', repeats=5,allowParallel=TRUE,classProbs=TRUE,summaryFunction=twoClassSummary)
ctrlParFast <- trainControl(method='cv',number=5,allowParallel=TRUE,classProbs=TRUE,
                            summaryFunction=twoClassSummary,
                            sampling = "smote")




#Fit GBM w smote
set.seed(8081)
smote.tune <- train(PA.train.clim,PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=gbmGrid2,metric="ROC")
#saveRDS(smote.tune,file="Objects/PA_GBM_Models/gbm_clim_env_3_13_17.rds")
smote.tune
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()


##compare performance on test set - so far Depth 21 (min node 5) is best at AUC .9321
predicted.PA <- predict(smote.tune,newdata=PA.test.clim,type="prob",na.action=na.pass) #predict on test set
roc <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA$Present)
auc(roc) 
summary(smote.tune)


#Find optimum threshold - base on training data - for climate only model - ~.4
pres.abs <- ifelse(PA.train$pres == "Present",1,0)
predicted.PA.train <- predict(smote.tune,newdata=PA.train.clim,type="prob",na.action=na.pass) #predict on train
t.opt <- optim.thresh((obs=as.numeric(pres.abs)),pred=predicted.PA.train$Present)
thresh <- t.opt$`sensitivity=specificity`

#compare performance on test set - using threshold and P/A prediction
predicted.PA.class <- ifelse(predicted.PA$Present> thresh,"Present","Absent")
confusionMatrix(predicted.PA.test.class,PA.test[,1])

## Fit gbm with only soil data 

bio.vars <- paste("bio",1:19,sep="")
PA.train.nonclim <- PA.train[,!(colnames(PA.train) %in% c(bio.vars,"LAT","LON","pres"))]
PA.test.nonclim <- PA.test[,!(colnames(PA.train) %in% c(bio.vars,"LAT","LON","pres"))]


gbmGrid2 <-  expand.grid(interaction.depth =c(3,5,10,15,21),
                         n.trees = c(100*(1:20)),
                         shrinkage = c(0.01),
                         n.minobsinnode = c(1,3,5))


## For parallel
library(doParallel)
max <- detectCores()
c1 <- makeCluster(round(max*.7))
registerDoParallel(c1)
#ctrlPar <- trainControl(method='repeatedcv', repeats=5,allowParallel=TRUE,classProbs=TRUE,summaryFunction=twoClassSummary)
ctrlParFast <- trainControl(method='cv',number=5,allowParallel=TRUE,classProbs=TRUE,
                            summaryFunction=twoClassSummary,
                            sampling = "smote")




#Fit GBM w smote
set.seed(8081)
smote.tune <- train(PA.train.nonclim,PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=gbmGrid2,metric="ROC")
#saveRDS(smote.tune,file="Objects/PA_GBM_Models/gbm_nonclim_3_13_17.rds")
smote.tune
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()

