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
#PA.train <- PA.train[c(2,1:nrow(PA.train)),]  ### Put an absent as the first value
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
ctrlPar <- trainControl(method='repeatedcv',number=5, repeats=5,allowParallel=TRUE,classProbs=TRUE,summaryFunction=twoClassSummary,sampling="smote")
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
max <- detectCores()
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
#  6/26/17 - Try a large random grid search - 200 combinations 
#      Note: 6/30/17 try depths below 10 but more trees
#      Total models tried: 6/26 - 8000..... 6/30: 
####

set.seed(1546)
seeds <- vector(mode = "list", length = 26)
for(i in 1:25) seeds[[i]] <- sample.int(1000, 50)
seeds[[26]] <- sample.int(1000, 1) ## For the last model
ctrlParSeeds <- trainControl(method='repeatedcv',number=5, repeats=5,allowParallel=TRUE,classProbs=TRUE,
                        summaryFunction=twoClassSummary,sampling="smote",seeds=seeds)
rand_search <- foreach(i=1:100, .combine='rbind') %do% {
  set.seed(i*10)
  randGrid <-expand.grid(interaction.depth =sample(1:10,1),
                          n.trees = c(100*(1:80)),
                          shrinkage = c(0.01),
                          n.minobsinnode = sample(1:15,1))
  c1 <- makeCluster(25)
  registerDoParallel(c1)

  trainIter <- train(PA.train[,-1],PA.train[,1],method='gbm',
                     trControl=ctrlParSeeds,tuneGrid=randGrid,metric="ROC")
  stopCluster(c1)
  registerDoParallel()
  trainIter$results
}
best <- rand_search[which.max(rand_search$ROC),]
best
#saveRDS(rand_search,file="Objects/PA_GBM_Models/6_30_17_randgrid.rds")

rand_search <- readRDS(file="Objects/PA_GBM_Models/6_26_17_randgrid.rds")
head(rand_search[order(-rand_search$ROC),])
plot(ROC~interaction.depth,data=rand_search) #AUC increases with interaction.depth 
plot(ROC~n.minobsinnode,data=rand_search) #No strong rel. between AUC and min obs in node
plot(ROC~n.trees,data=rand_search) #Low interaction depths may have needed more trees
plot(ROC~n.trees,data=subset(rand_search,interaction.depth<3)) #Depths below 8 may need more trees



#Best found by random search: Depth 32, n.minobsinnode=12, trees= 800, AUC = 0.91595, AUC_SD = 0.0041
#- also tested smaller interaction depths (<=10) with more trees, best = depth 9, n.minobsinnode=4, trees=2200, AUC= 0.9145, AUC_SD=0.005

randGrid <-expand.grid(interaction.depth =c(9),
                        n.trees = c(100*(1:25)),
                        shrinkage = c(0.01),
                        n.minobsinnode = c(4))
c1 <- makeCluster(round(detectCores()*.5))
registerDoParallel(c1)
set.seed(3554)
randTune <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlPar,tuneGrid=randGrid,metric="ROC")
#saveRDS(randTune,file="Objects/PA_GBM_Models/randBest_7_3_17.rds")
randTune
getTrainPerf(randTune)
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()


#check full training set AUC
predicted.PA.train <- predict(randTune,newdata=PA.train[,-1],type="prob",na.action=na.pass) #predict on train
roc.obj <- roc(ifelse(PA.train$pres == "Present",1,0),pred=predicted.PA.train$Present)
auc(roc.obj) #AUC 0.9556


#test set perf
predicted.PA.test <- predict(randTune,newdata=PA.test[,-1],type="prob",na.action=na.pass) #predict on test set
roc.obj <- roc(ifelse(PA.test$pres == "Present",1,0),pred=predicted.PA.test$Present)
auc(roc.obj) # AUC 0.9082



#compare best all depth vs best depth <=10
all <- readRDS(file="Objects/PA_GBM_Models/randBest_6_26_17.rds")
small <- readRDS(file="Objects/PA_GBM_Models/randBest_7_3_17.rds")
resamp <- resamples(list("Best Overall"=all, "Small depth"=small))
summary(resamp)
trellis.par.set(theme1)
bwplot(resamp, layout = c(2, 1))
dotplot(resamp, metric = "ROC") 



#####
#  6/19/17 - Try bayesian optimization of hyperparameters - optimize Depth and n.minobsinnode, try all ntrees 
#             Give stats for best ntrees value
####
library(rBayesianOptimization)
library(data.table)

#Use our random grid search as the starting point for bayesian optimization
rand_search <- readRDS(file="Objects/PA_GBM_Models/6_26_17_randgrid.rds")
rand_search <- rand_search[order(-rand_search$ROC),]
rand_search_init <- data.frame(unique(data.table(rand_search),by=c("interaction.depth","n.minobsinnode")))
# remove depths below 9 because they did not have enough trees in our rand grid search (and do not perform as well even with enough trees)
rand_search_init <- rand_search_init[rand_search_init$interaction.depth>8,c("interaction.depth","n.minobsinnode","ROC")] 
colnames(rand_search_init)[3] <- "Value"



gbmFit_bayes <- function(interaction.depth, n.minobsinnode){
    set.seed(15325)
    n.trees <- seq(from=100,to=5000,by=100)
    shrinkage <- 0.01
    ctrlParFast <- trainControl(method='cv',number=5,allowParallel=TRUE,classProbs=TRUE,
                                summaryFunction=twoClassSummary,
                                sampling = "smote")
    model <- train(PA.train[,-1],PA.train[,1],method='gbm',metric="ROC",
                   trControl=ctrlPar, tuneGrid = data.frame(interaction.depth, n.trees, shrinkage, n.minobsinnode))
    list(Score = getTrainPerf(model)[, "TrainROC"], Pred = 0)
    
    
}

## Define the bounds of the search
bounds <- list(interaction.depth = c(9L,34L),
               n.minobsinnode = c(1L, 25L))

bayes_search <- BayesianOptimization(gbmFit_bayes,bounds=bounds, init_grid_dt=rand_search_init,  #   init_points = 20,
                                     n_iter=30,acq="ucb", kappa=1, eps=0.0, verbose=T,
                                     kernel=list(type="matern", nu=5/2))
#saveRDS(bayes_search,file="Objects/PA_GBM_Models/7_3_17_bayes.rds")


#Found by bayesian optimization (6/19/17): Depth 27, n.minobsinnode = 9, AUC = 0.91501
#                               (7/3/17): Depth 32, n.minobsinnode=12, AUC = 0.9160 - same as best from random grid
bayesGrid <-expand.grid(interaction.depth =c(27),
                   n.trees = c(100*(1:30)),
                   shrinkage = c(0.01),
                   n.minobsinnode = c(9))
c1 <- makeCluster(round(detectCores()*.5))
registerDoParallel(c1)
set.seed(8081)
bayesTune <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlParFast,tuneGrid=bayesGrid,metric="ROC")
#saveRDS(smote.tune,file="Objects/PA_GBM_Models/gbmTune_3_7_17v6.rds")
bayesTune
getTrainPerf(bayesTune)
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()



########
# Best model so far
######
bestMod <-readRDS("Objects/PA_GBM_Models/randBest_6_26_17.rds")# readRDS("Objects/PA_GBM_Models/gbmTune_3_7_17v2.rds")


getTrainPerf(bestMod)
#Find optimum threshold for class "Present"- based on training data
pres.abs <- ifelse(PA.train$pres == "Present",1,0)
predicted.PA.train <- predict(bestMod,newdata=PA.train[,-1],type="prob",na.action=na.pass) #predict on train
t.opt <- optim.thresh((obs=as.numeric(pres.abs)),pred=predicted.PA.train$Present)
predicted.PA.train.class <- ifelse(predicted.PA.train$Present> thresh,"Present","Absent")
confusionMatrix(predicted.PA.train.class,pres.abs)



### 
# Get accuracy % for best model (via CV)
##

ctrlParFastAcc <- trainControl(method='cv',number=5,allowParallel=TRUE,classProbs=TRUE,
                            sampling = "smote")

bestGrid <-expand.grid(interaction.depth =c(32),
                        n.trees = c(100*(1:10)),
                        shrinkage = c(0.01),
                        n.minobsinnode = c(12))
c1 <- makeCluster(round(detectCores()*.7))
registerDoParallel(c1)
set.seed(8081)
bestTuneAcc <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlParFastAcc,tuneGrid=bestGrid,metric="Accuracy")
#saveRDS(smote.tune,file="Objects/PA_GBM_Models/gbmTune_3_7_17v6.rds")
bestTuneAcc
getTrainPerf(bestTuneAcc)
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()






#####
#compare best models found by manual search, random grid search, and bayesian optimization
#####

resamps <- resamples(list("Bayesian Opt."=bayesTune,"Manual search"=bestMod, "Random grid"=randTune))
summary(resamps)
trellis.par.set(theme1)
bwplot(resamps, layout = c(2, 1))
dotplot(resamps, metric = "ROC")


#refit all 3 "best model" candidates with more cross validations
compareGrid <-expand.grid(interaction.depth =c(21,27,28),
                          n.trees = c(50*(1:30)),
                          shrinkage = c(0.01),
                          n.minobsinnode = c(5,9,7))
compareGrid <- compareGrid[(compareGrid$interaction.depth==21&compareGrid$n.minobsinnode==5)|
                             (compareGrid$interaction.depth==27&compareGrid$n.minobsinnode==9)|
                             (compareGrid$interaction.depth==28&compareGrid$n.minobsinnode==7),]
c1 <- makeCluster(round(detectCores()*.7))
registerDoParallel(c1)
set.seed(8081)
compareTune <- train(PA.train[,-1],PA.train[,1],method='gbm', trControl=ctrlPar,tuneGrid=compareGrid,metric="ROC")
saveRDS(compareTune,file="Objects/PA_GBM_Models/compareTune_6_25_17.rds")
compareTune
getTrainPerf(compareTune)
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()
varImp(compareTune)








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

