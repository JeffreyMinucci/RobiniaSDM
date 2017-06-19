####
#### Code to fit Hurdle model of black locust abundance:
#### combining presence/absence and regression stages of modeling into one model
#### 
#### At bottom: trying a non-hurdle geoadditive boosted GAM (geoGAM package
####
#### By: Jeff Minucci
#### Date:4/21/17



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
library(gbm)


gbm_mod <- getModelInfo("gbm",regex=FALSE)[[1]]

lpHurdleGBM<- list(type = c("Regression"),
                   library = c("gbm","plyr","DMwR"),
                   loop = NULL,prob=NULL) 

params <- data.frame(parameter=c("n.trees.1","interaction.depth.1","shrinkage.1","n.minobsinnode.1","threshold",
                                 "n.trees.2","interaction.depth.2","shrinkage.2","n.minobsinnode.2"),
                     class = rep("numeric",9), label=c("Boosting Iterations 1","Max Tree Depth 1","Shrinkage 1",
                                                       "Min. Terminal Node Size 1","Threshold of Hurdle",
                                                       "Boosting Iterations 2","Max Tree Depth 2","Shrinkage 2",
                                                       "Min. Terminal Node Size 2"))
lpHurdleGBM$parameters <- params


hurdleGBM_Grid <- function(x, y, len = NULL, search = "grid") {
  if(search == "grid") {
    out <- expand.grid(interaction.depth.1 = seq(1, len),
                       n.trees.1 = floor((1:len) * 50),
                       shrinkage.1 = .1,
                       n.minobsinnode.1 = 10,
                       threshold = seq(0,.6,by=.1),
                       interaction.depth.2 = seq(1, len),
                       n.trees.2 = floor((1:len) * 50),
                       shrinkage.2 = .1,
                       n.minobsinnode.2 = 10,
                       decomp=T)
  } else {
    out <- data.frame(n.trees.1 = round(runif(len, min = 1000, max = 3000),-2),
                      interaction.depth.1 = sample(3:25, replace = TRUE, size = len),         
                      shrinkage.1 = rep(.001,times=len),
                      n.minobsinnode.1 = sample(3:25, replace = TRUE, size = len),
                      threshold = sample(seq(.1,.6,by=.05), replace = TRUE, size = len),
                      n.trees.2 = round(runif(len, min = 1000, max = 3000),-2),
                      interaction.depth.2 = sample(3:25, replace = TRUE, size = len),         
                      shrinkage.2 = rep(.001,times=len),
                      n.minobsinnode.2 = sample(3:25, replace = TRUE, size = len),
                      decomp=rep(T,times=len))
    out <- out[!duplicated(out),]
  }
  out
}

hurdleGBM_Grid_constrained <- function(x, y, len = NULL, search = "grid") {
  if(search == "grid") {
    out <- expand.grid(interaction.depth.1 = seq(1, len),
                       n.trees.1 = floor((1:len) * 50),
                       shrinkage.1 = .1,
                       n.minobsinnode.1 = 10,
                       threshold = seq(0,.6,by=.1),
                       interaction.depth.2 = seq(1, len),
                       n.trees.2 = floor((1:len) * 50),
                       shrinkage.2 = .1,
                       n.minobsinnode.2 = 10)
  } else {
    out <- data.frame(n.trees.1 = rep(5000,times=len),#round(runif(len, min = 1000, max = 3000),-2),
                      interaction.depth.1 = sample(1:34, replace = TRUE, size = len),         
                      shrinkage.1 = rep(.001,times=len),
                      n.minobsinnode.1 = rep(5,times=len),#sample(c(5), replace = TRUE, size = len),
                      threshold = sample(seq(0,1,by=.01), replace = TRUE, size = len),
                      n.trees.2 = rep(3500,times=len),#round(runif(len, min = 1000, max = 4000),-2),
                      interaction.depth.2 = sample(1:34, replace = TRUE, size = len),         
                      shrinkage.2 = rep(.001,times=len),
                      n.minobsinnode.2 = rep(5,times=len))#sample(c(5), replace = TRUE, size = len))
                      

    
    out <- out[!duplicated(out),]
  }
  out 
}

#lpHurdleGBM$grid <- hurdleGBM_Grid
lpHurdleGBM$grid <- hurdleGBM_Grid_constrained


hurdleGBM_Fit <- function(x, y, wts, param, lev, last, classProbs, ...) { 
  theDots <- list(...)
  #Step 1: Fit classification model to predict odds of presence
  y.class <- ifelse(y>0,1,0)
  x_mod <- x
  x_mod$y.class <- as.factor(y.class)
  #x_mod$y.class <- as.numeric(x_mod$y.class)
  #SMOTE requires factor as y
  #modArgs1 <- list(formula=y.class~.,
  #                 data=x_mod,
  #                interaction.depth = param$interaction.depth.1,
  #                n.trees = param$n.trees.1,
  #                shrinkage = param$shrinkage.1, 
  #               n.minobsinnode = param$n.minobsinnode.1)
  #if(length(theDots) > 0) modArgs1 <- c(modArgs1,theDots)
  #model1 <- do.call("gbm", modArgs1)
  x_mod_SMOTE <- SMOTE(y.class~.,data=x_mod)
  x_mod_SMOTE$y.class <- as.numeric(as.character(x_mod_SMOTE$y.class)) #gbm requires numeric response (0-1)
  model1 <- gbm(y.class~.,data=x_mod_SMOTE,interaction.depth = param$interaction.depth.1,n.trees = param$n.trees.1,
                shrinkage = param$shrinkage.1,n.minobsinnode = param$n.minobsinnode.1)
  model1_predictions <- gbm::predict.gbm(model1,newdata=x,type="response",n.trees=param$n.trees.1,na.action=na.pass)
  x.predict.present <- x[model1_predictions>param$threshold,]
  y.predict.present <- y[model1_predictions>param$threshold]
  #Step 2: Fit regression model to plots where odds of presence is greater than threshold
  #modArgs2 <- list(x = x.predict.present,
  #                y = y.predict.present,
  #               interaction.depth = param$interaction.depth.2,
  #              n.trees = param$n.trees.2,
  #             shrinkage = param$shrinkage.2, 
  #            n.minobsinnode = param$n.minobsinnode.2,
  #           distribution = "gaussian")
  #if(length(theDots) > 0) modArgs2 <- c(modArgs2,theDots)
  #model2 <- do.call("gbm.fit", modArgs2)
  x.predict.present$y <- y.predict.present 
  model2 <- gbm(y~.,data=x.predict.present,interaction.depth = param$interaction.depth.2,n.trees = param$n.trees.2,
                shrinkage = param$shrinkage.2,n.minobsinnode = param$n.minobsinnode.2)
  model1$continuous_mod <- model2
  model1$threshold <- param$threshold
  
  return(model1)
}


lpHurdleGBM$fit <- hurdleGBM_Fit


hurdleGBM_Predict <- function(modelFit, newdata, submodels = NULL) {
  odds.present <- gbm::predict.gbm(modelFit,newdata,type="response",n.trees=modelFit$tuneValue$n.trees.1,
                                   na.action=na.pass)
  #print(odds.present)
  present <- factor(ifelse(odds.present>modelFit$threshold,"Present","Absent"))
  predicted.IV <- gbm::predict.gbm(modelFit$continuous_mod,newdata,n.trees=modelFit$tuneValue$n.trees.2,
                                   na.action=na.pass)
  #print(predicted.IV)
  
  out <- ifelse(present=="Present",predicted.IV,0)
  if(!is.null(submodels)) {                   
    tmp <- out
    out <- vector(mode = "list", length = nrow(submodels) + 1)
    out[[1]] <- tmp
    for(j in seq(along = submodels$n.trees.2)) {
      odds.present <- gbm::predict.gbm(modelFit,newdata,type="response",n.trees=submodels$n.trees.1[j],
                                       na.action=na.pass)
      present <- factor(ifelse(odds.present>submodels$threshold[j],"Present","Absent"))
      predicted.IV <- gbm::predict.gbm(modelFit$continuous_mod,newdata,
                                       n.trees=submodels$n.trees.2[j],
                                       na.action=na.pass)
      out[[j+1]] <- ifelse(present=="Present",predicted.IV,0)
    }
  }
  return(out)                   
}

lpHurdleGBM$predict <- hurdleGBM_Predict 


hurdleGBM_sort <- function(x) x[order(x$interaction.depth.1),]
lpHurdleGBM$sort <- hurdleGBM_sort 


#hurdleGBM_loop = function(grid) {            
# loop <- grid[which.max(grid$n.trees.2),,drop = FALSE]
#submodels <- grid[-which.max(grid$n.trees.2),,drop = FALSE]
#submodels <- list(submodels)  
#list(loop = loop, submodels = submodels)
#} 
#lpHurdleGBM$loop <- hurdleGBM_loop


#hurdleGBM_loop2 <- function(grid) {     
#  loop <- ddply(grid, c("shrinkage.1", "interaction.depth.1", "n.minobsinnode.1","threshold","n.trees.1",
#                        "shrinkage.2","interaction.depth.2","n.minobsinnode.2"),
#                function(x) c(n.trees.2 = max(x$n.trees.2)))
#  submodels <- vector(mode = "list", length = nrow(loop))
#  for(i in seq(along = loop$n.trees.1)) {
#    index <- which(grid$interaction.depth.1 == loop$interaction.depth.1[i] & 
#                     grid$shrinkage.1 == loop$shrinkage.1[i] &
#                     grid$n.minobsinnode.1 == loop$n.minobsinnode.1[i] & grid$threshold == loop$threshold[i] &
#                     grid$n.trees.1 == loop$n.trees.1[i] & grid$interaction.depth.2 == loop$interaction.depth.2[i] &
#                     grid$n.minobsinnode.2 == loop$n.minobsinnode.2[i],grid$n.shrinkage.2 == loop$shrinkage.2[i])
#    trees2 <- grid[index, "n.trees.2"] 
#    submodels[[i]] <- data.frame(n.trees.2 = trees2[trees2 != loop$n.trees.2[i]])
#  }    
#  list(loop = loop, submodels = submodels)
#}


#lpHurdleGBM$loop <- hurdleGBM_loop2


#looping which uses both ntrees1 and ntrees2
hurdleGBM_loop_trees <- function(grid) {
  
  loop <- grid
  submodels <- vector(mode = "list", length = nrow(loop))
  for(i in 1:nrow(loop)){
      len <- length(seq(1000,loop[i,"n.trees.2"],by=250)) * length(seq(1000,loop[i,"n.trees.1"],by=250))
      #submodels[[i]] <- data.frame(matrix(rep(grid[i,],times=len),ncol=9,byrow=T))
      submodel <- grid[i,]
      submodel <- submodel[rep(seq_len(nrow(submodel)), each=len),]  
      submodels[[i]] <- submodel
      submodels[[i]]$n.trees.2 <- rep(seq(1000,loop[i,"n.trees.2"],by=250),each=length(seq(1000,loop[i,"n.trees.1"],by=250)))
      submodels[[i]]$n.trees.1 <- rep(seq(1000,loop[i,"n.trees.1"],by=250),times=length(seq(1000,loop[i,"n.trees.2"],by=250)))
      submodels[[i]] <- subset(submodels[[i]],!(n.trees.1 == loop[i,"n.trees.1"] & n.trees.2 == loop[i,"n.trees.2"]))
  }
  list(loop=loop, submodels=submodels)
}

lpHurdleGBM$loop <- hurdleGBM_loop_trees


#### Try out our custom model

library(caret)


#Read data and format factors
data <- read.csv("Datafiles/data.national.6.5.csv")
data$lith.pred <- factor(data$lith.pred)
data$Primary.rocktype <- factor(data$Primary.rocktype)
data$Secondary.rocktype <- factor(data$Secondary.rocktype)
data$usda_tex <- factor(data$usda_tex)
#PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
data <- data[!is.na(data$IV),]
data.PA <- data[,!colnames(data) %in% c("IV","PlotID","predictedClass","Present","Absent")] #remove unneeded variables
data.PA$pres <- ifelse(data.PA$pres=="Present",1,0)
data <- data[,!colnames(data) %in% c("pres","PlotID","predictedClass","Present","Absent")] #remove unneeded variables
data <- data[,c(8,1:7,9:length(data))] #reorder to put responses first




#Split 80/20 into training and test sets
set.seed(13537)
intrain <- createDataPartition(y=data$IV,p=0.8,list=FALSE)
data.train <- data[intrain,]
#data.train.c <- data.train[complete.cases(data.train),]
#data.train.nonzero <- subset(data.train,IV>0)
data.test <-  data[-intrain,]

#Set training controls
#ctrlPar <- trainControl(method='repeatedcv', repeats=5,allowParallel=TRUE,classProbs=TRUE,summaryFunction=twoClassSummary)
ctrlParFast <- trainControl(method='cv',number=5,allowParallel=T)
ctrlParVeryFast <- trainControl(method='cv',number=2,allowParallel=T)
adaptiveFastRand <- trainControl(method='adaptive_cv',number=5,repeats=1,
                                 adaptive = list(min=2,alpha=0.05,method='gls',
                                                 complete=FALSE),allowParallel=T,
                                 search="random", returnData=F)
fastRand <- trainControl(method='cv',number=5,allowParallel=T,search="random",returnData=F,
                         verboseIter=T)


#Set up custom SMOTE sampling (takes our continous response, and uses synthetic sampling to reduce number of 0's)
#smote_cust <- list(name = "SMOTE that takes a continuous variable and reduces 0's",
#                func = function (x, y) {
#                  library(DMwR)
#                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
#                  dat$.y0 <- y
#                  dat$.yFact <- ifelse(y>0,"Present","Absent") 
#                  dat_new <- SMOTE(.yFact ~ ., data = dat, k = 5)
#                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
#                       y = dat$.y)
#                },
#                first = TRUE)

#Set parameter search grid for tuning
hurdleGBMGrid <-  expand.grid(interaction.depth.1 =c(12,15),
                              n.trees.1 = c(300),
                              shrinkage.1 = c(0.001),
                              n.minobsinnode.1 = c(25),
                              interaction.depth.2 =c(6),
                              n.trees.2 = c(200*c(1)),
                              shrinkage.2 = c(0.001),
                              n.minobsinnode.2 = c(7),
                              threshold=c(.6))

#Test our loop functions on the grid
hurdleGBM_loop(hurdleGBMGrid)
hurdleGBM_loop2(hurdleGBMGrid)

#Test just the fit function
param_test <- data.frame(interaction.depth.1 = 5,n.trees.1 = 20,shrinkage.1 = 0.01, n.minobsinnode.1 = 5,interaction.depth.2 =5,
                         n.trees.2 = 20,shrinkage.2 = 0.001, n.minobsinnode.2 = 15,threshold=0)

testHurdle <- hurdleGBM_Fit(data.train[,-1], data.train[,1], param=param_test, lev=NULL, last=TRUE, classProbs=FALSE) 
predictedTest <- hurdleGBM_Predict(testHurdle,newdata=data.test[,-1])

gbmTest <- gbm(pres~.,data=data.PA,interaction.depth=5,n.trees=20,shrinkage=.01,n.minobsinnode=5)
testPredict <- predict.gbm(gbmTest,newdata=data.PA[,-1],n.trees=20,type="response")



## Set up parallel processing
library(doParallel)
max <- detectCores()
#c1 <- makeCluster(round(max*.5))
c1 <- makeCluster(12)
registerDoParallel(c1)



#Fit Hurdle Model with GBMs - Best: Depth.1 = 19, ntrees.1 = 1400, nodes.1 = 7, thresh = 0.15
#                                   Depth.2 = 25, n.trees.2 = 3300, nodes.2 = 23, shrinkage = 0.001
#                             RMSE = 3.77, rsq = 0.069 (CV values)
#             
#
#                           New Best: Depth.1 = 5, n.trees.1 = 2900, nodes.1=3, thresh=.1
#                                     Depth.2 = 4, n.trees.2 = 3000, nodes.2=5, shrink=0.001(?)
#                              RMSE= 3.624, rsq = 0.067 (CV values)
#
#
#using grid
#set.seed(15325)
#ptm <- proc.time()
#hurdleTune <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
#                trControl=ctrlTest,tuneGrid=hurdleGBMGrid)
#proc.time() - ptm


#using random sampling - NOTE - length 300 used up server memory
set.seed(15325)
#ptm <- proc.time()
hurdleTune <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
                    trControl=fastRand,tuneLength=2)
#proc.time() - ptm

saveRDS(hurdleTune,file="Objects/HurdleModels/5_3_17_rand.rds")
hurdleTune
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()

ggplot(hurdleTune)

#Performance on CV training (best model)
getTrainPerf(hurdleTune)

#compare to last best model
hurdleTuneOld <- readRDS(file="Objects/HurdleModels/4_22_17_rand.rds")
getTrainPerf(hurdleTuneOld)
resamps <- resamples(list(New = hurdleTune, Old = hurdleTuneOld))
summary(resamps)
trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")

#compare to null model 
null_mod<- nullModel(y=data.train$IV)
null_predict <- predict(null_mod,newdata=data.train)
postResample(predict(null_mod,newdata=data.train),data.train$IV)
null_predict_test <- predict(null_mod,newdata=data.test)
postResample(null_predict_test,data.test$IV)


#Full train set RMSE for best model
data.train.predict <- predict(hurdleTune,newdata=data.train,na.action=na.pass)
RMSE <- caret::postResample(data.train.predict,data.train$IV)
RMSE


#Test set predictions for best model
data.test.predict <- predict(hurdleTune,newdata=data.test,na.action=na.pass)
RMSE <- caret::postResample(data.test.predict,data.test$IV)
RMSE

#full predictions
predicted.IV <- predict(hurdleTune,newdata=data,na.action=na.pass)
data.predict.hurdle <- cbind(data,predicted.IV)
RMSE <- caret::postResample(data.predict.hurdle$predicted.IV,data$IV)
RMSE


#plot predicted IV values
usa <- getData('GADM' , country="USA", level=1)

prob.spatial <- data.predict.hurdle[,c("LON","LAT","predicted.IV")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)
plot(x,zlim=c(0,10),interpolate=T)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
#points(data.predict[,"LON"],data.predict[,"LAT"],cex=0.1)


#plot actual IV values for all FIA 
prob.spatial <- data.predict.hurdle[,c("LON","LAT","IV")]
#prob.spatial$IV[prob.spatial$IV>30] <- 30 #Set very high values to 30 so we can have similar scale to predicted values
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)  
plot(x,zlim=c(0,10),interpolate=T)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
#points(data.predict[,"LON"],data.predict[,"LAT"],cex=0.1)

#plot prob present/absent

predicted.PA <- predict(hurdleTune$finalModel,newdata=data.PA,na.action=na.pass,n.trees=3250,type="response")
data.PA.predict <- cbind(data.PA,predicted.PA)
prob.spatial <- data.PA.predict[,c("LON","LAT","predicted.PA")]
names(prob.spatial) <- c("x","y","z")
e <- extent(prob.spatial[,1:2])
r <- raster(e,ncol=130,100)
x <- rasterize(prob.spatial[,1:2],r,prob.spatial[,3],fun=mean)  
plot(x,zlim=c(0,1),interpolate=T)
plot(usa,xlim=c(-90,-70),ylim=(c(25,50)),axes=TRUE,add=T)
points(LAT~LON,data=subset(data.PA,pres==1),pch=3,cex=.1)
points(LAT~LON,data=subset(data,IV>95),pch=3,cex=.1)


#############Try parameter optimization with Bayesian Optimization

hurdleFit_bayes <- function(interaction.depth.1, threshold,
                            interaction.depth.2) {
  set.seed(15325)
  n.trees.1 <- 5000
  n.trees.2 <- 3000
  shrinkage.1 <- 0.001
  shrinkage.2 <- 0.001
  n.minobsinnode.1 <- 5
  n.minobsinnode.2 <- 5
  max <- detectCores()
  #c1 <- makeCluster(10)
  #registerDoParallel(c1)
  model <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
                 trControl=ctrlParFast, tuneGrid = data.frame(interaction.depth.1, n.trees.1, shrinkage.1, n.minobsinnode.1, threshold,
                                                              interaction.depth.2, n.trees.2, shrinkage.2, n.minobsinnode.2))
  #stopCluster(c1)
  #registerDoParallel()
  list(Score = -getTrainPerf(model)[, "TrainRMSE"], Pred = 0)
  
}


## Define the bounds of the search
bounds <- list(interaction.depth.1 = c(1L,34L),
               threshold = c(0,1),
               interaction.depth.2 = c(1L,34L))

#create grid of values already examined 
#initial_grid <- hurdleTune$results[, c("interaction.depth.1", "n.trees.1", "shrinkage.1", "n.minobsinnode.1", "threshold",
#                                       "interaction.depth.2", "n.trees.2", "shrinkage.2", "n.minobsinnode.2","RMSE")]
#initial_grid$RMSE <- -initial_grid$RMSE
#names(initial_grid)[10] <- "Value"


#max <- detectCores()
c1 <- makeCluster(12)
registerDoParallel(c1)

library(rBayesianOptimization)
set.seed(140)
bayes_search3 <- BayesianOptimization(hurdleFit_bayes,bounds=bounds, init_grid_dt=bayes_search2$History[,-1], #    init_points = 10,
                                  n_iter=30,acq="ucb", kappa=1, eps=0.0, verbose=T,
                                     kernel=list(type="matern", nu=5/2))
saveRDS(bayes_search3,file="Objects/HurdleModels/5_4_17_bayes_2.rds")
stopCluster(c1)
registerDoParallel()

### Best so far: depth 1 = 2, thresh = .37709, depth 2 = 9 , RMSE = -3.6845
bayes_search3
str(bayes_search)



#######Fit the model chosen by bayes
bayesGrid <-  expand.grid(interaction.depth.1 =c(2),
                              n.trees.1 = c(3500),
                              shrinkage.1 = c(0.001),
                              n.minobsinnode.1 = c(3,5),
                              interaction.depth.2 =c(9),
                              n.trees.2 = c(3500),
                              shrinkage.2 = c(0.001),
                              n.minobsinnode.2 = c(5),
                              threshold=c(.3771))
c1 <- makeCluster(12)
registerDoParallel(c1)
set.seed(140)
bayesTune <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
                    trControl=ctrlParVeryFast,tuneGrid=bayesGrid)
#proc.time() - ptm

saveRDS(bayesTune,file="Objects/HurdleModels/5_4_17_bayesBest.rds")
bayesTune
## STOP Parallel and restart
stopCluster(c1)
registerDoParallel()
getTrainPerf(bayesTune)
ggplot(bayesTune)


####### Take best bayes model 





####### Step 1: random grid searches


rand_search <- foreach(i=1:60, .combine='rbind') %do% {
  c1 <- makeCluster(15)
  registerDoParallel(c1)
  set.seed(i*412)
  trainIter <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
        trControl=fastRand,tuneLength=5)
  stopCluster(c1)
  registerDoParallel()
  trainIter$results
}
best <- rand_search[which.min(rand_search$RMSE),]
saveRDS(rand_search,file="Objects/HurdleModels/5_12_17_rand.rds")

####Best so far: RMSE 3.684, trees.1 = 3250, depth.1 = 6, threshold=.27, trees.2 = 2000, depth.2 = 20, min in node 1&2 = 5
####             RMSE 3.679, trees.1 = 5000, depth.1 = 10, threshold = 0.02, n.trees.2 = 2750, depth.2 = 23

bestGrid <-  expand.grid(interaction.depth.1 =c(10),
                          n.trees.1 = c(5500),
                          shrinkage.1 = c(0.001),
                          n.minobsinnode.1 = c(5),
                          interaction.depth.2 =c(23),
                          n.trees.2 = c(2750),
                          shrinkage.2 = c(0.001),
                          n.minobsinnode.2 = c(3,5),
                          threshold=c(.02))
c1 <- makeCluster(12)
registerDoParallel(c1)
set.seed(140)
bestRandTune <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
                   trControl=ctrlParFast,tuneGrid=bestGrid)
stopCluster(c1)
registerDoParallel()
getTrainPerf(bestRandTune)

#Test set predictions for best model chosen by random search
data.test.predict <- predict(bestRandTune,newdata=data.test,na.action=na.pass)
RMSE <- caret::postResample(data.test.predict,data.test$IV)
RMSE # RMSE 3.6701


####### Step 2: bayesian optimization

#Convert the results of our random search into something bayesianOptimization by 
# collapsing all of our submodels into one main model showing max trees and the best 
# submodel RMSE

collapse_length <- length(seq(1000,5000,by=250))*length(seq(1000,3500,by=250)) #calculate how many submodels in each main model
rand_search_collapse <- rand_search[with(rand_search,order(interaction.depth.1,interaction.depth.2,threshold,n.trees.1,n.trees.2)),]
best_submodel_RMSE <- vector()
for(i in seq(collapse_length,nrow(rand_search_collapse),by=collapse_length)){
  best_submodel_RMSE[i/collapse_length] <- min(rand_search_collapse$RMSE[(i-collapse_length+1):i])
}
rand_search_bayes <- rand_search_collapse[seq(collapse_length,nrow(rand_search_collapse),by=collapse_length),c("interaction.depth.1","threshold","interaction.depth.2","RMSE")]
rand_search_bayes$RMSE <- -1*best_submodel_RMSE #Make negative for bayesOptimization function
colnames(rand_search_bayes)[4]<- "Value"
rand_search_bayes <- rand_search_bayes[!is.na(rand_search_bayes$Value),]
library(rBayesianOptimization)
set.seed(8234)
bayes_search <- BayesianOptimization(hurdleFit_bayes,bounds=bounds, init_grid_dt=rand_search_bayes, #    init_points = 10,
                                      n_iter=20,acq="ucb", kappa=1, eps=0.0, verbose=T,
                                      kernel=list(type="matern", nu=5/2))
saveRDS(bayes_search3,file="Objects/HurdleModels/5_12_17_bayes.rds")


#bayes gets stuck/converges on depth.1 = 1, thresh = 0.100, interaction.depth.2 = 25

#best found by bayes: 
#    RMSE = 3.6747  depth.1 = 1, threshold = .1618, depth.2 = 25
#    Note: should try bayes optim allowing depth > 25 and thresh < .1


#best found by bayes round 2 (all parameter values allowed)
#    RMSE = 3.6791  depth.1 = X, threshold = 0, depth.2 = 34
#

bestGrid2 <-  expand.grid(interaction.depth.1 =c(1),
                         n.trees.1 = c(4000),
                         shrinkage.1 = c(0.001),
                         n.minobsinnode.1 = c(5),
                         interaction.depth.2 =c(25,26),
                         n.trees.2 = c(3500),
                         shrinkage.2 = c(0.001),
                         n.minobsinnode.2 = c(5),
                         threshold=c(.1618))
c1 <- makeCluster(12)
registerDoParallel(c1)
set.seed(140)
bestTune2 <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
                   trControl=ctrlParFast,tuneGrid=bestGrid2)
stopCluster(c1)
registerDoParallel()
getTrainPerf(bestTune2)


#Full train set RMSE for best model chosen by bayes optim
data.train.predict <- predict(bestTune2,newdata=data.train,na.action=na.pass)
RMSE <- caret::postResample(data.train.predict,data.train$IV)
RMSE


#Test set predictions for best model chosen by bayes optim
data.test.predict <- predict(bestTune2,newdata=data.test,na.action=na.pass)
RMSE <- caret::postResample(data.test.predict,data.test$IV)
RMSE # 3.6546

#full predictions for bset model chosen by bayes optim
predicted.IV <- predict(bestTune2,newdata=data,na.action=na.pass)
data.predict.hurdle <- cbind(data,predicted.IV)
RMSE <- caret::postResample(data.predict.hurdle$predicted.IV,data$IV)
RMSE


#compare to other models
hurdleTuneOld <- readRDS(file="Objects/HurdleModels/4_22_17_rand.rds")
getTrainPerf(hurdleTuneOld)
resamps <- resamples(list(BayesOptim = bestTune2, Random = bestRandTune))
summary(resamps)
trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")


#### Fit best model found by bayes with all parameter values allowed

bestGrid3 <-  expand.grid(interaction.depth.1 =c(1),
                          n.trees.1 = c(1000,1250),
                          shrinkage.1 = c(0.001),
                          n.minobsinnode.1 = c(5),
                          interaction.depth.2 =c(34),
                          n.trees.2 = c(3500),
                          shrinkage.2 = c(0.001),
                          n.minobsinnode.2 = c(5),
                          threshold=c(0))
c1 <- makeCluster(12)
registerDoParallel(c1)
set.seed(140)
bestTune3 <- train(data.train[,-1],data.train[,1],method=lpHurdleGBM,
                   trControl=ctrlParFast,tuneGrid=bestGrid3)
stopCluster(c1)
registerDoParallel()
getTrainPerf(bestTune3)



#Full train set RMSE for best model chosen by bayes optim
data.train.predict <- predict(bestTune3,newdata=data.train,na.action=na.pass)
RMSE <- caret::postResample(data.train.predict,data.train$IV)
RMSE


#Test set predictions for best model chosen by bayes optim
data.test.predict <- predict(bestTune3,newdata=data.test,na.action=na.pass)
RMSE <- caret::postResample(data.test.predict,data.test$IV)
RMSE # 3.6582

#full predictions for bset model chosen by bayes optim
predicted.IV <- predict(bestTune3,newdata=data,na.action=na.pass)
data.predict.hurdle <- cbind(data,predicted.IV)
RMSE <- caret::postResample(data.predict.hurdle$predicted.IV,data$IV)
RMSE


#compare to other models
hurdleTuneOld <- readRDS(file="Objects/HurdleModels/4_22_17_rand.rds")
getTrainPerf(hurdleTuneOld)
resamps <- resamples(list(BayesOptim_const = bestTune2, BayesOptim_expanded = bestTune3, Random = bestRandTune))
summary(resamps)
trellis.par.set(caretTheme())
dotplot(resamps, metric = "RMSE")






##############################################

######
## Try geoGAM package fitting sparse geospatial boosted GAM and compare to hurdle-GBM result
## Note: not a hurdle model
#####

library(geoGAM)



#Read and format data
data.gam <- read.csv("Datafiles/data.national.6.5.csv")
data.gam$lith.pred <- factor(data.gam$lith.pred)
data.gam$Primary.rocktype <- factor(data.gam$Primary.rocktype)
data.gam$Secondary.rocktype <- factor(data.gam$Secondary.rocktype)
data.gam$usda_tex <- factor(data.gam$usda_tex)
#PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
data.gam <- data.gam[!is.na(data.gam$IV),]
data.gam.PA <- data.gam[,!colnames(data.gam) %in% c("IV","PlotID","predictedClass","Present","Absent")] #remove unneeded variables
data.gam.PA$pres <- ifelse(data.gam.PA$pres=="Present",1,0)
data.gam <- data.gam[,!colnames(data.gam) %in% c("pres","PlotID","predictedClass","Present","Absent","asp_val")] #remove unneeded variables
data.gam <- data.gam[complete.cases(data.gam),c(7,1:5,8:26,28,31,32,34,6,27,29,30,33)] #reorder to put response first, then spatial vars, then continuous then factors
data.gam <- data.gam[data.gam$usda_tex != 13,]
data.gam$usda_tex <- factor(data.gam$usda_tex)
colnames(data.gam)[c(2,3)]<- c("x","y")
#add two-way interactions of continuous variables
data.gam_cont <- data.gam[,7:29]
data.gam_inter <- model.matrix(~.^2-1,data.gam_cont)
colnames(data.gam_inter) <- gsub(":","x",colnames(data.gam_inter))

data.gam2 <- cbind(data.gam[,c(1:6,30:ncol(data.gam))],data.gam_inter)  
data.gam2 <- cbind(data.gam[,c(1:6,30:ncol(data.gam))],data.gam_inter[,1:30])  #for testing purposes




#Split 80/20 into training and test sets
set.seed(13537)
intrain <- createDataPartition(y=data.gam$IV,p=0.8,list=FALSE)
data.gam.train <- data.gam[intrain,]
#data.train.c <- data.train[complete.cases(data.train),]
#data.train.nonzero <- subset(data.train,IV>0)
data.gam.test <-  data.gam[-intrain,]


#Split 80/20 into training and test sets
set.seed(13537)
intrain <- createDataPartition(y=data.gam2$IV,p=0.8,list=FALSE)
data.gam2.train <- data.gam2[intrain,]
#data.train.c <- data.train[complete.cases(data.train),]
#data.train.nonzero <- subset(data.train,IV>0)
data.gam2.test <-  data.gam2[-intrain,]



#Fit model
c1 <- makeCluster(15)
registerDoParallel(c1)


gam <- geoGAM(response="IV", covariates=names(data.gam.train)[4:ncol(data.gam.train)], data=data.gam.train,
              coords=c("x","y"), non.stationary=T, seed=15574,
               verbose=1, cores=15,validation.data=data.gam.test)
saveRDS(gam,"Objects/geoGam_5.19_17_v2.rds")


stopCluster(c1)
registerDoParallel()

summary(gam)
summary(gam,what="path")


#
# non-stationary interactions only - CV RMSE = 3.67997  r2 = 0.0398
#                                    Test RMSE = 3.924  r2 = 0.0488 - ??way worse than null model? (RMSE 3.7985)
#
# non-stationary + a few interactive terms... CV = 3.67287 r2 = 0.0435
#                                      Test RMSE = 3.92111r2 = 0.05019
###


#data.gam2.test.x <- data.gam2.test
#data.gam2.test.x$LAT.1 <- data.gam2.test.x$LAT
predicted <- predict(gam$gamboost,newdata=data.gam.test,type="response")
predicted <- geoGAM::predict.geoGAM(gam,newdata=data.gam.train,type="response")
RMSE <- caret::postResample(predicted,data.gam.train$IV)
RMSE # 


#Test set predictions
#x <- predict(gam,type="response")
gam.test.predict <-predict.geoGAM(gam,newdata=data.gam.test,type="response")
RMSE <- caret::postResample(gam$gam.final.extern$pred,gam$gam.final.extern$dat)
RMSE # 3.9211

#compare to null model 
null_mod<- nullModel(y=data.gam2.train$IV)
#null_predict <- predict(null_mod,newdata=data.gam2train)
#postResample(predict(null_mod,newdata=data.train),data.train$IV)
null_predict_test <- predict(null_mod,newdata=data.gam2.test)
postResample(null_predict_test,data.gam2.test$IV) # RMSE = 4.023

#Full data set
#predicted.IV <- predict(gam,)


predict.geoGAM <- function( object,
                            newdata,
                            type = c("response", "link", "probs", "class"),
                            back.transform = c("none", "log", "sqrt"),
                            threshold = 0.5,
                            se.fit = F, ...) {
  
  
  
  type <- match.arg(type)
  back.transform <- match.arg(back.transform)
  
  if( object$parameters$family[[2]] == "Gaussian" & type %in% c("probs", "class") ){
    warning( cat( "Predictions of type = '",
                  type,"' not possible for continuous response. \nDoing predictions for type = 'response' ..", sep = "") ) }
  
  
  # Transformed calibration data
  daten <-  object$gam.final$model[, -match("(weights)", names(object$gam.final$model)), drop = F ]
  
  l.fact <- gsub("ag$", "", names(daten[,-1,drop = F])[ unlist( lapply( daten[,-1, drop = F], is.factor) )  ] )
  l.no.fact <- names(daten[,-1,drop = F])[ !unlist( lapply( daten[,-1, drop = F], is.factor) )  ]
  
  newdata <- newdata[ , c(object$parameters$coords, l.fact, l.no.fact) , drop = F]
  
  ## Transform the data like the original data set
  # Scale numerical data to [0;1], then center (- mean), but not the response
  # Use means / max-min as in calibration data set!
  l.sub.fact <- l.sub.fact.1 <- names(newdata) %in% l.fact
  names(l.sub.fact) <- names(newdata)
  l.sub.fact[ names(l.sub.fact) %in% c( object$parameters$response, object$parameters$coords, "(weights)") ] <- TRUE
  
  fun.center.v <- function(col, vali, kali, daten ){
    
    x.c <- ( vali[, col] - min(kali[, col]) ) /
      ( max(kali[, col]) - min(kali[, col]) )
    
    # Mean from [0,1]-column from calibration-data set
    x.kali.m <- ( kali[, col] - min(kali[, col]) ) /
      ( max(kali[, col]) - min(kali[, col]))
    
    # center with this mean
    x.sc <- scale(x.c, center = mean( x.kali.m ), scale = F)[, 1]
    return(x.sc)
  }
  
  if( length(l.sub.fact) > 0){
    newdata[, !l.sub.fact]  <- data.frame(
      lapply( names(newdata[, !l.sub.fact, drop = F]), FUN = fun.center.v,
              daten = daten, vali = newdata, kali = object$data) )
  }
  
  # Center coordinates with mean from calibration data
  if( !is.null(object$parameters$coords)){
    coord.means <-  colMeans( object$data[ , object$parameters$coords])
    newdata[, object$parameters$coords] <- data.frame(
      scale( newdata[, object$parameters$coords], center = coord.means, scale = F))
  }
  
  # Add intercept (and an alibi SE)
  newdata$int <- newdata$se <- rep(1, nrow(newdata) )
  
  # Add renamed factors for main effect
  if( length(l.sub.fact.1) > 0){
    newdata[ , paste( names( l.sub.fact.1 )[ l.sub.fact.1 ], "ag", sep = "") ] <-
      newdata[ , paste( names( l.sub.fact.1 )[ l.sub.fact.1 ] ) ]
  }
  
  ##  Aggregate factors accordingly
  l.factors <- names( daten[,-1, drop = F] )[ unlist( lapply( daten[, -1, drop = F], is.factor ) ) ]
  
  for( fact in l.factors ){
    
    if( fact %in% names(newdata) ){
      ii.new.fact <- which( names(newdata) == fact )
      fact.nag <- fact
    } else {
      fact.nag <- gsub("ag$", "", fact)
      ii.new.fact <- which( names(newdata) == fact.nag )
      # Add, if renamed factor
      newdata[, fact ] <- newdata[ , ii.new.fact ]
    }
    
    n.lev.cal <- length( lev.cal <- levels( daten[, fact] ) )
    n.lev.val <- length( lev.val <- levels( newdata[, fact.nag ]) )
    
    if( sum( !lev.val %in% lev.cal ) > 1){
      # Aggregate each level stepwise
      for( ii in 1:n.lev.val ){
        levels(newdata[, fact ])[ match( c( strsplit( lev.cal[ii], "--" )[[1]] ),
                                         levels( newdata[, fact ]) )]  <- lev.cal[ii]
      }
    }
  }
  
  
  se.fit <- ifelse( back.transform != "none", T, se.fit)
  type.pr <- ifelse( type %in% c("probs", "class"), "response", type)
  
  # normal untranformed Gaussian prediction
  pred <- predict( object$gam.final, newdata = newdata, type = type.pr, se.fit = se.fit)
  
  
  # Back transform LOG
  # approximation of unbiased backtransform using Var(XBeta) from GAM model
  # (true var(XBeta) probably larger, because model selection is ignored)
  if( back.transform == "log" ){
    
    pred <- exp(pred$fit - pred$se.fit*0.5)
    
    
    # Backtransformation of SQRT
    # same approximation with Var(XBeta) from GAM object
  } else if( back.transform == "sqrt" ) {
    
    pred <- pred$fit^2 - pred$se.fit
    
    
    # for ordinal / binomial regression
    # create "category numbers" for integer raster
  } else if( object$parameters$family[[2]] == "Binomial" & type == "class" ) {
    
    
    # optimal threshold for binarisation
    # see line 220 in 4_geoam_presentations_diverse_zhg.R
    pred <- as.integer( ifelse( pred > threshold, 1, 0) )
    
    
  } else if( object$parameters$family[[2]] == "PropOdds" & type == "class" ){
    
    pred <- as.integer( apply( pred,1, function(x){ min( which( cumsum(x) >= 0.5 ) ) } ) )
    
    if( is.factor( object$data[, object$parameters$response ] ) ){
      
      pred <- factor( pred, levels = sort(unique(pred)), labels = levels( object$data[, object$parameters$response ]))
    }
    
  }
  
  return(pred)
}
