#############
# Use Mboost..... and caret?
# Still a work in progress...
#############



library(geoGAM)
library(mboost)


#Read and format data
data.gam <- read.csv("Datafiles/data.national.6.5.csv")
data.gam$lith.pred <- factor(data.gam$lith.pred)
data.gam$Primary.rocktype <- factor(data.gam$Primary.rocktype)
data.gam$Secondary.rocktype <- factor(data.gam$Secondary.rocktype)
data.gam$usda_tex <- factor(data.gam$usda_tex)
#PA$pres <- factor(PA$pres,levels=c("Present",'Absent'))
data.gam <- data.gam[!is.na(data.gam$IV),]
data.gam.PA <- data.gam[,!colnames(data.gam) %in% c("IV","PlotID","predictedClass","Present","Absent","asp_val")] #remove unneeded variables
#data.gam.PA$pres <- ifelse(data.gam.PA$pres=="Present",1,0)
#data.gam.PA <- data.gam.PA[,!colnames(data.gam.PA) %in% c("pres","PlotID","predictedClass","Present","Absent","asp_val")] #remove unneeded variables
data.gam.PA <- data.gam.PA[complete.cases(data.gam.PA),c(6,1:5,8:26,28,31,32,34,7,27,29,30,33)] #reorder to put response first, then spatial vars, then continuous then factors
data.gam.PA <- data.gam.PA[data.gam.PA$usda_tex != 13,]
colnames(data.gam.PA)[c(2,3)]<- c("x","y")
data.gam.PA$usda_tex <- factor(data.gam.PA$usda_tex)

#data.gam <- data.gam[,!colnames(data.gam) %in% c("pres","PlotID","predictedClass","Present","Absent","asp_val")] #remove unneeded variables
#data.gam <- data.gam[complete.cases(data.gam),c(7,1:5,8:26,28,31,32,34,6,27,29,30,33)] #reorder to put response first, then spatial vars, then continuous then factors
#data.gam <- data.gam[data.gam$usda_tex != 13,]
#colnames(data.gam)[c(2,3)]<- c("x","y")
#data.gam$usda_tex <- factor(data.gam$usda_tex)
#add two-way interactions of continuous variables
#data.gam_cont <- data.gam[,7:29]
#data.gam_inter <- model.matrix(~.^2-1,data.gam_cont)
#colnames(data.gam_inter) <- gsub(":","x",colnames(data.gam_inter))

#data.gam2 <- cbind(data.gam[,c(1:6,30:ncol(data.gam))],data.gam_inter)  
#data.gam2 <- cbind(data.gam[,c(1:6,30:ncol(data.gam))],data.gam_inter[,1:30])  #for testing purposes




#Split 80/20 into training and test sets
set.seed(13537)
intrain <- createDataPartition(y=data.gam.PA$pres,p=0.8,list=FALSE)
data.gam.PA.train <- data.gam.PA[intrain,]
#data.train.c <- data.train[complete.cases(data.train),]
#data.train.nonzero <- subset(data.train,IV>0)
data.gam.PA.test <-  data.gam.PA[-intrain,]


#Split 80/20 into training and test sets
#set.seed(13537)
#intrain <- createDataPartition(y=data.gam.PA2$IV,p=0.8,list=FALSE)
#data.gam.PA2.train <- data.gam.PA2[intrain,]
#data.train.c <- data.train[complete.cases(data.train),]
#data.train.nonzero <- subset(data.train,IV>0)
#data.gam.PA2.test <-  data.gam.PA2[-intrain,]




#### Create formula
makeMboostForm <- function(df){
  resp <- paste(colnames(df)[1],"~")
  names <- colnames(df)
  coords <- c("x","y")
  l.fact <- names(df)[ unlist( lapply( df, is.factor) ) == TRUE ]
  factors <- grep( paste0("^", resp, "$"), l.fact, invert = T, value = T)
  factors <- factors[-which(factors=="pres")]
  print(factors)
  nums <- names[-which(names %in% c(colnames(df)[1],coords,factors))]
  
  #Make splines
  form.bbs <- paste( "bbs(",
                      paste(nums,
                        collapse = paste(", center = T, df = 1)", " + bbs("
                        )),
                      ", center = T, df = 1)",
                      sep = ""
  )
  
  #Make spatial
  form.spatial <- "bspatial(x,y,df=6,knots=12)"
  
  #Make nonstationary
  form.nonspatial <- paste( "bspatial(", paste(coords, collapse = ","), ", by=",
                       paste(nums,
                         collapse = paste(
                           ", df=6, knots = 12) ", " + bspatial(", paste(coords, collapse = ","), ", by=" )
                       ),
                       ", df=6, knots = 12)",
                       sep = ""
  ) 
 
  
  #make factors - and linear continuous base learners
  form.bols <- paste( "bols(",
                    paste(c(factors,nums),
                          collapse = paste(", intercept = T, df = 1)", " + bols("
                          )),
                    ", intercept = T, df = 1)",
                    sep = "")
  
  return(paste(resp,paste(form.bbs,form.spatial,form.bols,form.nonspatial,sep="+")))


}

form <- makeMboostForm(data.gam.PA.train)
form
form <- as.formula(form) 
#form = "pres ~ bbs(STDAGE,center=T,df=1)+bspatial(x,y,df=1)"
#form[[2]]

m1 <- gamboost(formula=as.formula(makeMboostForm(data.gam.PA.train)),data=data.gam.PA.train,family=Binomial(),control=boost_control(mstop=700))

c1 <- makeCluster(round(detectCores()*.7))
registerDoParallel(c1)
cv <- cvrisk(m1)
stopCluster(c1)
registerDoParallel()
plot(cv)
finalModel <- m1[mstop(cv)]
summary(finalModel)
#model <- gamboost()



#Test set performance
predicted.PA.test <- predict(finalModel,newdata=data.gam.PA.test,type="response",na.action=na.pass) #predict on test set
roc.obj <- roc(ifelse(data.gam.PA.test$pres == "Present",1,0),pred=predicted.PA.test)
auc(roc.obj)


predicted.PA.train <- predict(finalModel,newdata=data.gam.PA.train,type="response",na.action=na.pass) #predict on test set
roc.obj <- roc(ifelse(data.gam.PA.train$pres == "Present",1,0),pred=predicted.PA.train)
auc(roc.obj)



############

#basic logistic regression w/smote- for ROC AUC comparison
ctrlPar <- trainControl(method='repeatedcv',number=5, repeats=5,allowParallel=TRUE,classProbs=TRUE,summaryFunction=twoClassSummary,sampling="smote")
library(doParallel)

c1 <- makeCluster(round(detectCores()*.5))
registerDoParallel(c1)
set.seed(8081)

logistic <- train(data.gam.PA.train[,-1],data.gam.PA.train[,1],method="glm",family="binomial",
<<<<<<< HEAD
                  trControl=trainControl(method="none"))
=======
                  trControl=ctrlPar)
>>>>>>> 3d521b03936cc3aa15965652269e102e0117c9cb
#logistic <- glm(pres~.,data=data.gam.PA.train,family=binomial)

stopCluster(c1)
registerDoParallel()

<<<<<<< HEAD
predicted.PA.test <- predict(logistic,newdata=data.gam.PA.test,type="prob",na.action=na.pass) #predict on test set
=======
predicted.PA.test <- predict(logistic,newdata=data.gam.PA.test,type="response",na.action=na.pass) #predict on test set
>>>>>>> 3d521b03936cc3aa15965652269e102e0117c9cb
roc.obj <- roc(ifelse(data.gam.PA.test$pres == "Present",1,0),pred=predicted.PA.test,metric="ROC")
auc(roc.obj) #AUC = 0.8922
summary(logistic)

#GAM (not boosted)
library(caret)
c1 <- makeCluster(round(detectCores()*.5))
registerDoParallel(c1)
set.seed(8081)
gam <- train(data.gam.PA.train[,-1],data.gam.PA.train[,1],method="gam",family=binomial,
             trControl=ctrlPar,tuneGrid=expand.grid(select=T,method="GCV.Cp"),metric="ROC")
stopCluster(c1)
registerDoParallel()



###random Forest w/smote - CV AUC = 0.9114
<<<<<<< HEAD
rfGrid <- expand.grid(mtry=c(1:3))
set.seed(8081)
c1 <- makeCluster(round(detectCores()*.5))
registerDoParallel(c1)
rfTune <- train(x=data.gam.PA.train[,-1],y=data.gam.PA.train[,1],method='rf', trControl=ctrlPar,metric="ROC",ntree=200,tuneGrid=rfGrid)
=======
rfGrid <- expand.grid(mtry=c(1:7))
set.seed(8081)
c1 <- makeCluster(round(detectCores()*.5))
registerDoParallel(c1)
rfTune <- train(x=data.gam.PA.train[,-1],y=data.gam.PA.train[,1],method='rf', trControl=ctrlPar,metric="ROC",ntree=600,tuneGrid=rfGrid)
>>>>>>> 3d521b03936cc3aa15965652269e102e0117c9cb
rfTune
ggplot(rfTune)
plot(rfTune$finalModel)
stopCluster(c1)
registerDoParallel()

#test set accuracy
predicted.PA.test <- predict(rfTune,newdata=data.gam.PA.test,type="prob",na.action=na.pass) #predict on test set
roc.obj <- roc(ifelse(data.gam.PA.test$pres == "Present",1,0),pred=predicted.PA.test$Present,metric="ROC")
auc(roc.obj) #AUC = 0.9182
varImp(rfTune)
