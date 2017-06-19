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
data.gam.PA <- data.gam[,!colnames(data.gam) %in% c("IV","PlotID","predictedClass","Present","Absent")] #remove unneeded variables
data.gam.PA$pres <- ifelse(data.gam.PA$pres=="Present",1,0)
data.gam <- data.gam[,!colnames(data.gam) %in% c("pres","PlotID","predictedClass","Present","Absent","asp_val")] #remove unneeded variables
data.gam <- data.gam[complete.cases(data.gam),c(7,1:5,8:26,28,31,32,34,6,27,29,30,33)] #reorder to put response first, then spatial vars, then continuous then factors
data.gam <- data.gam[data.gam$usda_tex != 13,]
data.gam$usda_tex <- factor(data.gam$usda_tex)
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




#### Create formula
makeMboostForm <- function(df){
  names <- colnames(makeMboostForm)
  coords <- c("x","y")
  factors <- names[is.factor(names)]
  nums <- names[-which(names %in% c(spatial,factors))]
  
  #Make splines
  form.bbs <- paste( "bbs(",
                      paste(nums,
                        collapse = paste(", center = T, df = 5)", t.exp, " + bbs("
                        )),
                      ", center = T, df = 5)",
                      sep = ""
  )
  
  #Make spatial
  form.spatial <- "bspatial(x,y,df=5,knots=12)+"
  
  #Make nonstationary
  #form.nonspatial <- paste( "bspatial(", paste(coords, collapse = ","), ", by=",
  #                     paste(nums,
  #                       collapse = paste(
  #                         ", df=5, knots = 12) ", t.exp , " + bspatial(", paste(coords, collapse = ","), ", by=" )
  #                     ),
  #                     ", df=5, knots = 12)", t.exp,
  #                     sep = ""
  #) 
 
  
  #make factors
  form.bols  paste( "bols(",
                    paste(factors,
                          collapse = paste(", intercept = F, df = 5)", t.exp, " + bols("
                          )),
                    ", intercept = F, df = 5)",
                    sep = "")
  return(paste(form.bbs,form.spatial,form.bols,collapse="+"))


}

model <- gamboost()
