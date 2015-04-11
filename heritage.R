source("calc_error.R")
alldata <- read.csv("modeling_set1.csv")
colnames(alldata)[1]='MemberID_t'
y3.actual <- read.csv('DaysInHospital_Y3.csv')


#identify train and test data
trainrows <- which(alldata$trainset == 1)
testrows <- which(alldata$trainset == 0)
#get member id of test data
memberid <- alldata[testrows,'MemberID_t']
alldata$MemberID_t <- NULL
alldata$YEAR_t <- NULL
alldata$trainset <- NULL


#convert days in hospital of in training data to log scale
theTarget <- 'DaysInHospital'
alldata[trainrows,theTarget] <- log1p(alldata[trainrows,theTarget]) 
targindex <-  which(names(alldata)==theTarget)

#split the data to train and test set
train<-alldata[trainrows,]
test<-alldata[testrows,]




#DO NOT MODIFY FROM ABOVE THIS LINE HERE

############################################build the GBM model#############################################
library(gbm)
#GBM model settings, these can be varied
GBM_NTREES = 10
GBM_SHRINKAGE = 0.05
GBM_DEPTH = 4
GBM_MINOBS = 50

GBM_model <- gbm.fit(
             x = train[,-targindex]
            ,y = train[,targindex]
            ,distribution = "gaussian"
            ,n.trees = GBM_NTREES
            ,shrinkage = GBM_SHRINKAGE
            ,interaction.depth = GBM_DEPTH
            ,n.minobsinnode = GBM_MINOBS
            ,verbose = TRUE) 

#list variable importance
summary(GBM_model,GBM_NTREES)
#predict
prediction <- predict.gbm(object = GBM_model
                          ,newdata = test[,-targindex]
                          ,GBM_NTREES)


prediction <- expm1(prediction)
prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the predictionFile distribution
hist(prediction, breaks=500)

predictionFile <- cbind(memberid,prediction)
colnames(predictionFile) <- c("MemberID","DaysInHospital")
fnname <- "GBM_demo1.csv"
write.csv(predictionFile, file=fnname, row.names = FALSE)
############################################################################################################

############################################build MARS model################################################
library(earth)
marsFit <- earth(x = train[,-targindex], y = train[,targindex])
prediction<-predict(marsFit,newdata = test[,-targindex])

prediction <- expm1(prediction)
prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the predictionFile distribution
hist(prediction, breaks=500)

predictionFile <- cbind(memberid,prediction)
colnames(predictionFile) <- c("MemberID","DaysInHospital")
fnname <- "MARS_demo1.csv"
write.csv(predictionFile, file=fnname, row.names = FALSE)

############################################################################################################

####################################ANN MODEL###############################################################
library(caret)
ctrl <- trainControl(method = "cv", number = 2)
nnetGrid <- expand.grid(.decay = c(0, 0.1, .1), .size = c(1:2),.bag = FALSE)
nnetTune <- train(train[,-2] ,train[,2],method = "avNNet",tuneGrid = nnetGrid,trControl = ctrl,linout = TRUE,trace = FALSE,MaxNWts = 10 * (ncol(train[,-2]) + 1) + 10 + 1,maxit = 50)
prediction<-predict(nnetTune,newdata = test[,-targindex])

prediction <- expm1(prediction)
prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the predictionFile distribution
hist(prediction, breaks=500)
prediction<-predict(marsFit,newdata = test[,-targindex])
predictionFile <- cbind(memberid,prediction)
colnames(predictionFile) <- c("MemberID","DaysInHospital")
fnname <- "ANN_demo1.csv"
write.csv(predictionFile, file=fnname, row.names = FALSE)
############################################################################################################

####################################PLS MODEL###############################################################
plsTune <- train(train[,-2], train[,2],method = "pls",tuneLength = 20,trControl = ctrl)

prediction <-predict(plsTune,test)
prediction <- expm1(prediction)
prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the predictionFile distribution
hist(prediction, breaks=500)
predictionFile <- cbind(memberid,prediction)
colnames(predictionFile) <- c("MemberID","DaysInHospital")
fnname <- "GBM_demo1.csv"
write.csv(predictionFile, file=fnname, row.names = FALSE)

############################################################################################################
#ADD OTHER MODELS FROM HERE



####################################RMSE EVALUATION########################################################
#GBM
y3.prediction <- read.csv('GBM_demo1.csv')
names(y3.prediction)[2] = 'DaysInHospital.Prediction'

y3.merge <- merge (y3.actual, y3.prediction, by='MemberID')
calc_error(y3.merge$DaysInHospital, y3.merge$DaysInHospital.Prediction)

#mars
y3.prediction <- read.csv('MARS_demo1.csv')
names(y3.prediction)[2] = 'DaysInHospital.Prediction'

y3.merge <- merge (y3.actual, y3.prediction, by='MemberID')
calc_error(y3.merge$DaysInHospital, y3.merge$DaysInHospital.Prediction)
####################
#ADD OTHER MODELS EVALUATION FROM HERE


