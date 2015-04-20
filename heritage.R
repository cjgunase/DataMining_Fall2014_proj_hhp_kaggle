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
dim(train)
dim(test)
#first build models then run eval

#DO NOT MODIFY FROM ABOVE THIS LINE HERE
###new model####


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
fnname <- "PLS_demo1.csv"
write.csv(predictionFile, file=fnname, row.names = FALSE)

############################################################################################################
#######################################Random Forest########################################################

library(randomForest)
newModel <- randomForest(x = train[1:1000,-targindex], y = train[1:1000,targindex], ntree=10)


prediction <- predict(object = newModel, newdata = test[,-targindex], type="response",
                      norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)


prediction <- expm1(prediction)
prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the predictionFile distribution
hist(prediction, breaks=500)

predictionFile <- cbind(memberid,prediction)
colnames(predictionFile) <- c("MemberID","DaysInHospital")
fnname <- "RF_demo1.csv"
write.csv(predictionFile, file=fnname, row.names = FALSE)

# Calculate Importance
importance <- importance(newModel, type=NULL, class=NULL, scale=TRUE)
impSort <- sort(importance, decreasing = TRUE, index.return = TRUE)


############################################################################################################
#######################################Neural net########################################################
library(nnet)
nnetFit <- nnet(train[1:1000,-targindex], train[1:1000,targindex],size = 5,decay = 0.01,linout = TRUE, trace = FALSE,maxit = 500,MaxNWts = 5 * (ncol(train[,-2]) + 1) + 5 + 1)
prediction<-predict(nnetFit,newdata = test[,-targindex])

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
################################################Shivam code############################################################



RMSE(nn,train[,2])
nnt<-predict(nnetFit,-test[,-targindex])
nntest<- cbind(memberid,nnt)
names(nntest)<-c("MemberID","DaysInHospital")
y3.merge <- merge (y3.actual, nntest, by='MemberID')
calc_error(y3.merge$DaysInHospital, y3.merge$DaysInHospital.Prediction)


library(earth)
mars_model<-earth(train[,-targindex],train[,targindex])
mp<-predict(mars_model,train[,-targindex])
RMSE(mp,train[,targindex])
mpt<-predict(mars_model,test[,-targindex])
RMSE(mpt,y3.actual$DaysInHospital)





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

#RF
y3.prediction <- read.csv('RF_demo1.csv')
names(y3.prediction)[2] = 'DaysInHospital.Prediction'

y3.merge <- merge (y3.actual, y3.prediction, by='MemberID')
calc_error(y3.merge$DaysInHospital, y3.merge$DaysInHospital.Prediction)


#NN
y3.prediction <- read.csv('ANN_demo1.csv')
names(y3.prediction)[2] = 'DaysInHospital.Prediction'

y3.merge <- merge (y3.actual, y3.prediction, by='MemberID')
calc_error(y3.merge$DaysInHospital, y3.merge$DaysInHospital.Prediction)
