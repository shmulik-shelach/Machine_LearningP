# Machine_LearningP
Machine learning project

PML_project.R
owner

Sun Jan 25 16:52:57 2015

ML_Project <- function()
{
TrainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

Train_PML <- read.csv(TrainURL, na.strings = c("NA", ""))

#remove NAs

Train_PML <- Train_PML[, !colSums(is.na(Train_PML)), drop = FALSE]

#cleaning the data from Zero and Near Zero Variance Predictors.

NZV <- nearZeroVar(Train_PML)

Train_PML <- Train_PML[,-NZV]


#and remove some variables that seem unrelavent - X, user_name, cvtd_timestamp 
#(but not all, since Random Forest is problematic with overfitting )

Train_PML <- Train_PML[,-c(1,2,5)]

#That will leave the sample with 56 Predictors. 
#Will have to use the original training set (after the cleaning process) 
#for Cross validation as well

set.seed (2015)

trainIndex <- createDataPartition(Train_PML$classe, p=0.5, list=FALSE)
trainingSet <- Train_PML[trainIndex,]
testSet <- Train_PML[-trainIndex,] 

......

#The machine algoritem to start with here seems like Random Forest, 
#which is relatively easy to implement, and can deal with 
#large number of variables without specific knowledge about their importance
#Random forest is capable of regression and classification 
#and it's helpful for estimating which variables are important.
ctrl = trainControl(method = "oob", number = 3, verboseIter = TRUE)

modelFit <- train (trainingSet$classe ~., data = trainingSet, method="rf",
                   prof=TRUE, trControl = ctrl)

#to get the accuracy/training error rate of the model we examine the results    

modelFit$results 
#=>  Accuracy of 0.9975540

modelFit$finalModel
#=> OOB estimate of  error rate: 0.22%  


#Runing the model with the CV sample in order to assess the the model accuracy
# and find the error level for out of the sample

Prediction <- predict(modelFit, testSet)

cfmx <- confusionMatrix(Prediction, testSet$classe)

cfmx$overall 
#=> Accuracy of the tested model = 0.9975 with confidence intervals 0.9962, 0.9984,
#while the error rate is 0.254% (a bit larger than th trained model)

#Discussion

varImp (modelFit)
#=> the importance of the variables can be obtained from "varImp", with the 5
#most important variables are from the Belt, Forearm and the Dumbbell (not the Glove)
#it seems rational since the classes tested mistakes like:
#"throwing the hips to the front (Class E)", or "throwing the elbows to the 
#front (Class B)", which are connected directly with the sensors on the belt (E)
#and the forearm (B)


}
