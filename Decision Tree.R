library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caret)

Iris <- read.csv("C:/Users/harsh/Downloads/Iris.csv")
head(Iris)
str(Iris)
sapply(Iris, function(x)sum(is.na(x)))

set.seed(12345)
dataT <- sample(1:nrow(Iris), size = nrow(Iris)*0.7, replace = FALSE)
train.IrisLabel <- Iris[dataT,2:6]
test.IrisLabel <- Iris[-dataT,2:6]

decTree <- rpart(train.IrisLabel$Species~., data = train.IrisLabel, method = 'class')
summary(decTree)
fancyRpartPlot(decTree)
printcp(decTree)
decTree$variable.importance
predicts <- predict(decTree, test.IrisLabel, type = 'class')
tableX <- table(predicts, test.IrisLabel$Species)
confusionMatrix(tableX)
accuracy <- 100*(sum(test.IrisLabel$Species==predicts)/NROW(test.IrisLabel))
accuracy

prunedDecTree <- prune(decTree, cp=0.05)
fancyRpartPlot(prunedDecTree)
printcp(prunedDecTree)
prunePredicts <- predict(prunedDecTree, test.IrisLabel, type = 'class')
prunetableX <- table(prunePredicts, test.IrisLabel$Species)
confusionMatrix(prunetableX)
accuracyPrune <- 100*(sum(test.IrisLabel$Species==prunePredicts)/NROW(test.IrisLabel))
accuracyPrune