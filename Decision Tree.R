library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caret)

# Read the data
Iris <- read.csv("C:/Users/harsh/Downloads/Iris.csv")
# Display top rows
head(Iris)
# Display structure of data
str(Iris)
# Display null values
sapply(Iris, function(x)sum(is.na(x)))

# Set random seed for sampling
set.seed(12345)
# Spit data into train and test
dataT <- sample(1:nrow(Iris), size = nrow(Iris)*0.7, replace = FALSE)
train.IrisLabel <- Iris[dataT,2:6]
test.IrisLabel <- Iris[-dataT,2:6]

# Create decision tree
decTree <- rpart(train.IrisLabel$Species~., data = train.IrisLabel, method = 'class')
summary(decTree)
fancyRpartPlot(decTree)
printcp(decTree)
# Display important variables in decision tree
decTree$variable.importance

predicts <- predict(decTree, test.IrisLabel, type = 'class')
tableX <- table(predicts, test.IrisLabel$Species)
confusionMatrix(tableX)
accuracy <- 100*(sum(test.IrisLabel$Species==predicts)/NROW(test.IrisLabel))
accuracy

# Create Pruned Decision Tree
prunedDecTree <- prune(decTree, cp=0.05)
fancyRpartPlot(prunedDecTree)
printcp(prunedDecTree)
prunePredicts <- predict(prunedDecTree, test.IrisLabel, type = 'class')
prunetableX <- table(prunePredicts, test.IrisLabel$Species)
confusionMatrix(prunetableX)
accuracyPrune <- 100*(sum(test.IrisLabel$Species==prunePredicts)/NROW(test.IrisLabel))
accuracyPrune