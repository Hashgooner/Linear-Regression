library(class)
library(caret)
library(e1071)

# Read the dataset
Iris <- read.csv("C:/Users/harsh/Downloads/Iris.csv")
# Display top rows
head(Iris)
# Display structure of data
str(Iris)
# Display the count of null values
sapply(Iris, function(x)sum(is.na(x)))
# normalise the data
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
# Select the variable columns to be normalised
Iris.normalize <- as.data.frame(lapply(Iris[,2:5], normalize))
# Display top rows from normalised data
head(Iris.normalize)
# Set random sampling seed
set.seed(12345)
# Create test and train data
dataT <- sample(1:nrow(Iris.normalize), size = nrow(Iris.normalize)*0.7, replace = FALSE)
train.Iris <- Iris.normalize[dataT,]
test.Iris <- Iris.normalize[-dataT,]
# Select the target variable column
train.IrisLabel <- Iris[dataT,6]
test.IrisLabel <- Iris[-dataT,6]
# Number of rows from the training data
NROW(train.IrisLabel)
i=1
k.optm = 1
# Create model for every cluster - value selected by using sqaure root of data size
for (i in 1:11) {
  knn.mod <- knn(train = train.Iris, test = test.Iris, cl = train.IrisLabel, k=i)
  k=i
  k.optm[i] <- 100*(sum(test.IrisLabel==knn.mod)/NROW(test.IrisLabel))
  cat(k,'=',k.optm[i],'\n')
}
# plot the accuracy of the model graph
plot(k.optm, type = 'b', xlab = 'K-value', ylab = 'Accuracy level')