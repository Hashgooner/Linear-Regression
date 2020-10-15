library(class)
library(caret)
library(e1071)
Iris <- read.csv("C:/Users/harsh/Downloads/Iris.csv")
head(Iris)
str(Iris)
sapply(Iris, function(x)sum(is.na(x)))
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
Iris.normalize <- as.data.frame(lapply(Iris[,2:5], normalize))
head(Iris.normalize)
set.seed(12345)
dataT <- sample(1:nrow(Iris.normalize), size = nrow(Iris.normalize)*0.7, replace = FALSE)
train.Iris <- Iris.normalize[dataT,]
test.Iris <- Iris.normalize[-dataT,]
train.IrisLabel <- Iris[dataT,6]
test.IrisLabel <- Iris[-dataT,6]
NROW(train.IrisLabel)
i=1
k.optm = 1
for (i in 1:11) {
  knn.mod <- knn(train = train.Iris, test = test.Iris, cl = train.IrisLabel, k=i)
  k=i
  k.optm[i] <- 100*(sum(test.IrisLabel==knn.mod)/NROW(test.IrisLabel))
  cat(k,'=',k.optm[i],'\n')
}
plot(k.optm, type = 'b', xlab = 'K-value', ylab = 'Accuracy level')