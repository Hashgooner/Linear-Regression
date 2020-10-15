library('pastecs')
library('moments')

# Read the data from file
data <- read.csv('https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv')
# Display top rows from the dataset
head(data)
# Display names of columns
names(data)
# Display the structure of dataset
str(data)

# Descriptive Statistics rounded to 2 decimals
res <- stat.desc(data)
round(res,2)

# Skewness and Kurtosis
skewness(data$Hours)
kurtosis(data$Hours)
skewness(data$Scores)
kurtosis(data$Scores)

# Histogram with normal line
h <- hist(data$Hours, main = 'Histogram for variable Hours', col = '#add8e6', xlab = 'Hours', breaks = 10)
xfit <- seq(min(data$Hours), max(data$Hours), length=40)
yfit <- dnorm(xfit, mean = mean(data$Hours), sd = sd(data$Hours))
yfit <- yfit*diff(h$mids[1:2])*length(data$Hours)
lines(xfit, yfit, col='black', lwd=2)

h1 <- hist(data$Scores, main = 'Histogram for variable Scores', col = '#add8e6', xlab = 'Scores', breaks = 10)
xfit1 <- seq(min(data$Scores), max(data$Scores), length=40)
yfit1 <- dnorm(xfit1, mean = mean(data$Scores), sd = sd(data$Scores))
yfit1 <- yfit1*diff(h1$mids[1:2])*length(data$Scores)
lines(xfit1, yfit1, col='black', lwd=2)

# Boxplots
boxplot(data$Hours, main = 'Boxplot for variable Hours', col = '#add8e6', ylab = 'Hours')
boxplot(data$Scores, main = 'Boxplot for variable Scores', col = '#add8e6', ylab = 'Scores')

# Scatter plot
scatter.smooth(x = data$Hours, y=data$Scores)

# Correlation between variables
cor(data$Hours, data$Scores)

# Linear Regression Model
linMod <- lm(Scores ~ Hours, data = data)
summary(linMod)
plot(linMod)

# Predicting values based on new inputs
coef(linMod)[1]+coef(linMod)[2]*9.25
predict(linMod, data.frame(Hours=9.25))