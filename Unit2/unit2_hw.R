# Climate Change
data <- read.csv('climate_change.csv')
str(data)
summary(data)
train <- subset(data, data$Year <= 2006)
test <- subset(data, data$Year >= 2007)

model <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model)
cor(train)

model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(model2)

model3 <- step(model)
summary(model3)

# Get test predictions
pred <- predict(model3, newdata=test)
summary(pred)

# Get testing set R-squared
SSE = sum((pred - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2

# Reading test scores
pisaTrain <- read.csv('pisa2009train.csv')
pisaTest <- read.csv('pisa2009test.csv')

str(pisaTrain)

# Average reading scores by male/female
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# Remove obs with NA
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)

str(pisaTrain)
str(pisaTest)

# Set reference level of factor variable
pisaTrain$raceeth <- relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth, "White")

# Build model
lmScore <- lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

# Get RMSE
SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE

# Predicting
m <- 29.542707
studentAScore <- m*11
studentBScore <- m*9
diffScore <- studentAScore - studentBScore

# Predicting the test set
predTest <- predict(lmScore, newdata=pisaTest)
predTest
summary(predTest)

# SSE
SSE <- sum((predTest - pisaTest$readingScore)^2)
RMSE <- sqrt(SSE/nrow(pisaTest))

# Baseline model
baseScore <- mean(pisaTrain$readingScore)
SST = sum((baseScore - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST

# -------------
# Flu epidemics
# -------------

FluTrain <- read.csv('FluTrain.csv')
FluTest <- read.csv('FluTest.csv')
str(FluTrain)
summary(FluTrain)
which.max(FluTrain$ILI)
which.max(FluTrain$Queries)
hist(FluTrain$ILI)

plot(FluTrain$Queries, log(FluTrain$ILI))
FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
cor(FluTrain$Queries, log(FluTrain$ILI))
PredTest1 <- exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == '2012-03-11 - 2012-03-17')]
RelativeError <- (FluTest$ILI - 2.187378)/FluTest$ILI
RelativeError[which(FluTest$Week == '2012-03-11 - 2012-03-17')]

SSE <- sum((PredTest1 - FluTest$ILI)^2)
RMSE1 <- sqrt(SSE/nrow(FluTest))

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

plot(FluTrain$ILI, FluTrain$ILILag2)

#Problem 4.3
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)

#Problem 5.1
FluTest$ILILag2 <- coredata(lag(zoo(FluTest$ILI), -2, na.pad=TRUE))
summary(FluTest)

#Problem 5.3
FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

#Problem 5.4
PredTest2 <- exp(predict(FluTrend2, newdata=FluTest))
SSE <- sum((PredTest2 - FluTest$ILI)^2)
RMSE2 <- sqrt(SSE/nrow(FluTest))
RMSE2
