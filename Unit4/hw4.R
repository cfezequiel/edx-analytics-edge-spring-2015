#Why People Vote

gerber = read.csv('gerber.csv')
str(gerber)

#1.1
prop.table(table(gerber$voting))

#1.2
prop.table(table(gerber$voting, gerber$civicduty))
prop.table(table(gerber$voting, gerber$hawthorne))
prop.table(table(gerber$voting, gerber$self))
prop.table(table(gerber$voting, gerber$neighbors))

#1.3
gerberLog <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(gerberLog)

#1.4
gerberPred <- predict(gerberLog, type='response')
table(gerber$voting, gerberPred > 0.3)
acc <- (51966 + 134513) / nrow(gerber)
acc

#1.5
table(gerber$voting, gerberPred > 0.5)
acc <- (235388) / nrow(gerber)
acc

#1.6
table(gerber$voting)
noneVotingProp <- 235388 / nrow(gerber)
noneVotingProp
library(ROCR)
ROCRpred = prediction(gerberPred, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

#2.1
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber) 
plot(CARTmodel)

#2.2-2.3
CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0) 
rpart.plot(CARTmodel2)

#2.4
CARTmodel3 <- rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0) 
rpart.plot(CARTmodel3)

#3.1
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0) 
rpart.plot(CARTmodel4)
prp(CARTmodel4, digits=6)
abs(0.296638 - 0.34)

#3.2
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0) 
prp(CARTmodel5, digits=6)
womenDiff <- abs(0.290456 - 0.334176)
womenDiff
menDiff <- abs(0.302795 - 0.345818)
menDiff
diff <- abs(womenDiff - menDiff)
diff

#3.3
LogModelSex <- glm(voting ~ control + sex, data=gerber, family=binomial)
summary(LogModelSex)

#3.4
Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
lmsPred <- predict(LogModelSex, newdata=Possibilities, type="response")
lmsPred
diff <- abs(.290456 - .2908065)
diff

#3.5
LogModelSex2 <- glm(voting ~ sex + control + sex:control, data=gerber, family='binomial')
summary(LogModelSex2)

#3.6
lmsPred2 <- predict(LogModelSex2, newdata=Possibilities, type="response")
lmsPred2
abs(0.2904558 - 0.290456)

# Letter Recognition
#1.1
letters <- read.csv('letters_ABPR.csv')
letters$isB <- as.factor(letters$letter == 'B')
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain = subset(letters, split==TRUE)
lettersTest = subset(letters, split==FALSE)
table(letters$isB)
2350 / nrow(letters)

#1.2
CARTb <- rpart(isB ~ . - letter, data=lettersTrain, method="class")
CARTbPred <- predict(CARTb, newdata=lettersTest, type="class")
table(lettersTest$isB, CARTbPred)
(340 + 1118) / nrow(lettersTest)

#1.3
library(randomForest)
set.seed(1000)
lettersForest <- randomForest(isB ~ . - letter, data=lettersTrain, method="class")
lettersPred = predict(lettersForest, newdata=lettersTest, type="class")
table(lettersTest$isB, lettersPred)
(1165 + 374) / nrow(lettersTest)

#2.1
letters$letter <- as.factor(letters$letter)
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
lettersTrain = subset(letters, split==TRUE)
lettersTest = subset(letters, split==FALSE)
table(letters$letter)
803 / nrow(letters)

#2.2
lettersCART <- rpart(letter ~ . - isB, data=lettersTrain, method="class")
lettersCARTPred = predict(lettersCART, newdata=lettersTest, type="class")
str(lettersCARTPred)
table(lettersTest$letter, lettersCARTPred)
(348 + 318 + 363 + 340) / nrow(lettersTest)

#2.3
set.seed(1000)
lettersForest <- randomForest(letter ~ . - isB, data=lettersTrain, method="class")
lettersPred = predict(lettersForest, newdata=lettersTest, type="class")
table(lettersTest$letter, lettersPred)
(390 + 380 + 393 + 364) / nrow(lettersTest)


# Predicting earnings from census data
#1.1
census <- read.csv('census.csv')
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
censusTrain = subset(census, split==TRUE)
censusTest = subset(census, split==FALSE)
censusLog <- glm(over50k ~ ., data=censusTrain, family=binomial)
summary(censusLog)

#1.2
censusPred <- predict(censusLog, newdata=censusTest)
table(censusTest$over50k, censusPred > 0.5)
(9351 + 1515) / nrow(censusTest)

#1.3
table(censusTest$over50k)
9713/nrow(censusTest)

#1.4
library(ROCR)
predROC <- prediction(censusPred, censusTest$over50k)
as.numeric(performance(predROC, "auc")@y.values)

#2.1 CART
censusCART <- rpart(over50k ~ ., data=censusTrain, method="class")

#2.2-2.3
prp(censusCART)

#2.4
censusCARTPred <- predict(censusCART, newdata=censusTest, type="class")
table(censusTest$over50k, censusCARTPred)
(9243 + 1596) / nrow(censusTest)

#2.5
censusCARTPred2 <- predict(censusCART, newdata=censusTest)
predROC <- prediction(censusCARTPred[,2], censusTest$over50k)
perf = performance(predROC, "tpr", "fpr")
plot(perf)

#2.6
as.numeric(performance(predROC, "auc")@y.values)

#3.1
set.seed(1)
trainSmall <- censusTrain[sample(nrow(censusTrain), 2000),]
set.seed(1)
censusForest <- randomForest(over50k ~ ., data=trainSmall, method="class")
censusForestPred <- predict(censusForest, newdata=censusTest)
table(censusTest$over50k, censusForestPred)
(9643 + 851) / nrow(censusTest)

#3.2
vu <- varUsed(censusForest, count=TRUE)
vusorted <- sort(vu, decreasing=FALSE, index.return=TRUE)
dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

#3.3
varImpPlot(censusForest)

#4.1
library(caret)
library(e1071)
set.seed(2)
numFolds <- trainControl(method="cv", number=10)
cartGrid <- expand.grid(.cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data=censusTrain, method="rpart", trControl=numFolds, tuneGrid=cartGrid)

#4.2
censusCARTCV <- rpart(over50k ~ ., data=censusTrain, method="class", cp=0.002)
censusCARTCVPred <- predict(censusCARTCV, newdata=censusTest, type="class")
table(censusTest$over50k, censusCARTCVPred)
(9178 + 1838) / nrow(censusTest)

#4.3
prp(censusCARTCV)
