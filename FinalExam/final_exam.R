# Airlines
Airlines = read.csv('AirlineDelay.csv')

# Dataprep
set.seed(15071)
spl = sample(nrow(Airlines), 0.7*nrow(Airlines))
AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]

#P1
nrow(AirlinesTrain)
nrow(AirlinesTest)

#P2 - 2nd

#P3-4
linreg = lm(TotalDelay ~ ., data=AirlinesTrain)
summary(linreg)

#P5
cor(AirlinesTrain$NumPrevFlights, AirlinesTrain$PrevFlightGap)
cor(AirlinesTrain$OriginAvgWind, AirlinesTrain$OriginWindGust)

#P6 - [x], ans=1st 

#P7 - 47.913638

#P8 - [x], ans=2nd

#P9
abs(1.571501 - -5.418356)
abs(-4.506943 - -5.418356)

#P10
lrpred = predict(linreg, newdata=AirlinesTest)
SSE = sum((lrpred - AirlinesTest$TotalDelay)^2)
SSE
SST = sum((mean(AirlinesTrain$TotalDelay) - AirlinesTest$TotalDelay)^2)
SST
R2 = 1 - SSE/SST
R2

#P11 - 1st

#P12
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
length(which(Airlines$DelayClass == "No Delay"))
length(which(Airlines$DelayClass == "Minor Delay"))
length(which(Airlines$DelayClass == "Major Delay"))
Airlines$TotalDelay = NULL
set.seed(15071)
library(caTools)
spl = sample.split(Airlines$DelayClass, SplitRatio=0.7)
AirlinesTrain = subset(Airlines, spl==TRUE)
AirlinesTest = subset(Airlines, spl==FALSE)

#P13
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(DelayClass ~ ., data=AirlinesTrain) 
rpart.plot(CARTmodel)

#P14 - 3rd

#P15
CARTpred = predict(CARTmodel, type="class")
table(AirlinesTrain$DelayClass, CARTpred)
acc = (0 + 361 + 3094) / nrow(AirlinesTrain)
acc

#P16
table(AirlinesTrain$DelayClass)
3282 / nrow(AirlinesTrain)

#P17
CARTpred = predict(CARTmodel, newdata=AirlinesTest, type="class")
table(AirlinesTest$DelayClass, CARTpred)
acc = (153 + 1301) / nrow(AirlinesTest)
acc

#P18 - [X], ans=2nd+3rd


# eBay
#P1
eBay = read.csv('ebay.csv')
prop.table(table(eBay$sold))

#P2
str(eBay)
table(is.na(eBay$biddable))
table(is.na(eBay$sold))
table(is.na(eBay$startprice))
table(is.na(eBay$size)) #has NA

#P3
hist(eBay$size, breaks=12)

#P4 [x]
eBay$sold = as.factor(eBay$sold)
eBay$condition = as.factor(eBay$condition)
eBay$heel = as.factor(eBay$heel)
eBay$style = as.factor(eBay$style)
eBay$color = as.factor(eBay$color)
eBay$material = as.factor(eBay$material)

#ans=Random Forest

#P5
set.seed(144)
library(caTools)
spl = sample.split(eBay$sold, 0.7)
training = subset(eBay, spl==TRUE)
testing = subset(eBay, spl==FALSE)
# ans=3rd

#P6
model.log = glm(sold ~ biddable + startprice + condition + heel + style + color + material, data=training, family=binomial)
summary(model.log)
# ans=2nd

#P7
pred1.log = -0.0044423*100 + -0.4952981*1 + 0.1224260*1 + 0.5268920*1 + 0.2226547*1 + -1.1078098*1
prob = 1 / (1 + exp(-pred1.log))
prob

#P8 [x]
odds = exp(0.8325406)
odds
#ans=4th

#P9
pred.log = predict(model.log, newdata=testing, type="response")
table(pred.log >= 0.5)
table(testing$sold)
abs((899 + 240) - 1059)

#P10
library(ROCR)
ROCRpred = prediction(pred.log, testing$sold)
as.numeric(performance(ROCRpred, "auc")@y.values)


#P11 [x], ans=2nd

#P12 [x], ans=1st
perf <- performance(ROCRpred,"tpr","fpr")
plot(perf)

#P13 [x], ans=0.64
plot(perf, colorize=TRUE)

#P14 [x], ans=3rd

#P15
set.seed(144)
library(caret)
library(e1071)
numFolds <- trainControl(method="cv", number=10)
cartGrid <- expand.grid(.cp = seq(0.001,0.05,0.001))
train(sold ~ biddable + startprice + condition + heel + style + color + material, data=training, method="rpart", trControl=numFolds, tuneGrid=cartGrid)

#P16
library(rpart)
library(rpart.plot)
model.CART = rpart(sold ~ biddable + startprice + condition + heel + style + color + material, data=training, cp=0.005) 
prp(model.CART)

#P17
library(tm)
corpus = Corpus(VectorSource(eBay$description))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
df = as.data.frame(as.matrix(dtm))
str(df)

#P18
spdtm <- removeSparseTerms(dtm, 0.90)
df = as.data.frame(as.matrix(spdtm))
str(df)

#P19
descriptionText = as.data.frame(as.matrix(spdtm))
sort(sapply(descriptionText, sum))

#P20
names(descriptionText) = paste0("D", names(descriptionText))
descriptionText$sold = eBay$sold
descriptionText$biddable = eBay$biddable
descriptionText$startprice = eBay$startprice
descriptionText$condition = eBay$condition
descriptionText$heel = eBay$heel
descriptionText$style = eBay$style
descriptionText$color = eBay$color
descriptionText$material = eBay$material
str(descriptionText)
set.seed(144)
spl = sample.split(descriptionText$sold, 0.7)
trainText = subset(descriptionText, spl==TRUE)
testText = subset(descriptionText, spl==FALSE)
length(names(testText))

#P21 [x], ans=13 (I counted 16)
glmText = glm(sold ~ ., data=trainText, family=binomial)
summary(glmText)

#P22
predTrainText = predict(glmText, type="response")
length(predTrainText)
length(trainText$sold)
library(ROCR)
predROCR <- prediction(predTrainText, trainText$sold)
performance(predROCR, "auc")@y.values
predText = predict(glmText, newdata=testText, type="response")
predROCR <- prediction(predText, testText$sold)
performance(predROCR, "auc")@y.values

#P23 [x], ans=4th
