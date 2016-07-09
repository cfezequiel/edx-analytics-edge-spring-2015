#1.1
wiki <- read.csv('wiki.csv')
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

#1.2
str(wiki)
library(tm)
corpusAdded <- Corpus(VectorSource(wiki$Added))
strwrap(corpusAdded[[1]])
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)

#1.3
dtmAdded = removeSparseTerms(dtmAdded, 0.997)
dtmAdded

#1.4
wordsAdded <- as.data.frame(as.matrix(dtmAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
strwrap(corpusRemoved[[1]])
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))
str(wordsRemoved)

#1.5
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
summary(wikiWords)
set.seed(123)
library(caTools)
spl = sample.split(wikiWords$Vandal, 0.7)
wikiTrain = subset(wikiWords, spl == TRUE)
wikiTest = subset(wikiWords, spl == FALSE)
table(wikiTest$Vandal)
(618)/(618 + 545)

#1.6
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~., data=wikiTrain, method="class")
wikiPred <- predict(wikiCART, newdata=wikiTest, type="class")
str(wikiPred)
table(wikiTest$Vandal, wikiPred)
(618 + 12) / nrow(wikiTest)

#1.7
prp(wikiCART)

#2.1
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http", wiki$Added, fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

#2.2
wikiTrain2 <- subset(wikiWords2, spl==TRUE)
wikiTest2 <- subset(wikiWords2, spl==FALSE)
wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method="class")
wikiPred2 <- predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, wikiPred2)
(609 + 57) / nrow(wikiTest2)

#2.3
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

#2.4
wikiTrain3 <- subset(wikiWords2, spl==TRUE)
wikiTest3 <- subset(wikiWords2, spl==FALSE)
wikiCART3 <- rpart(Vandal~., data=wikiTrain3, method="class")
wikiPred3 <- predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, wikiPred3)
(514 + 248) / nrow(wikiTest3)

#3.1
wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin
wikiTrain4 <- subset(wikiWords3, spl==TRUE)
wikiTest4 <- subset(wikiWords3, spl==FALSE)
wikiCART4 <- rpart(Vandal~., data=wikiTrain4, method="class")
wikiPred4 <- predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, wikiPred4)
(595 + 241) / nrow(wikiTest4)

#3.2
prp(wikiCART4)

# Automating reviews in medicine

#1.1
trials <- read.csv('clinical_trial.csv', stringsAsFactors=FALSE)
str(trials)
summary(nchar(trials$abstract))

#1.2
nrow(trials[which(nchar(trials$abstract) == 0),])

#1.3
min(nchar(trials$title))
trials$title[which(nchar(trials$title) == 28)]

#2.1
library(tm)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("English"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("English"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
str(dtmTitle)
str(dtmAbstract)

#2.2
# No command needed

#2.3
which(colSums(dtmAbstract) == max(colSums(dtmAbstract)))
sum(dtmAbstract$patient)

#3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
str(dtmTitle)
str(dtmAbstract)

#3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
str(dtm)

#3.3
set.seed(144)
library(caTools)
spl <- sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trial)
(730)/(730 + 572)

#3.4
library(rpart)
library(rpart.plot)
dtmCART <- rpart(trial~., data=train, method="class")
prp(dtmCART)

#3.5
trainPred <- predict(dtmCART)
trainPred.prob <- trainPred[,2]
summary(trainPred.prob)

#3.6
# Pred prob of test exactly the same due to 
# pred prob of each leaf node the same in CART
# Tree

#3.7
table(train$trial, trainPred.prob > 0.5)
(631 + 441) / nrow(train)
(441) / (441 + 131)
(631) / (631 + 99)

#4.1
predTest <- predict(dtmCART, newdata=test)
predTest.prob <- predTest[,2]
table(test$trial, predTest.prob > 0.5)
(261 + 162) / nrow(test)

#4.2
library(ROCR)
predROCR <- prediction(predTest.prob, test$trial)
performance(predROCR, "auc")@y.values

#5.1
# No command needed
# Answer: 3rd option

#5.2
# No command needed
# Answer: 1st option

#5.3
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Answer: 4th option


# Separating spam from ham
#1.1
emails <- read.csv('emails.csv', stringsAsFactors=FALSE)
str(emails)

#1.2
table(emails$spam)

#1.3
summary(emails)
emails[1:10,]

#1.4
# Yes a word that appears in every email would help

#1.5
summary(nchar(emails$text))

#1.6
which(nchar(emails$text) == 13)

#2.1
library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

#2.2
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

#2.3
emailsSparse <- as.data.frame(as.matrix(spdtm))
str(emailsSparse)
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

#2.4
emailsSparse$spam <- emails$spam
which(emailsSparse$spam == 1)
ham <- emailsSparse[which(emailsSparse$spam == 0),]
table(colSums(ham) >= 5000)

#2.5
spam <- emailsSparse[which(emailsSparse$spam == 1),]
spam$spam <- 0
table(colSums(spam) >= 1000)

#2.5
# Answer: Option 2

#2.7
# Answer: Option 2

#3.1
emailsSparse$spam <- as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam, 0.7)
train <- subset(emailsSparse, spl == TRUE)
test <- subset(emailsSparse, spl == FALSE)

# -- logistic model --
spamLog <- glm(spam~., data=train, family=binomial)
spamLogPredTrain <- predict(spamLog, type="response")

# -- CART model --
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam~., data=train, method="class")
spamCARTPredTrain <- predict(spamCART)
spamCARTPredTrain.prob <- spamCARTPredTrain[,2]

# -- RF model --
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam~., data=train)
spamRFPredTrain <- predict(spamRF, type="prob")
spamRFPredTrain.prob <- spamRFPredTrain[,2]

table(spamLogPredTrain < 0.00001)
table(spamLogPredTrain > 0.99999)
table(spamLogPredTrain >= 0.00001 && spamLogPredTrain <= 0.99999)

#3.2 
summary(spamLog)

#3.3
prp(spamCART)

#3.4
table(train$spam, spamLogPredTrain > 0.5)
(3052 + 954) / nrow(train)

#3.5
library(ROCR)
spamLogROCR <- prediction(spamLogPredTrain, train$spam)
performance(spamLogROCR, "auc")@y.values

#3.6
table(train$spam, spamCARTPredTrain.prob > 0.5)
(2885 + 894) / nrow(train)

#3.7
spamCARTROCR <- prediction(spamCARTPredTrain.prob, train$spam)
performance(spamCARTROCR, "auc")@y.values

#3.8
table(train$spam, spamRFPredTrain.prob > 0.5)
(3013 + 914) / nrow(train)

#3.9
spamRFROCR <- prediction(spamRFPredTrain.prob, train$spam)
performance(spamRFROCR, "auc")@y.values

#4.1
spamLogPred <- predict(spamLog, newdata=test, type="response")
table(test$spam, spamLogPred > 0.5)
(1257 + 376) / nrow(test)

#4.2
spamLogROCRTest <- prediction(spamLogPred, test$spam)
performance(spamLogROCRTest, "auc")@y.values

#4.3
spamCARTPred <- predict(spamCART, newdata=test)
spamCARTPred.prob <- spamCARTPred[,2]
table(test$spam, spamCARTPred.prob > 0.5)
(1228 + 386) / nrow(test)

#4.4
spamCARTROCRTest <- prediction(spamCARTPred.prob, test$spam)
performance(spamCARTROCRTest, "auc")@y.values

#4.5
spamRFPred <- predict(spamRF, newdata=test, type="prob")
spamRFPred.prob <- spamRFPred[,2]
table(test$spam, spamRFPred.prob > 0.5)
(1290 + 385) / nrow(test)

#4.6
spamRFPredROCR <- prediction(spamRFPred.prob, test$spam)
performance(spamRFPredROCR, "auc")@y.values
