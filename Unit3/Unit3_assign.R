# Music
songs = read.csv('songs.csv')
str(songs)
summary(songs)

#1.1
str(songs[songs$year == 2010,])

#1.2
mj_songs <- songs[songs$artistname == 'Michael Jackson',]
mj_songs$songtitle[which(mj_songs$Top10 == 1)]

#1.4
table(songs$timesignature)

#1.5
songs$songtitle[which(songs$tempo == max(songs$tempo))]

#2.1
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
str(SongsTrain)

#2.2
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[, !(names(SongsTrain)%in%nonvars)]
SongsTest = SongsTest[, !(names(SongsTest)%in%nonvars)]
SongsLog1 <- glm(Top10 ~., data=SongsTrain, family=binomial)

#2.3-5
summary(SongsLog1)

#3.1
cor(SongsTrain$loudness, SongsTrain$energy)

#3.2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#3.3
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#4.1
SongsPred3 = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, SongsPred3 >= 0.45)
accuracy <- (309 + 19) / nrow(SongsTest)
accuracy

#4.2
baseline <- 314 / nrow(SongsTest)
baseline

#4.3
table(SongsTest$Top10, SongsPred3 >= 0.45)

#4.4
sensitivity <- 19 / (19 + 40)
sensitivity
specificity <- 309 / (309 + 5)
specificity

#Parole

#1.1
Parole <- read.csv('parole.csv')
str(Parole)
summary(Parole)

#1.2
table(Parole$violator)

#1.3 (see 1.1)

#2.2
Parole$state <- as.factor(Parole$state)
Parole$crime <- as.factor(Parole$crime)
summary(Parole)

#3.1
set.seed(144)
library(caTools)
split = sample.split(Parole$violator, SplitRatio = 0.7)
train <- subset(Parole, split == TRUE)
test <- subset(Parole, split == FALSE)

#4.1
ParoleLog = glm(violator ~ ., data=train, family=binomial)
summary(ParoleLog)

#4.3
LogOdds <- 0.3869904*1 + 0.8867192*1 - 0.0001756*50 - 0.1238867*3 + 0.0802954*12 + 0.6837143*1 - 4.2411574
Odds <- exp(LogOdds)
Odds
Prob <- 1 / (1 + exp(-LogOdds))
Prob

#5.1
ParolePred = predict(ParoleLog, newdata=test, type="response")
summary(ParolePred)

#5.2
table(test$violator, ParolePred >= 0.5)
sensitivity <- 12 / (12 + 11)
sensitivity
specificity <- 167 / (167 + 12)
specificity
accuracy <- (167 + 12) / nrow(test)
accuracy

#5.3
baseline_accuracy <- 179 / nrow(test)
baseline_accuracy

#5.6
library(ROCR)
ROCRpred = prediction(ParolePred, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Load Repayment
loans <- read.csv('loans.csv')
str(loans)

#1.1
prop.table(table(loans$not.fully.paid))

#1.2
summary(loans)

#1.3
#?

#1.4
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed #different values
loans <- read.csv('loans_imputed.csv')

#2.1
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
LoansTrain <- subset(loans, split == TRUE)
LoansTest <- subset(loans, split == FALSE)

LoansLog = glm(not.fully.paid ~ ., data=LoansTrain, family=binomial)
summary(LoansLog)

#2.2
Ldiff <- -9.317e-03 * (700 - 710)
Ldiff
oa_over_ob <- exp(Ldiff)

#2.3
predicted.risk = predict(LoansLog, newdata=LoansTest, type="response")
LoansTest$predicted.risk <- predicted.risk
table(LoansTest$not.fully.paid, predicted.risk >= 0.5)
acc <- (3 + 2400) / nrow(LoansTest)
acc
baseline_acc <- 2413 / nrow(LoansTest)
baseline_acc

#2.4
library(ROCR)
ROCRpred = prediction(predicted.risk, LoansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#3.1
LoansLog2 = glm(not.fully.paid~int.rate, data=LoansTrain, family=binomial)
summary(LoansLog2)

#3.2
base.pred <- predict(LoansLog2, newdata=LoansTest, type="response")
table(LoansTest$not.fully.paid, base.pred >= 0.5)

#3.3
ROCRpred = prediction(base.pred, LoansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#4.1
I = 10 * exp((0.06)*(3))
I

#5.1
LoansTest$profit <- exp(LoansTest$int.rate*3) - 1
LoansTest$profit[LoansTest$not.fully.paid==1] = -1
maxProfit10 <- 10 * max(LoansTest$profit)
maxProfit10

#6.1
highInterest <- subset(LoansTest, LoansTest$int.rate >= 0.15)
summary(highInterest$profit)

#6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff
selectedLoans <- subset(highInterest, highInterest$predicted.risk <= cutoff)
nrow(selectedLoans)
sum(selectedLoans$profit)

