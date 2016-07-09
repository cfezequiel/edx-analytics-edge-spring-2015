library(ggplot2)
library(maps)
library(ggmap)

# Votes

statesMap = map_data("state")
str(statesMap)

#1.1
unique(statesMap$group)

#1.2
ggplot(statesMap, aes(x=long, y=lat, group=group))  + geom_polygon(fill="white", color="black")

#2.1
polling = read.csv('PollingImputed.csv')
spl = split(polling, polling$Year == 2012)
Train = spl[[1]]
Test = spl[[2]]
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family=binomial)
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
length(Test$State[which(TestPredictionBinary == 1)])
mean(TestPrediction)

#2.2
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by="region")
predictionMap = predictionMap[order(predictionMap$order),]
nrow(predictionMap)
nrow(statesMap)

#2.3

#2.4
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

#2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#3.1
str(predictionMap)
predictionMap$TestPrediction[which(predictionMap$region=="florida")]

#4.1
#linetype, size

#4.2
#alpha


# Visualizing network data

#1.1
edges = read.csv('edges.csv')
users = read.csv('users.csv')
ave = nrow(edges) * 2 / nrow(users)
ave

#1.2
table(users$locale)

#1.3
table(users$school, users$gender)

#2.1
install.packages("igraph")
library(igraph)
g = graph.data.frame(edges, FALSE, users)

#2.2
plot(g, vertex.size=5, vertex.label=NA)

#2.3
table(degree(g) >= 10)

#2.4
V(g)$size = degree(g)/2 + 2
plot(g, vertex.label=NA)
max(V(g)$size)
min(V(g)$size)

#3.1
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
sort(V(g)$size, decreasing=TRUE)
V(g)$gender[which(V(g)$size >= 7)]

#3.2
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label=NA)
edges$V2[which(edges$V1==3999)]
sort(V(g)$size, decreasing=TRUE)
V(g)$school[which(V(g)$size >= 7)]

#3.3
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "blue"
plot(g, vertex.label=NA)


# Word Clouds

#1.1
tweets = read.csv("tweets.csv")
library(tm)
corpufy = function(v) {
  corpus = Corpus(VectorSource(v))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))  
  dtm = DocumentTermMatrix(corpus)
  words = as.data.frame(as.matrix(dtm))
  return(words)
}
allTweets = corpufy(tweets$Tweet)
str(allTweets)

#1.2
#Easier to read wordcloud

#2.1
install.packages("wordcloud")
library(wordcloud)

#2.2
#colSums

#2.3
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

#3.1
negTweets = allTweets[which(tweets$Avg < 0),]
wordcloud(colnames(negTweets), colSums(negTweets), scale=c(2, 0.25))

#3.2
#Word Cloud A

#3.3
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), random.order=FALSE)

#3.4
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(4, 0.5), rot.per=.1)
# Word Cloud A

#3.5
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), random.color=TRUE)

#4.1
library(RColorBrewer)

#4.3
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, "Blues")[c(-1,-2,-3,-4)])

