# Bike sharing
#P1
hubway.trips = read.csv('HubwayTrips.csv')
nrow(hubway.trips)

#P2
str(hubway.trips)
summary(hubway.trips)
summary(hubway.trips$Duration[hubway.trips$Weekday == 1])
summary(hubway.trips$Duration[hubway.trips$Weekday == 0])

#P3
nrow(hubway.trips[hubway.trips$Morning == 1,])
nrow(hubway.trips[hubway.trips$Afternoon == 1,])
nrow(hubway.trips[hubway.trips$Evening == 1,])

#P4
prop.table(table(hubway.trips$Male))

#P5
summary(hubway.trips)

#P6
library(caret)
preproc = preProcess(hubway.trips)
hubway.trips.norm = predict(preproc, hubway.trips)
summary(hubway.trips.norm)

#P7 - 3rd

#P8
set.seed(5000)
hubway.trips.kmeans = kmeans(hubway.trips.norm, centers=10)
spl = split(hubway.trips.norm, hubway.trips.kmeans$cluster)
lapply(spl, nrow)

#P9: cluster 10
#P10: cluster 8
#P11: cluster 4
spl2 = split(hubway.trips, hubway.trips.kmeans$cluster)
lapply(spl2, summary)

#P12
set.seed(4000)
hubway.trips.kmeans2 = kmeans(hubway.trips.norm, centers=10)
spl3 = split(hubway.trips.norm, hubway.trips.kmeans2$cluster)
lapply(spl3, nrow)

#P13 - 2nd, decrease

#P14
set.seed(8000)
hubway.trips.kmeans3 = kmeans(hubway.trips.norm, centers=20)
spl4 = split(hubway.trips.norm, hubway.trips.kmeans3$cluster)
lapply(spl4, nrow)

#P15 - cluster 13,7
spl5 = split(hubway.trips, hubway.trips.kmeans3$cluster)
lapply(spl5, summary)
summary(c(45891,738.6,748.6, 767.7, 687.5, 876.1, 646.4, 673.8, 772.1, 586, 734.6, 680.7, 729.4, 689.1, 844.8, 617.1, 621.4, 744.3, 9335, 840))

#P16 - 1st

#P17

