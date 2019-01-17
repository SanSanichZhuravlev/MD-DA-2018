library(caret)

dataset <- read.csv("archive.csv")
nrow(dataset)
dataset <- na.omit(dataset)
nrow(dataset)

dataset[dataset$Year >= 1958 & dataset$Year <=1977,]$Year <- 1
dataset[dataset$Year >= 1978 & dataset$Year <= 1997,]$Year <- 2
dataset[dataset$Year >= 1998 & dataset$Year<= 2017,]$Year <- 3
dataset$Year <- factor(c("1958-1977", "1978-1997", "1998-2017")[dataset$Year])
names(dataset)[1] <- "Period"

firstPeriod <- firstPeriod[sample(1:nrow(firstPeriod), 220, replace=FALSE),]
secondPeriod <- secondPeriod[sample(1:nrow(secondPeriod), 220, replace=FALSE),]
thirdPeriod <- thirdPeriod[sample(1:nrow(thirdPeriod), 220, replace=FALSE),]
dataset <- rbind(firstPeriod, secondPeriod, thirdPeriod)

validation_index <- createDataPartition(dataset$Period, p=0.90, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(13)
fit.rf <- train(Period~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                  Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="rf", metric=metric, trControl=control)