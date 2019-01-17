install.packages("caret")
install.packages("ellipse")
install.packages("e1071")
install.packages("randomForest")

library(caret)

dataset <- read.csv("archive.csv")

nrow(dataset)
dataset <- na.omit(dataset)
nrow(dataset)
colnames(dataset)
summary(dataset)
dim(dataset)
sapply(dataset, class)
head(dataset)

dataset[dataset$Year >= 1958 & dataset$Year <=1977,]$Year <- 1
dataset[dataset$Year >= 1978 & dataset$Year <= 1997,]$Year <- 2
dataset[dataset$Year >= 1998 & dataset$Year<= 2017,]$Year <- 3
dataset$Year <- factor(c("1958-1977", "1978-1997", "1998-2017")[dataset$Year])
names(dataset)[1] <- "Period"
summary(dataset)

boxplot(dataset$Carbon.Dioxide..ppm., main = names(dataset[4]))
boxplot(dataset$Seasonally.Adjusted.CO2..ppm., main = names(dataset[5]))
boxplot(dataset$Carbon.Dioxide.Fit..ppm., main = names(dataset[6]))
boxplot(dataset$Seasonally.Adjusted.CO2.Fit..ppm., main = names(dataset[7]))
firstPeriod <- dataset[dataset$Period == "1958-1977",]
secondPeriod <- dataset[dataset$Period == "1978-1997",]
thirdPeriod <- dataset[dataset$Period == "1998-2017",]

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
fit.lda <- train(Period~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="lda", metric=metric, trControl=control)
set.seed(13)
fit.cart <- train(Period~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                    Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="rpart", metric=metric, trControl=control)
set.seed(13)
fit.knn <- train(Period~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="knn", metric=metric, trControl=control)
set.seed(13)
fit.svm <- train(Period~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="svmRadial", metric=metric, trControl=control)
set.seed(13)
fit.rf <- train(Period~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                  Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="rf", metric=metric, trControl=control)
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
print(fit.rf)

predictions <- predict(fit.rf, validation)
confusionMatrix(predictions, validation$Period)
