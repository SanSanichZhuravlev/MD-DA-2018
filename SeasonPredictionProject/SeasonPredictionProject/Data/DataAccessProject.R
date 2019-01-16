install.packages("caret")
install.packages("ellipse")
install.packages("e1071")
install.packages("randomForest")

library(caret)

dataset <- read.csv("archive.csv")

nrow(dataset)
dataset <- na.omit(dataset)
nrow(dataset)
summary(dataset)
dim(dataset)
sapply(dataset, class)
head(dataset)

dataset[dataset$Month == 1 | dataset$Month == 2 | dataset$Month == 12,]$Month <- 1
dataset[dataset$Month >= 3 & dataset$Month <= 5,]$Month <- 2
dataset[dataset$Month >= 6 & dataset$Month <= 8,]$Month <- 3
dataset[dataset$Month >= 9 & dataset$Month <= 11,]$Month <- 4
dataset$Month <- factor(c("Winter", "Spring", "Summer", "Autumn")[dataset$Month])
names(dataset)[2] <- "Season"
summary(dataset)

boxplot(dataset$Carbon.Dioxide..ppm., main = names(dataset[4]))
boxplot(dataset$Seasonally.Adjusted.CO2..ppm., main = names(dataset[5]))
boxplot(dataset$Carbon.Dioxide.Fit..ppm., main = names(dataset[6]))
boxplot(dataset$Seasonally.Adjusted.CO2.Fit..ppm., main = names(dataset[7]))

winter <- dataset[dataset$Season == "Winter",]
spring <- dataset[dataset$Season == "Spring",]
summer <- dataset[dataset$Season == "Summer",]
autumn <- dataset[dataset$Season == "Autumn",]
winter <-winter[sample(1:nrow(winter), 170, replace=FALSE),]
spring <- spring[sample(1:nrow(spring), 170, replace=FALSE),]
summer <- summer[sample(1:nrow(summer), 170, replace=FALSE),]
autumn <- autumn[sample(1:nrow(autumn), 170, replace=FALSE),]
dataset <- rbind(winter, spring, summer, autumn)

validation_index <- createDataPartition(dataset$Season, p=0.90, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(13)
fit.lda <- train(Season~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="lda", metric=metric, trControl=control)
set.seed(13)
fit.cart <- train(Season~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                    Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="rpart", metric=metric, trControl=control)
set.seed(13)
fit.knn <- train(Season~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="knn", metric=metric, trControl=control)
set.seed(13)
fit.svm <- train(Season~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="svmRadial", metric=metric, trControl=control)
set.seed(13)
fit.rf <- train(Season~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                  Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="rf", metric=metric, trControl=control)
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)

predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Season)
