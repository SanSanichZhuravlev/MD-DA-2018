library(caret)

dataset <- read.csv("archive.csv")
nrow(dataset)
dataset <- na.omit(dataset)
nrow(dataset)

dataset[dataset$Month == 1 | dataset$Month == 2 | dataset$Month == 12,]$Month <- 1
dataset[dataset$Month >= 3 & dataset$Month <= 5,]$Month <- 2
dataset[dataset$Month >= 6 & dataset$Month <= 8,]$Month <- 3
dataset[dataset$Month >= 9 & dataset$Month <= 11,]$Month <- 4
dataset$Month <- factor(c("Winter", "Spring", "Summer", "Autumn")[dataset$Month])
names(dataset)[2] <- "Season"

winter <- dataset[dataset$Season == "Winter",]
spring <- dataset[dataset$Season == "Spring",]
summer <- dataset[dataset$Season == "Summer",]
autumn <- dataset[dataset$Season == "Autumn",]
winter <-winter[sample(1:nrow(winter), 170, replace=FALSE),]
spring <- spring[sample(1:nrow(spring), 170, replace=FALSE),]
summer <- summer[sample(1:nrow(summer), 170, replace=FALSE),]
autumn <- autumn[sample(1:nrow(autumn), 170, replace=FALSE),]
dataset <- rbind(winter, spring, summer, autumn)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(13)
fit.lda <- train(Season~Carbon.Dioxide..ppm. + Seasonally.Adjusted.CO2..ppm. + Carbon.Dioxide.Fit..ppm. + 
                   Seasonally.Adjusted.CO2.Fit..ppm., data=dataset, method="lda", metric=metric, trControl=control)