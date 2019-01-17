install.packages("gWidgets"); 
install.packages("gWidgetstcltk"); 
install.packages("RGtk2"); 

library(caret) 
library(gWidgets) 
library(gWidgetstcltk) 
require(RGtk2) 

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
predictions <- predict(fit.rf, validation)


labelCarbonDioxidePpm <- "Впишите показатель Carbon.Dioxide..ppm. (от 313.2 до 407.6): "
labelSeasonallyAdjustedCO2Ppm <- "Впишите показатель Seasonally.Adjusted.CO2..ppm. (от 314.4 до 406.0): "
labelCarbonDioxideFitPpm <- "Впишите показатель Carbon.Dioxide.Fit..ppm. (от 312.5 до 407.3): "
labelSeasonallyAdjustedCO2FitPpm <- "Впишите показатель Seasonally.Adjusted.CO2.Fit..ppm. (от 314.9 до 405.8): "
labelPeriod <- ("Период: ")

createrInterface <- function()
{
  library(gWidgetstcltk) 
  options(guiToolkit = "tcltk") 
  wnds <- gwindow("Определить период", visible = FALSE) 
  
  
  frmCarbonDioxidePpm <- gframe("Carbon.Dioxide..ppm.", container = wnds) 
  lblCarbonDioxidePpm <- glabel(labelCarbonDioxidePpm, container = frmCarbonDioxidePpm) 
  txtCarbonDioxidePpm <- gedit(container = frmCarbonDioxidePpm) 
  
  frmSeasonallyAdjustedCO2Ppm <- gframe("Seasonally.Adjusted.CO2..ppm.", container = wnds) 
  lblSeasonallyAdjustedCO2Ppm <- glabel(labelSeasonallyAdjustedCO2Ppm, container = frmSeasonallyAdjustedCO2Ppm) 
  txtSeasonallyAdjustedCO2Ppm <- gedit(container = frmSeasonallyAdjustedCO2Ppm) 
  
  frmCarbonDioxideFitPpm <- gframe("Carbon.Dioxide.Fit..ppm.", container = wnds) 
  lblCarbonDioxideFitPpm <- glabel(labelCarbonDioxideFitPpm, container = frmCarbonDioxideFitPpm) 
  txtCarbonDioxideFitPpm <- gedit(container = frmCarbonDioxideFitPpm) 
  
  frmSeasonallyAdjustedCO2FitPpm <- gframe("Seasonally.Adjusted.CO2.Fit..ppm.", container = wnds) 
  lblSeasonallyAdjustedCO2FitPpm <- glabel(labelSeasonallyAdjustedCO2FitPpm, container = frmSeasonallyAdjustedCO2FitPpm) 
  txtSeasonallyAdjustedCO2FitPpm <- gedit(container = frmSeasonallyAdjustedCO2FitPpm) 
  
  btnCalc <- gbutton("Период времени", container = wnds, 
                     handling = function(h, ...)
                     {
                         CarbonDioxidePpm <- as.Numeric(svalue(txtCarbonDioxidePpm))
                         SeasonallyAdjustedCO2Ppm <- as.Numeric(svalue(txtSeasonallyAdjustedCO2Ppm))
                         CarbonDioxideFitPpm <- as.Numeric(svalue(txtCarbonDioxideFitPpm))
                         SeasonallyAdjustedCO2FitPpm <- as.Numeric(svalue(txtSeasonallyAdjustedCO2FitPpm))
                         
                         params <- data.frame("CarbonDioxidePpm" = CarbonDioxidePpm, "SeasonallyAdjustedCO2Ppm" = SeasonallyAdjustedCO2Ppm,
                                              "CarbonDioxideFitPpm" = CarbonDioxideFitPpm, "SeasonallyAdjustedCO2FitPpm" = SeasonallyAdjustedCO2FitPpm) 
                         
                         params 
                         
                         predictions <- predict(fit.lda, params) 
                         svalue(txtPeriod) <- format(predictions[1]) 
                     }
  )
  frmResults <- gframe("Predicted quality", container = wnds) 
  lblPeriod <- glabel(labelPeriod, container = frmResults) 
  txtPeriod <- gedit(container = frmResults) 
  visible(wnds) <- TRUE 
}

createrInterface()