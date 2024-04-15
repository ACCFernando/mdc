#################################
# MDDC - Machine Learning	    #
# Random Forest Iris			#
#################################
library(randomForest)
library("caret")

set.seed(42)

# load dataset
data <- iris

summary(data)

# split the data from each class into train and val
setosaData <- data[data$Species == "setosa",]
idx <- sample(1:nrow(setosaData), 0.8*nrow(setosaData))
setosaTrainData <- setosaData[idx,]
setosaValData <- setosaData[-idx,]

versicolorData <- data[data$Species == "versicolor",]
idx <- sample(1:nrow(versicolorData), 0.8*nrow(versicolorData))
versicolorTrainData <- versicolorData[idx,]
versicolorValData <- versicolorData[-idx,]

virginicaData <- data[data$Species == "virginica",]
idx <- sample(1:nrow(virginicaData), 0.8*nrow(virginicaData))
virginicaTrainData <- virginicaData[idx,]
virginicaValData <- virginicaData[-idx,]

# join all validation data together
trainData <- rbind(setosaTrainData, versicolorTrainData, virginicaTrainData)
valData <- rbind(setosaValData, versicolorValData, virginicaValData)

#Check both sets
summary(trainData)


# Train a randomForest
rfModel <- randomForest(formula= Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                        data= trainData, ntree=10)


# Predict valData
prediction <- predict(rfModel, valData)

# Get confusion matrix
cm <- confusionMatrix(data = as.factor(prediction),
                      reference = as.factor(valData$Species))

cm$table
cm$byClass