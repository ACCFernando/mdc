#############################################################################
#
#  This file is a part of the R package "gradDescent".
#
#  Author: Galih Praja Wijaya
#  Co-author: Dendi Handian, Imam Fachmi Nasrulloh
#  Supervisors: Lala Septem Riza, Rani Megasari, Enjun Junaeti
#  Copyright (c) Department of Computer Science Education, Indonesia University of Education.
#
#  This package is free software: you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free Software
#  Foundation, either version 2 of the License, or (at your option) any later version.
#
#  This package is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
#  A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
#############################################################################
#' A function to do feature scaling to dataset with variance/standardization
#' scaling method .
#'
#' This function changes the value of dataset that represented by data.frame
#' object into variance scaled value that has interval value near -1 to 1.
#'
#' @title The Variance/Standardization Feature Scaling Function
#'
#' @param dataSet a data.frame that representing dataset to be processed.
#'        dataSet must have at leas two columns and ten rows of data that
#'        contain only numbers (integer or float). The last column to the
#'        left will be defined as output variable.
#'
#' @examples
#' ##################################
#' ## Feature scaling with Variance Scaling Method
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## do variance scaling to dataset
#' featureScalingResult <- varianceScaling(dataSet)
#' ## show result
#' print(featureScalingResult$scaledDataSet)
#' print(featureScalingResult$scalingParameter)
#'
#' @seealso \code{\link{varianceDescaling}}
#'
#' @return a list contains feature scaled dataset and scaling parameter
#'
#' @export

varianceScaling <- function(dataSet){
    columnLength <- ncol(dataSet)
    varianceParameter <- getMeanStd(dataSet)
    meanValue <- varianceParameter[1,]
    stdValue <- varianceParameter[2,]
    scaledDataSet <- (dataSet-meanValue)/stdValue
    result <- list()
    result$scaledDataSet <- scaledDataSet
    result$scalingParameter <- varianceParameter
    return(result)
}

#' A function to revert the value that has been done by variance/ .
#' standardization scaling method.
#'
#' This function changes the value of variance scaled dataset that
#' produced by \code{\link{varianceScaling}} function and represented
#' by data.frame object.
#'
#' @title Variance/Standardization Revert Function
#'
#' @param dataSet a data.frame that representing dataset (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataSet
#'        must have at leas two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param varianceParameter a matrix that has value of variance scaling
#'        parameter, such as mean value and standard deviation value of
#'        data that can be used to restore the original value of dataset.
#'        This parameter is exclusively produced by \code{\link{varianceScaling}}
#'        function.
#'
#' @examples
#' ##################################
#' ## Revert Variance Scaling
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' fsr <- varianceScaling(dataSet)
#' rfsr <- varianceDescaling(fsr$scaledDataSet, fsr$scalingParameter)
#'
#' @seealso \code{\link{varianceScaling}}
#'
#' @return a data.frame representing reverted dataset value
#'
#' @export

varianceDescaling <- function(dataSet, varianceParameter){
    columnLength <- ncol(dataSet)
    
    meanValue <- varianceParameter[1,]
    stdValue <- varianceParameter[2,]
    
    descaledDataSet <- (dataSet * stdValue) + meanValue
    result <- descaledDataSet
    return(result)
}

#' A function to do feature scaling to dataset with min-max scaling method.
#'
#' This function changes the value of dataset that represented by data.frame
#' object into min-max scaled value that has interval between 0 to 1.
#'
#' @title The Min-Max Feature Scaling Function
#'
#' @param dataSet a data.frame that representing dataset (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataSet
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @examples
#' ##################################
#' ## Feature scaling with Min-Max Scaling Method
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## do min-max scaling to dataset
#' featureScalingResult <- minmaxScaling(dataSet)
#' ## show result
#' print(featureScalingResult$scaledDataSet)
#' print(featureScalingResult$scalingParameter)
#'
#' @seealso \code{\link{minmaxDescaling}}
#'
#' @return a list contains feature scaled dataset and scaling parameter
#'
#' @export

minmaxScaling <- function(dataSet){
    columnLength <- ncol(dataSet)
    minmaxParameter <- getMinMax(dataSet)
    minValue <- minmaxParameter[1,]
    maxminValue <- minmaxParameter[2,]
    scaledDataSet <- (dataSet-minValue)/(maxminValue)
    result <- list()
    result$scaledDataSet <- scaledDataSet
    result$scalingParameter <- minmaxParameter
    return(result)
}

#' A function to revert the value that has been done by min-max
#' scaling method.
#'
#' This function changes the value of min-max scaled dataset that
#' produced by \code{\link{varianceScaling}} function and represented
#' by data.frame object.
#'
#' @title Min-Max Scaling Revert Function
#'
#' @param dataSet a data.frame that representing dataset (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataSet
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param minmaxParameter a matrix that has value of minmax scaling
#'        parameter, such as minimum value and maximum value of data that
#'        can be used to restore the original value of dataset. This
#'        parameter is exclusively produced by \code{\link{varianceScaling}}
#'        function.
#'
#' @examples
#' ##################################
#' ## Revert Min-Max Scaling
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' fsr <- minmaxScaling(dataSet)
#' rfsr <- minmaxDescaling(fsr$scaledDataSet, fsr$scalingParameter)
#'
#' @seealso \code{\link{minmaxScaling}}
#'
#' @return a data.frame representing reverted dataset value
#'
#' @export

minmaxDescaling <- function(dataSet, minmaxParameter){
    columnLength <- ncol(dataSet)
    
    minValue <- minmaxParameter[1,]
    maxminValue <- minmaxParameter[2,]
    
    descaledDataSet <- (dataSet * maxminValue) + minValue
    result <- descaledDataSet
    return(result)
}

#' A function to split dataset into training and testing data
#'
#' This function split dataset into training and testing data. By default,
#' this function split dataset into 50% of \code{dataTrain} and 50% of
#' \code{dataTest}. You can decide the training data rate by change the
#' value of \code{dataTrainRate}. Example, if you want to set the training
#' data rate by 80%, you can simply set the \code{dataTrainRate} to 0.8.
#' As the remaining of \code{dataTrainRate} value, which is 0.2, will be
#' set as \code{dataTest} rate.
#'
#' @title The Data Spliting Function
#'
#' @param dataSet a data.frame that representing dataset (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataSet
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param dataTrainRate a float number between 0 to 1 representing the
#'        training data rate of given dataset. This parameter has
#'        default value of 0.5.
#'
#' @param seed a integer value for static random. Default value is NULL, which
#'        means the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Splitting Dataset into Training and Testing Data
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' #show result
#' print(splitedDataSet$dataTrain)
#' print(splitedDataSet$dataTest)
#'
#' @return a list contains data.frame of training data and testing data.
#'
#' @export

splitData <- function(dataSet, dataTrainRate=0.5, seed=NULL){
    #get all index in dataSet
    allDataIndex <- 1:nrow(dataSet)
    #set static random
    set.seed(seed)
    #randomize and set dataTrain index
    dataTrainIndex <- sample(allDataIndex, trunc(length(allDataIndex) * dataTrainRate))
    #unset static random
    set.seed(NULL)
    #set dataTrain and dataTest
    dataTrain <- dataSet[dataTrainIndex, ]
    dataTest <- dataSet[-dataTrainIndex, ]
    #wrap result into list
    result <- list()
    result$dataTrain <- dataTrain
    result$dataTest <- dataTest
    return(result)
}

#' A function to predict testing data with built gradient descent model
#'
#' This function used to predict testing data with only input variable named
#' \code{dataTestInput}. The \code{model} parameter is the coefficients
#' that produced by gradient-descent-based learning function. The result of
#' this function is a dataset that contains \code{dataTestInput} combined
#' with prediction data as the last column of dataset.
#'
#' @title Predicting Function for Linear Model
#'
#' @param dataTestInput a data.frame represented dataset with input variables
#'        only (\eqn{m \times n-1}), where \eqn{m} is the number of instances
#'        and \eqn{n} is the number of input variables only.
#'
#' @param model a matrix of coefficients used as a linear model to predict
#'        testing data input. This parameter exclusively produced by the
#'        gradient-descent-based learning function.
#'
#' @examples
#' ##################################
#' ## Predict Testing Data Using GD Model
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## do variance scaling to dataset
#' featureScalingResult <- varianceScaling(dataSet)
#' ## split dataset
#' splitedDataSet <- splitData(featureScalingResult$scaledDataSet)
#' ## built model using GD
#' model <- GD(splitedDataSet$dataTrain)
#' ## separate testing data with input only
#' dataTestInput <- (splitedDataSet$dataTest)[,1:ncol(splitedDataSet$dataTest)-1]
#' ## predict testing data using GD model
#' prediction <- prediction(model,dataTestInput)
#' ## show result()
#' prediction
#'
#'
#' ##################################
#' ## Predict Testing Data Using SARAHPlus Model
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## do variance scaling to dataset
#' featureScalingResult <- varianceScaling(dataSet)
#' ## split dataset
#' splitedDataSet <- splitData(featureScalingResult$scaledDataSet)
#' ## built model using SARAHPlus
#' model <- SARAHPlus(splitedDataSet$dataTrain, alpha=0.1, maxIter=10,
#'          innerIter=10, gammaS=0.125, seed=NULL)
#' ## separate testing data with input only
#' dataTestInput <- (splitedDataSet$dataTest)[,1:ncol(splitedDataSet$dataTest)-1]
#' ## predict testing data using GD model
#' prediction <- prediction(model,dataTestInput)
#' ## show result()
#' prediction
#'
#' @return a data.frame of testing data input variables and prediction variables.
#'
#' @seealso \code{\link{GD}}, \code{\link{MBGD}}, \code{\link{SGD}}, \code{\link{SAGD}},
#'          \code{\link{MGD}}, \code{\link{AGD}}, \code{\link{ADAGRAD}}, \code{\link{ADADELTA}},
#'          \code{\link{RMSPROP}}, \code{\link{ADAM}}, \code{\link{SSGD}}, \code{\link{SVRG}},
#'          \code{\link{SARAH}}, \code{\link{SARAHPlus}}
#'
#' @export

prediction <- function(model, dataTestInput){
    #convert data.frame of dataTestInput in matrix
    dataTestInputMatrix <- matrix(unlist(dataTestInput), ncol=ncol(dataTestInput), byrow=FALSE)
    #bind 1 column into dataTestInput
    dataTestInputMatrix <- cbind(1, dataTestInputMatrix)
    #predict
    prediction <- dataTestInputMatrix %*% t(model)
    #convert prediction into data.frame
    prediction <- as.data.frame(prediction)
    #merge prediction with dataTestInput
    predictionData <- cbind(dataTestInput, prediction)
    #wrap to result
    result <- predictionData
    return(result)
}

#' A function to calculate error using Root-Mean-Square-Error
#'
#' This function used to calculate the error between two variables.
#' \code{outputData} is the first parameter of this function representing
#' the real output value. \code{prediction} is the second parameter of
#' this function representing the prediction value.
#'
#' @title RMSE Calculator Function
#'
#' @param outputData a data.frame represented dataset with output variable
#'        only (\eqn{m \times 1}), where \eqn{m} is the number of instances
#'        has one variable, which is the output.
#'
#' @param prediction a data.frame represented prediction data with output
#'        variable only (\eqn{m \times 1}), where \eqn{m} is the number of
#'        instances has one variable, which is the output.
#'
#' @examples
#' ##################################
#' ## Calculate Error using RMSE
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor Data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## do variance scaling to dataset
#' featureScalingResult <- varianceScaling(dataSet)
#' ## split dataset
#' splitedDataSet <- splitData(featureScalingResult$scaledDataSet)
#' ## built model using GD
#' model <- GD(splitedDataSet$dataTrain)
#' ## separate testing data with input only
#' dataTestInput <- (splitedDataSet$dataTest)[,1:ncol(splitedDataSet$dataTest)-1]
#' ## predict testing data using GD model
#' predictionData <- prediction(model, dataTestInput)
#' ## calculate error using rmse
#' errorValue <- RMSE(splitedDataSet$dataTest[,ncol(splitedDataSet$dataTest)],
#' predictionData[,ncol(predictionData)])
#' ## show result
#' errorValue
#'
#' @return a float value represent the average error of the prediction
#'
#' @export

RMSE <- function(outputData, prediction){
    #get data length
    rowLength <- nrow(cbind(outputData, prediction))
    #calculate root-mean-squared-error
    inner <- (outputData - prediction)^2
    rmse <- sqrt(sum(inner) / rowLength)
    result <- rmse
    return(result)
}

#' @importFrom stats sd runif

getMeanStd <- function(dataSet){
    #create result matrix
    result <- matrix(dataSet, nrow = 2, ncol(dataSet))
    
    #loop the data column
    for(column in 1:ncol(dataSet)) {
        result[1 , column] <- mean(unlist(dataSet[column]))
        result[2 , column] <- sd(unlist(dataSet[column]))
    }
    
    return (result)
}

getMinMax <- function(dataSet){
    #create result matrix
    result <- matrix(dataSet, nrow = 2, ncol(dataSet))
    
    #loop the data column
    for(column in 1:ncol(dataSet)) {
        result[1 , column] <- min(unlist(dataSet[column]))
        result[2 , column] <- max(unlist(dataSet[column])) - min(unlist(dataSet[column]))
    }
    
    return (result)
}

getTheta <- function(columnLength, minTheta=0, maxTheta=1, seed=NULL){
    #create static random
    set.seed(seed)
    #random a value
    thetaList <- runif(columnLength, min=minTheta, max=maxTheta)
    #clear static random
    set.seed(seed)
    #transform into matrix
    result <- matrix(unlist(thetaList), ncol=columnLength, nrow=1, byrow=FALSE)
    return(result)
}

getRandomProb <- function(innerIter=10, lamda=0, alpha=0.1){
    #roulette number
    roulette <- data.frame(1:innerIter)
    #probability tiap number
    probability <- data.frame((1-(lamda*alpha))^(-roulette[1]))
    if(lamda != 0){
        probability <- minmaxScaling(probability)
        #bind roulette and probability
        roulette <- cbind(roulette,probability$scaledDataSet)
        #sort roulette berdasarkan number
        roulette <- roulette[order(roulette[,2],roulette[,1]),]
        #random number for roulette
        pickNumber <- runif(1,0,1)
        #result the number yg terpilih
        i <- 1
        stat <- 0
        while(i <= nrow(roulette) && stat == 0){
            if(pickNumber <= roulette[i,2]){
                result <- roulette[i,1]
                stat <- 1
            }
            i <- i + 1
        }
    }else{
        result <- sample(innerIter,1)
    }
    
    #print(roulette)
    #print(probability)
    #print(pickNumber)
    
    return(result)
}