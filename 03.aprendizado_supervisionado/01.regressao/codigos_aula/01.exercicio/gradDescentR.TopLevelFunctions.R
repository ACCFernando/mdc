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
#' A top-level funtion to generate/learn the model from numerical data
#' using a selected gradient descent method.
#'
#' This function makes accessible all learning methods that are implemented
#' in this package. All of the methods use this function as interface for the learning
#' stage, so users do not need to call other functions in the learning phase.
#' In order to obtain good results, users need to adjust some parameters such as the
#' number of labels, the type of the shape of the membership function, the maximal number of iterations,
#' the step size of the gradient descent, or other method-dependent parameters which are collected in the \code{control}
#' parameter. After creating the model using this function, it can be used to predict new data with \code{\link{predict}}.
#'
#' @title GradDescent Learning Function
#'
#' @param dataSet a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param featureScaling a boolean value that decide to do feature scaling on dataset.
#'        The default value is TRUE, which the function will do feature scaling.
#'
#' @param scalingMethod a string value that represent the feature scaling method to be used.
#'        There are two option for this arguments: \code{"VARIANCE"} and \code{"MINMAX"}.
#'        The default value is \code{"VARIANCE"}, which the function will do Variance Scaling/
#'        Standardization to dataset.
#'
#' @param learningMethod a string value that represent the learning method to do model building.
#'        There are ten option for this arguments: \code{"GD"}, \code{"MBGD"}, \code{"SGD"},
#'        \code{"SAGD"}, \code{"MGD"}, \code{"AGD"}, \code{"ADAGRAD"}, \code{"ADADELTA"},
#'        \code{"RMSPROP"}, \code{"ADAM"}, \code{"SSGD"}, \code{"SVRG"}, \code{"SARAH"}
#'        and \code{"SARAHPlus"}. The default value is \code{"GD"}, which the
#'        function will to Gradient Descent learning.
#'
#' @param control a list containing all arguments, depending on the learning algorithm to use. The following list are
#'                  parameters required for each methods.
#' \itemize{
#'        \item \code{alpha}: a float value in interval of [0,1] that represent the step-size or learning rate
#'              of the learning. The default value is 0.1.
#'        \item \code{maxIter}: a integer value that represent the iteration/loop/epoch of the learning.
#'              The default value is 10, which the function will do 10 times learning calculation.
#'        \item \code{momentum}: a float value represent momentum give a constant speed to learning process.
#'        \item \code{nBatch}: a integer value representing the training data batch.
#'        \item \code{lamda}: a float value to generate random value from innerIter with probability for innerloop.
#'        \item \code{innerIter}: the maximal number of iterations in innerloop.
#'        \item \code{option}: is an option to set the theta. option 1 set the theta with the last theta
#'              in innerloop. option 2 set the theta with random theta from 1 to last innerloop.
#'        \item \code{gammaS}: a float value to provide sufficient reduction. Default value is 0.125.
#'
#' }
#'
#' @param seed a integer value for static random. Default value is NULL, which the
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with GD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## train dataset
#' modelObject <- gradDescentR.learn(dataSet)
#'
#' @return The \code{gradDescentRObject}.
#'
#' @seealso \code{\link{predict}}
#'
#' @export

gradDescentR.learn <- function(dataSet, featureScaling=TRUE, scalingMethod="VARIANCE", learningMethod="GD", control=list(), seed=NULL){
    
    ## checking missing parameters
    control <- setDefaultParametersIfMissing(control, list(alpha=0.1, maxIter=10, momentum=0.9, nBatch=2, lamda=0, innerIter=10, option=2, gammaS=0.125))
    
    ## feature scaling
    if(featureScaling){
        if(scalingMethod == "VARIANCE"){
            scalingResult <- varianceScaling(dataSet)
        }else if(scalingMethod == "MINMAX"){
            scalingResult <- minmaxScaling(dataSet)
        }else{
            stop("unknown scalingMethod argument value")
        }
        dataSet <- scalingResult$scaledDataSet
        scalingParameter <- scalingResult$scalingParameter
    }else{
        scalingParameter <- NULL
    }
    
    alpha <- control$alpha
    maxIter <- control$maxIter
    momentum <- control$momentum
    nBatch <- control$nBatch
    lamda <- control$lamda
    innerIter <- control$innerIter
    option <- control$option
    gammaS <- control$gammaS
    
    ## learning and build model
    if(learningMethod == "GD"){
        model <- GD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed)
    }else if(learningMethod == "MBGD"){
        model <- MBGD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed, nBatch=nBatch)
    }else if(learningMethod == "SGD"){
        model <- SGD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed)
    }else if(learningMethod == "SAGD"){
        model <- SAGD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed)
    }else if(learningMethod == "MGD"){
        model <- MGD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed, momentum=momentum)
    }else if(learningMethod == "AGD"){
        model <- AGD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed, momentum=momentum)
    }else if(learningMethod == "ADAGRAD"){
        model <- ADAGRAD(dataSet, maxIter=maxIter, alpha=alpha, seed=seed)
    }else if(learningMethod == "ADADELTA"){
        model <- ADADELTA(dataSet, maxIter=maxIter, seed=seed, momentum=momentum)
    }else if(learningMethod == "RMSPROP"){
        model <- RMSPROP(dataSet, maxIter=maxIter, alpha=alpha, seed=seed, momentum=momentum)
    }else if(learningMethod == "ADAM"){
        model <- ADAM(dataSet, maxIter=maxIter, alpha=alpha, seed=seed)
    }else if(learningMethod == "SSGD"){
        model <- SSGD(dataSet, maxIter=maxIter, alpha=alpha, lamda=lamda, innerIter=innerIter, seed=seed)
    }else if(learningMethod == "SVRG"){
        model <- SVRG(dataSet, maxIter=maxIter, alpha=alpha, innerIter=innerIter, option=option, seed=seed)
    }else if(learningMethod == "SARAH"){
        model <- SARAH(dataSet, maxIter=maxIter, alpha=alpha, innerIter=innerIter, seed=seed)
    }else if(learningMethod == "SARAHPlus"){
        model <- SARAHPlus(dataSet, maxIter=maxIter, alpha=alpha, innerIter=innerIter, gammaS=gammaS, seed=seed)
    }else{
        stop("unknown learningMethod argument value")
    }
    
    ## create result wrapper list
    result <- list()
    result$featureScaling <- featureScaling
    result$scalingMethod <- scalingMethod
    result$scalingParameter <- scalingParameter
    result$learningMethod <- learningMethod
    result$model <- model
    result$control <- control
    
    ## build into gradDescentRObject class
    result <- gradDescentRObjectFactory(result)
    
    return(result)
}

#' This is the main function to obtain a final result as predicted values for all methods in this package.
#' In order to get predicted values, this function is run using an \code{gradDescentRObject}, which is typically generated using \code{\link{gradDescentR.learn}}.
#'
#' @title The gradDescentR prediction stage
#'
#' @param object an \code{gradDescentRObject}.
#' @param newdata a data frame or matrix (\eqn{m \times n}) of data for the prediction process, where \eqn{m} is the number of instances and
#' \eqn{n} is the number of input variables. It should be noted that the testing data must be expressed in numbers (numerical data).
#' @param ... the other parameters (not used)
#'
#' @seealso \code{\link{gradDescentR.learn}}
#'
#' @return The predicted values.
#'
#' @aliases predict
#'
#' @examples
#' ##################################
#' ## Predict NewData Using Model Object
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## train dataset
#' modelObject <- gradDescentR.learn(dataSet)
#' ## create new data input
#' temp <- c(273.1, 353.1, 363.1)
#' pres <- c(24.675, 24.675, 24.675)
#' conf <- c(0.8066773, 0.9235751, 0.9325948)
#' zfac <- data.frame(temp, pres, conf)
#' ## predict
#' prediction_data <- predict(modelObject, zfac)
#'
#'
#' ##################################
#' ## Predict NewData Using Model Object
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## train dataset
#' modelObject <- gradDescentR.learn(dataSet, featureScaling=TRUE, scalingMethod="VARIANCE",
#'                learningMethod="SARAHPlus", control=list(), seed=NULL)
#' ## create new data input
#' temp <- c(273.1, 353.1, 363.1)
#' pres <- c(24.675, 24.675, 24.675)
#' conf <- c(0.8066773, 0.9235751, 0.9325948)
#' zfac <- data.frame(temp, pres, conf)
#' ## predict
#' prediction_data <- predict(modelObject, zfac)
#'
#' @method predict gradDescentRObject
#' @export

predict.gradDescentRObject <- function(object, newdata, ...){
    if(!inherits(object, "gradDescentRObject")) stop("not a legitimate gradDescentRObject model")
    
    model <- object$model
    
    if(object$featureScaling){
        if(object$scalingMethod == "VARIANCE"){
            varianceParameter <- object$scalingParameter
            meanValue <- varianceParameter[1,]
            stdValue <- varianceParameter[2,]
            newdata <- (newdata-meanValue)/stdValue
        }else if(object$scalingMethod == "MINMAX"){
            minmaxParameter <- object$scalingParameter
            minValue <- minmaxParameter[1,]
            maxminValue <- minmaxParameter[2,]
            newdata <- (newdata-minValue)/(maxminValue)
        }
    }
    
    dataPrediction <- prediction(model, newdata[,1:ncol(newdata)-1])
    
    if(object$featureScaling){
        if(object$scalingMethod == "VARIANCE"){
            dataPrediction <- varianceDescaling(dataPrediction, object$scalingParameter)
        }else if(object$scalingMethod == "MINMAX"){
            dataPrediction <- minmaxDescaling(dataPrediction, object$scalingParameter)
        }
    }
    
    result <- dataPrediction
    return(result)
}

gradDescentRObjectFactory <- function(mod){
    class(mod) <- "gradDescentRObject"
    return(mod)
}

setDefaultParametersIfMissing <- function(control, defaults) {
    for(i in names(defaults)) {
        if(is.null(control[[i]])) control[[i]] <- defaults[[i]]
    }
    control
}