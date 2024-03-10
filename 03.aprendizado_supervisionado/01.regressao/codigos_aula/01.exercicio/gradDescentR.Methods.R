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
#' A function to build prediction model using Gradient Descent method.
#'
#' This function build a prediction model using Gradient Descent (GD) method.
#' Gradient Descent is a first order optimization algorithm to find a local
#' minimum of an objective function by searching along the steepest descent
#' direction. In machine learning, it is mostly used for dealing with supervised
#' learning, which is regression task. By using GD, we construct a model
#' represented in a linear equation that maps the relationship between input
#' variables and the output one. In other words, GD determines suitable coefficient
#' of each variables. So, that the equation can express the mapping correctly.
#'
#' @title Gradient Descent (GD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with GD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with GD
#' GDmodel <- GD(splitedDataSet$dataTrain)
#' #show result
#' print(GDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{MBGD}}
#'
#' @references
#' L.A. Cauchy,
#' "Methode generale pour la resolution des systemes d equations",
#' Compte Rendu a l Academie des Sciences 25,
#' pp. 536-538 (1847)
#'
#' @export

GD <- function(dataTrain, alpha=0.1, maxIter=10, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle data train
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    #updateRule <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData %*% t(theta)) - outputData
        for(column in 1:length(theta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient <- sum(term) / rowLength
            temporaryTheta[1,column] = theta[1,column] - (alpha*gradient)
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using Mini-Batch Gradient Descent (MBGD) method.
#'
#' This function based on \code{\link{GD}} method with optimization to use
#' the training data partially. MBGD has a parameter named batchRate that represent
#' the instances percentage of training data.
#'
#' @title Mini-Batch Gradient Descent (MBGD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param nBatch a integer value representing the training data batch.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with MBGD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with 0.8 batch rate MBGD
#' MBGDmodel <- MBGD(splitedDataSet$dataTrain, nBatch=2)
#' #show result
#' print(MBGDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{GD}}
#'
#' @references
#' A. Cotter, O. Shamir, N. Srebro, K. Sridharan
#' Better Mini-Batch Algoritms via Accelerated Gradient Methods,
#' NIPS,
#' pp. 1647- (2011)
#'
#' @export

MBGD <- function(dataTrain, alpha=0.1, maxIter=10, nBatch=2, seed=NULL){
    #convert nBatch to batchRate
    batchRate <- 1/nBatch;
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    #updateRule <- matrix(0, ncol=length(theta), nrow=1)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        #split dataTrain to Batch
        if(iteration %% nBatch == 1){
            temp <- 1
            x <- nrow(dataTrain)*batchRate
            temp2 <- x
        }else if(nBatch == 1){
            temp <- 1
            x <- nrow(dataTrain)*batchRate
            temp2 <- x
        }
        #dataTrain batch
        batch <- dataTrain[temp:temp2,]
        #parse dataTrain into input and output
        inputData <- batch[,1:ncol(batch)-1]
        outputData <- batch[,ncol(batch)]
        #constant variables
        rowLength <- nrow(batch)
        #next batch
        temp <- temp + x
        temp2 <- temp2 + x
        
        error <- (inputData %*% t(theta)) - outputData
        for(column in 1:length(theta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient <- sum(term) / rowLength
            temporaryTheta[1,column] = theta[1,column] - (alpha*gradient)
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using Stochastic Gradient Descent (SGD) method.
#'
#' This function based on \code{\link{GD}} method with optimization to use only one instance
#' of training data stochasticaly. So, SGD will perform fast computation and the learning.
#' However, the learning to reach minimum cost will become more unstable.
#'
#' @title Stochastic Gradient Descent (SGD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with SGD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with SGD
#' SGDmodel <- SGD(splitedDataSet$dataTrain)
#' #show result
#' print(SGDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{SAGD}}
#'
#' @references
#' N. Le Roux, M. Schmidt, F. Bach
#' A Stochastic Gradient Method with an Exceptional Convergence Rate for Finite Training Sets,
#' Advances in Neural Information Processing Systems,
#' (2011)
#'
#' @export

SGD <- function(dataTrain, alpha=0.1, maxIter=10, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    # updateRule <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    stochasticList <- sample(1:rowLength, maxIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData[stochasticList[iteration],] %*% t(theta)) - outputData[stochasticList[iteration]]
        for(column in 1:length(theta)){
            #calculate gradient
            gradient <- error * inputData[stochasticList[iteration], column]
            temporaryTheta[1,column] = theta[1,column] - (alpha*gradient)
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using Stochastic Average Gradient Descent (SAGD) method.
#'
#' This function based on \code{\link{SGD}} that only compute one instances of
#' of training data stochasticaly. But \code{SAGD} has an averaging control optimization
#' to decide between do the coefficient update or not randomly. This optimization
#' will speed-up the learning, if it doesn't perform computation and
#' update the coefficient.
#'
#' @title Stochastic Average Gradient Descent (SAGD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with SAGD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with SAGD
#' SAGDmodel <- SAGD(splitedDataSet$dataTrain)
#' #show result
#' print(SAGDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{SGD}}
#'
#' @references
#' M. Schmidt, N. Le Roux, F. Bach
#' Minimizing Finite Sums with the Stochastic Average Gradient,
#' INRIA-SIERRA Project - Team Departement d'informatique de l'Ecole Normale Superieure,
#' (2013)
#'
#' @export

SAGD <- function(dataTrain, alpha=0.1, maxIter=10, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    # updateRule <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    stochasticList <- sample(1:rowLength, maxIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        #stochastic average randomization
        if(sample(0:1,1) == 1){
            error <- (inputData[stochasticList[iteration],] %*% t(theta)) - outputData[stochasticList[iteration]]
            for(column in 1:length(theta)){
                #calculate gradient
                gradient <- error * inputData[stochasticList[iteration], column]
                temporaryTheta[1,column] = theta[1,column] - (alpha*gradient)
            }
            #update all theta in the current iteration
            theta <- temporaryTheta
        }
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using Momentum Gradient Descent (MGD) method.
#'
#' This function based on \code{\link{SGD}} with an optimization to speed-up the learning
#' by adding a constant momentum.
#'
#' @title Momentum Gradient Descent (MGD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param momentum a float value represent momentum give a constant speed to learning process.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with MGD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with MGD
#' MGDmodel <- MGD(splitedDataSet$dataTrain)
#' #show result
#' print(MGDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{AGD}}
#'
#' @references
#' N. Qian
#' On the momentum term in gradient descent learning algorithms.,
#' Neural networks : the official journal of the International Neural Network Society,
#' pp. 145-151- (1999)
#'
#' @export

MGD <- function(dataTrain, alpha=0.1, maxIter=10, momentum=0.9, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    updateRule <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData %*% t(theta)) - outputData
        for(column in 1:length(theta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient <- sum(term) / rowLength
            updateRule[1,column] <-  (momentum*updateRule[1,column]) + (alpha*gradient)
            temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using Accelerated Gradient Descent (AGD) method.
#'
#' This function based on \code{\link{SGD}} and \code{\link{MGD}} with optimization
#' to accelerate the learning with momentum constant in each iteration.
#'
#' @title Accelerated Gradient Descent (AGD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param momentum a float value represent momentum give a constant speed to learning process.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with AGD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with AGD
#' AGDmodel <- AGD(splitedDataSet$dataTrain)
#' #show result
#' print(AGDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{MGD}}
#'
#' @references
#' Y. Nesterov
#' A method for unconstrained convex minimization problem with the rate of convergence O (1/k2),
#' Soviet Mathematics Doklady 27 (2),
#' pp. 543-547 (1983)
#'
#' @export

AGD <- function(dataTrain, alpha=0.1, maxIter=10, momentum=0.9, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    updateRule <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        #accelerate
        theta <- theta - (updateRule * momentum)
        error <- (inputData %*% t(theta)) - outputData
        for(column in 1:length(theta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient <- sum(term) / rowLength
            updateRule[1,column] <-  (momentum*updateRule[1,column]) + (alpha*gradient)
            temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using ADAGRAD method.
#'
#' This function based on \code{\link{SGD}} with an optimization to create
#' an adaptive learning rate with an approach that accumulate previous cost in each iteration.
#'
#' @title ADAGRAD Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with ADAGRAD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with ADAGRAD
#' ADAGRADmodel <- ADAGRAD(splitedDataSet$dataTrain)
#' #show result
#' print(ADAGRADmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{ADADELTA}}, \code{\link{RMSPROP}}, \code{\link{ADAM}}
#'
#' @references
#' J. Duchi, E. Hazan, Y. Singer
#' Adaptive Subgradient Methods for Online Learning and Stochastic Optimization,
#' Journal of Machine Learning Research 12,
#' pp. 2121-2159 (2011)
#'
#' @export

ADAGRAD <- function(dataTrain, alpha=0.1, maxIter=10, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    updateRule <- matrix(0, ncol=length(theta), nrow=1)
    gradientList <- matrix(nrow=1, ncol=0)
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    stochasticList <- sample(1:rowLength, maxIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData[stochasticList[iteration],] %*% t(theta)) - outputData[stochasticList[iteration]]
        for(column in 1:length(theta)){
            #calculate gradient
            gradient <- error * inputData[stochasticList[iteration], column]
            #adagrad update rule calculation
            gradientList <- cbind(gradientList, gradient)
            gradientSum <- sqrt(gradientList %*% t(gradientList))
            updateRule[1,column] <- (alpha / gradientSum) * gradient
            temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using ADADELTA method.
#'
#' This function based on \code{\link{SGD}} with an optimization to create
#' an adaptive learning rate by hessian approximation correction approach.
#' Correction and has less computation load than \code{\link{ADAGRAD}}. This method
#' create an exclusive learning rate and doesn't need \code{alpha} parameter, but uses
#' momentum parameter same as \code{\link{MGD}} and \code{\link{AGD}}.
#'
#' @title ADADELTA Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param momentum a float value represent momentum give a constant speed to learning process.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with ADADELTA
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with ADADELTA
#' ADADELTAmodel <- ADADELTA(splitedDataSet$dataTrain)
#' #show result
#' print(ADADELTAmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{ADAGRAD}}, \code{\link{RMSPROP}}, \code{\link{ADAM}}
#'
#' @references
#' M. D. Zeiler
#' Adadelta: An Adaptive Learning Rate Method,
#' arXiv: 1212.5701v1,
#' pp. 1-6 (2012)
#'
#' @export

ADADELTA <- function(dataTrain, maxIter=10, momentum=0.9, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    updateRule <- matrix(0, ncol=length(theta), nrow=1)
    ESG <- 0
    ESR <- 0
    RMSUpdate <- 0
    smooth <- 0.0000001
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    stochasticList <- sample(1:rowLength, maxIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData[stochasticList[iteration],] %*% t(theta)) - outputData[stochasticList[iteration]]
        for(column in 1:length(theta)){
            #calculate gradient
            gradient <- error * inputData[stochasticList[iteration], column]
            #adadelta update rule calculation
            ESG <- (momentum*ESG) + (1-momentum)*gradient^2
            RMSGradient <- sqrt(ESG + smooth)
            ESR <- (momentum*ESR) + (1-momentum)*updateRule[1,column]^2
            updateRule[1,column] <- (RMSUpdate / RMSGradient) * gradient
            #temporary change
            temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
            #adadelta temporary change
            RMSUpdate <- sqrt(ESR + smooth)
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using RMSPROP method.
#'
#' This function based on \code{\link{SGD}} with an optimization to create
#' an adaptive learning rate by RMS cost and hessian approximation correction approach.
#' In other word, this method combine the \code{\link{ADAGRAD}} and \code{\link{ADADELTA}}
#' approaches.
#'
#' @title ADADELTA Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param momentum a float value represent momentum give a constant speed to learning process.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with RMSPROP
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with RMSPROP
#' RMSPROPmodel <- RMSPROP(splitedDataSet$dataTrain)
#' #show result
#' print(RMSPROPmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{ADAGRAD}}, \code{\link{ADADELTA}}, \code{\link{ADAM}}
#'
#' @references
#' M. D. Zeiler
#' Adadelta: An Adaptive Learning Rate Method,
#' arXiv: 1212.5701v1,
#' pp. 1-6 (2012)
#'
#' @export

RMSPROP <- function(dataTrain, alpha=0.1, maxIter=10, momentum=0.9, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    updateRule <- matrix(0, ncol=length(theta), nrow=1)
    ESG <- 0
    smooth <- 0.0000001
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    stochasticList <- sample(1:rowLength, maxIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData[stochasticList[iteration],] %*% t(theta)) - outputData[stochasticList[iteration]]
        for(column in 1:length(theta)){
            #calculate gradient
            gradient <- error * inputData[stochasticList[iteration], column]
            #rmsprop update rule calculation
            ESG <- (momentum*ESG) + (1-momentum)*gradient^2
            RMSGradient <- sqrt(ESG + smooth)
            updateRule[1,column] <- (alpha / RMSGradient) * gradient
            #temporary change
            temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using ADAM method.
#'
#' This function based on \code{\link{SGD}} with an optimization to create
#' an adaptive learning rate by two moment estimation called mean and variance.
#'
#' @title ADADELTA Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with ADAM
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with ADAM
#' ADAMmodel <- ADAM(splitedDataSet$dataTrain)
#' #show result
#' print(ADAMmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{ADAGRAD}}, \code{\link{RMSPROP}}, \code{\link{ADADELTA}}
#'
#' @references
#' D.P Kingma, J. Lei
#' Adam: a Method for Stochastic Optimization,
#' International Conference on Learning Representation,
#' pp. 1-13 (2015)
#'
#' @export

ADAM <- function(dataTrain, alpha=0.1, maxIter=10, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    updateRule <- matrix(0, ncol=length(theta), nrow=1)
    beta1 <- 0.9
    beta2 <- 0.999
    meanMoment <- 0
    varianceMoment <- 0
    smooth <- 0.0000001
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    stochasticList <- sample(1:rowLength, maxIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        error <- (inputData[stochasticList[iteration],] %*% t(theta)) - outputData[stochasticList[iteration]]
        for(column in 1:length(theta)){
            #calculate gradient
            gradient <- error * inputData[stochasticList[iteration], column]
            #adam update rule calculation
            meanMoment <- (beta1*meanMoment) + (1-beta1)*gradient
            varianceMoment <- (beta2*varianceMoment) + (1-beta2)*(gradient^2)
            mean.hat <- meanMoment/(1-beta1)
            variance.hat <- varianceMoment/(1-beta2)
            updateRule[1,column] <- (alpha/(sqrt(variance.hat)+smooth)) * mean.hat
            #temporary change
            temporaryTheta[1,column] = theta[1,column] - updateRule[1,column]
        }
        #update all theta in the current iteration
        theta <- temporaryTheta
    }
    result <- theta
    return(result)
}

#' A function to build prediction model using SVRG method.
#'
#' This function based on \code{\link{SGD}} with an optimization that accelerates
#' the process toward converging by reducing the gradient in \code{\link{SGD}}
#'
#' @title Stochastic Variance Reduce Gradient (SVRG) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations in outerloop.
#'
#' @param innerIter the maximal number of iterations in innerloop.
#'
#' @param option is an option to set the theta. option 1 set the theta with the last theta
#'        in innerloop. option 2 set the theta with random theta from 1 to last innerloop.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with SVRG
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with SVRG
#' SVRGmodel <- SVRG(splitedDataSet$dataTrain)
#' #show result
#' print(SVRGmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{SSGD}}, \code{\link{SARAH}}, \code{\link{SARAHPlus}}
#'
#' @references
#' Rie Johnson, Tong Zang
#' Accelerating Stochastic Gradient Descent using Predictive Variance Reduction,
#' Advances in Neural Information Processing Systems,
#' pp. 315-323 (2013)
#'
#' @export

#variance baru (SVRG)
SVRG <- function(dataTrain, alpha=0.1, maxIter=10, innerIter=10, option=2, seed=NULL){
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #print(theta[1,1])
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    temporaryTheta2 <- matrix(ncol=length(theta), nrow=1)
    temporaryThetaList <- matrix(ncol=length(theta))
    #updateRule <- matrix(0, ncol=length(theta), nrow=1)
    gradient <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    randRowList <- sample(1:rowLength, innerIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        
        temporaryTheta <- theta
        
        error <- (inputData %*% t(temporaryTheta)) - outputData
        for(column in 1:length(temporaryTheta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient[,column] <- sum(term) / rowLength
        }
        
        temporaryTheta2 <- temporaryTheta
        temporaryThetaList <- temporaryTheta2
        
        for(innerIteration in 1:(innerIter)){
            error2 <- (inputData[randRowList[innerIteration],] %*% t(temporaryTheta2)) - outputData[randRowList[innerIteration]]
            error3 <- (inputData[randRowList[innerIteration],] %*% t(temporaryTheta)) - outputData[randRowList[innerIteration]]
            
            for(column in 1:length(theta)){
                term2 <- error2 * inputData[randRowList[innerIteration],column]
                term3 <- error3 * inputData[randRowList[innerIteration],column]
                
                totalGrad <- term2 - term3 + gradient[,column]
                
                temporaryTheta2[1,column] <- temporaryTheta2[1,column] - (alpha*totalGrad)
            }
            
            temporaryThetaList <- rbind(temporaryThetaList, temporaryTheta2)
        }
        
        randInnerIter <- sample(innerIter,1)
        if(option == 1){
            theta[1,] <- temporaryThetaList[innerIter,]
        }else{
            theta[1,] <- temporaryThetaList[randInnerIter,]
        }
    }
    
    result <- theta
    return(result)
}

#' A function to build prediction model using SSGD method.
#'
#' This function combines elements from both \code{\link{GD}} and \code{\link{SGD}}. \code{\link{SSGD}} starts by
#' computing the full gradient once and then proceeds with stochastic updates by choosing one of
#' the gradients at a time.
#'
#' @title Semi Stochastic Gradient Descent (SSGD) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations in outerloop.
#'
#' @param innerIter the maximal number of iterations in innerloop.
#'
#' @param lamda a float value to generate random value from innerIter with probability for innerloop.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with SSGD
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with SSGD
#' SSGDmodel <- SSGD(splitedDataSet$dataTrain)
#' #show result
#' print(SSGDmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{SVRG}}, \code{\link{SARAH}}, \code{\link{SARAHPlus}}
#'
#' @references
#' George Papamakarios
#' Comparison of Modern Stochastic Optimization Algorithms,
#' (2014)
#'
#' @export

#variance baru (SSGD)
SSGD <- function(dataTrain, alpha=0.1, maxIter=10, lamda=0, innerIter=10, seed=NULL){
    start_time <- Sys.time()
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    #updateRule <- matrix(0, ncol=length(theta), nrow=1)
    gradient <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    #loop the gradient descent
    for (iteration in 1:maxIter) {
        error <- (inputData %*% t(theta)) - outputData
        for(column in 1:length(theta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient[,column] <- sum(term) / rowLength
        }
        
        #temporary theta
        themp <- theta
        
        #random innerIteration
        randIter <- getRandomProb(innerIter,lamda,alpha)
        #print(randIter)
        
        for (innerIteration in 1:randIter) {
            #choose random of row
            randRow <- sample(rowLength,1)
            
            error2 <- (inputData[randRow,] %*% t(themp)) - outputData[randRow]
            error3 <- (inputData[randRow,] %*% t(theta)) - outputData[randRow]
            
            for(column in 1:length(theta)){
                term2 <- error2 * inputData[randRow,column]
                term3 <- error3 * inputData[randRow,column]
                
                totalGrad <- gradient[,column] - term2 + term3
                
                themp[1,column] <- themp[1,column] - (alpha*totalGrad)
            }
        }
        theta <- themp
    }
    result <- theta
    end_time <- Sys.time()
    print(end_time - start_time)
    return(result)
}

#' A function to build prediction model using SARAH method.
#'
#' This function Similarly to \code{\link{SVRG}}, \code{\link{SARAH}} iterations are divided
#' into the outer loop where a full gradient is computed and the inner loop where only
#' stochastic gradient is computed. Unlike the case of \code{\link{SVRG}}, the steps of
#' the inner loop of \code{\link{SARAH}} are based on accumulated stochastic information.
#'
#' @title Stochastic Recursive Gradient Algorithm (SARAH) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations in outerloop.
#'
#' @param innerIter the maximal number of iterations in innerloop.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with SARAH
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with SARAH
#' SARAHmodel <- SARAH(splitedDataSet$dataTrain)
#' #show result
#' print(SARAHmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{SVRG}}, \code{\link{SSGD}}, \code{\link{SARAHPlus}}
#'
#' @references
#' Lam M. Nguyen, Jie Lu, Katya Scheinberg, Martin Takac
#' SARAH: A Novel Method for Machine Learning Problems Using Stochastic Recursive Gradient,
#' arXiv preprint arXiv:1703.00102,
#' (2017)
#'
#' @export

#variance baru (SARAH)
SARAH <- function(dataTrain, alpha=0.1, maxIter=10, innerIter=10, seed=NULL){
    start_time <- Sys.time()
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #print(theta[1,1])
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    temporaryTheta2 <- matrix(ncol=length(theta), nrow=1)
    temporaryThetaList <- matrix(ncol=length(theta))
    #updateRule <- matrix(0, ncol=length(theta), nrow=1)
    gradient <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    randRowList <- sample(1:rowLength, innerIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        
        temporaryTheta <- theta
        
        error <- (inputData %*% t(temporaryTheta)) - outputData
        for(column in 1:length(temporaryTheta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient[,column] <- sum(term) / rowLength
            temporaryTheta2[,column] <- temporaryTheta[,column] - (alpha*gradient[,column])
        }
        
        temporaryThetaList <- temporaryTheta
        
        for(innerIteration in 1:(innerIter-1)){
            error2 <- (inputData[randRowList[innerIteration],] %*% t(temporaryTheta2)) - outputData[randRowList[innerIteration]]
            error3 <- (inputData[randRowList[innerIteration],] %*% t(temporaryTheta)) - outputData[randRowList[innerIteration]]
            
            temporaryTheta <- temporaryTheta2
            
            for(column in 1:length(theta)){
                term2 <- error2 * inputData[randRowList[innerIteration],column]
                term3 <- error3 * inputData[randRowList[innerIteration],column]
                
                gradient[,column] <- term2 - term3 + gradient[,column]
                
                temporaryTheta2[1,column] <- temporaryTheta2[1,column] - (alpha*gradient[,column])
            }
            
            temporaryThetaList <- rbind(temporaryThetaList, temporaryTheta2)
        }
        
        randInnerIter <- sample(innerIter,1)
        theta[1,] <- temporaryThetaList[randInnerIter,]
    }
    
    result <- theta
    end_time <- Sys.time()
    print(end_time - start_time)
    return(result)
}

#' A function to build prediction model using SARAH+ method.
#'
#' This function is practical variant of \code{\link{SARAH}}, \code{\link{SARAHPlus}} provides a possibility of
#' earlier termination and unnecessary careful choices of maximum innerloop size, and it also covers the
#' classical gradient descent when we set gammaS = 1 (since the while loop does not proceed).
#'
#' @title Stochastic Recursive Gradient Algorithm+ (SARAH+) Method Learning Function
#'
#' @param dataTrain a data.frame that representing training data (\eqn{m \times n}),
#'        where \eqn{m} is the number of instances and \eqn{n} is the number
#'        of variables where the last column is the output variable. dataTrain
#'        must have at least two columns and ten rows of data that contain
#'        only numbers (integer or float).
#'
#' @param alpha a float value representing learning rate. Default value is 0.1
#'
#' @param maxIter the maximal number of iterations in outerloop.
#'
#' @param innerIter the maximal number of iterations in innerloop.
#'
#' @param gammaS a float value to provide sufficient reduction. Default value is 0.125.
#'
#' @param seed a integer value for static random. Default value is NULL, which means
#'        the function will not do static random.
#'
#' @examples
#' ##################################
#' ## Learning and Build Model with SARAH+
#' ## load R Package data
#' data(gradDescentRData)
#' ## get z-factor data
#' dataSet <- gradDescentRData$CompressilbilityFactor
#' ## split dataset
#' splitedDataSet <- splitData(dataSet)
#' ## build model with SARAH+
#' SARAHPlusmodel <- SARAHPlus(splitedDataSet$dataTrain)
#' #show result
#' print(SARAHPlusmodel)
#'
#' @return a vector matrix of theta (coefficient) for linear model.
#'
#' @seealso \code{\link{SVRG}}, \code{\link{SSGD}}, \code{\link{SARAH}}
#'
#' @references
#' Lam M. Nguyen, Jie Lu, Katya Scheinberg, Martin Takac
#' SARAH: A Novel Method for Machine Learning Problems Using Stochastic Recursive Gradient,
#' arXiv preprint arXiv:1703.00102,
#' (2017)
#'
#' @export

#variance baru (SARAH+)
SARAHPlus <- function(dataTrain, alpha=0.1, maxIter=10, innerIter=10, gammaS=0.125, seed=NULL){
    start_time <- Sys.time()
    #convert data.frame dataSet in matrix
    dataTrain <- matrix(unlist(dataTrain), ncol=ncol(dataTrain), byrow=FALSE)
    #shuffle dataTrain
    set.seed(seed)
    dataTrain <- dataTrain[sample(nrow(dataTrain)), ]
    set.seed(NULL)
    #initialize theta
    theta <- getTheta(ncol(dataTrain), seed=seed)
    #print(theta[1,1])
    #bind 1 column to dataTrain
    dataTrain <- cbind(1, dataTrain)
    #parse dataTrain into input and output
    inputData <- dataTrain[,1:ncol(dataTrain)-1]
    outputData <- dataTrain[,ncol(dataTrain)]
    #temporary variables
    temporaryTheta <- matrix(ncol=length(theta), nrow=1)
    temporaryTheta2 <- matrix(ncol=length(theta), nrow=1)
    temporaryThetaList <- matrix(ncol=length(theta))
    #updateRule <- matrix(0, ncol=length(theta), nrow=1)
    gradient <- matrix(0, ncol=length(theta), nrow=1)
    #constant variables
    rowLength <- nrow(dataTrain)
    set.seed(seed)
    randRowList <- sample(1:rowLength, innerIter, replace=TRUE)
    set.seed(NULL)
    #loop the gradient descent
    for(iteration in 1:maxIter){
        
        temporaryTheta <- theta
        
        error <- (inputData %*% t(temporaryTheta)) - outputData
        for(column in 1:length(temporaryTheta)){
            term <- error * inputData[,column]
            #calculate gradient
            gradient[,column] <- sum(term) / rowLength
            temporaryTheta2[,column] <- temporaryTheta[,column] - (alpha*gradient[,column])
        }
        firstGradient <- gradient
        
        temporaryThetaList <- temporaryTheta
        
        iter <- 1
        
        while(((abs(colSums(gradient)/3))^2) > (gammaS*((abs(colSums(firstGradient)/3))^2)) && iter < innerIter){
            
            error2 <- (inputData[randRowList[iter],] %*% t(temporaryTheta2)) - outputData[randRowList[iter]]
            error3 <- (inputData[randRowList[iter],] %*% t(temporaryTheta)) - outputData[randRowList[iter]]
            
            temporaryTheta <- temporaryTheta2
            
            for(column in 1:length(theta)){
                term2 <- error2 * inputData[randRowList[iter],column]
                term3 <- error3 * inputData[randRowList[iter],column]
                
                gradient[,column] <- term2 - term3 + gradient[,column]
                
                temporaryTheta2[1,column] <- temporaryTheta2[1,column] - (alpha*gradient[,column])
            }
            
            temporaryThetaList <- rbind(temporaryThetaList, temporaryTheta2)
            
            iter <- iter + 1
        }
        
        theta[1,] <- temporaryThetaList[iter,]
    }
    
    result <- theta
    end_time <- Sys.time()
    print(end_time - start_time)
    return(result)
}