#' Function to calculate the step ahead forecasting for a proposed Prediction method
#'
#' @param dataIn as input time series for testing
#' @param trainedData  as partition point of input data `dataIn`
#' @param MethodPath as as location of function for the proposed imputation method
#' @param errorParameter as type of error calculation (RMSE, MAE or MAPE)
#' @param stepSize as interval at which step by step prediction will be done (Possible values are 1 & 2)
#' @import ggplot2
#' @importFrom stats na.omit
#' @export
#' @return returns the plot for one/two step ahead prediction along with error values decided by `errorParameter`
step_ahead_forecast <- function(dataIn, trainedData, MethodPath, errorParameter, stepSize)
{
  j <- 1
  Predicted <- NULL
  actual <- NULL
  err <- NULL
  index <- NULL
  sd <- NULL
  #stepSize <- 1

  if(!(hasArg(dataIn)))
  {
    dataIn <- c(1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5)
  }

  if(!is.vector(dataIn))
  {
    dataIn <- dataIn[, 1]
  }

  if(!(hasArg(errorParameter)))
  {
    errorParameter <- 1
  }

  if(!(hasArg(stepSize)))
  {
    stepSize <- 1
  }

  if(!(hasArg(trainedData)))
  {
    trainedData <- round(0.75*length(dataIn))
  }

  if(stepSize == 2)
  {
    for(i in seq(trainedData, (length(dataIn)-1), 2))
    {
      testData <- dataIn[1:i]
      testData1 <- dataIn[1:i+1]
      d <- parse(text = MethodPath)
      d <- eval(d)
      d <- d$value(testData)

      Predicted_dummy <- d
      Predicted[j] <- Predicted_dummy[1]
      Predicted[j+1] <- Predicted_dummy[2]
      actual[j] <- dataIn[i+1]
      actual[j+1] <- dataIn[i+2]
      sd[j] <- sd(testData)
      err[j] <- sd[j]
      sd[j+1] <- sd(testData1)
      err[j+1] <- sd[j+1]
      index[j] <- j
      index[j+1] <- j+1
      j <- j + 2
    }
  }

  if(stepSize == 1)
  {
    for(i in trainedData:(length(dataIn)-1))
    {
      testData <- dataIn[1:i]
      d <- parse(text = MethodPath)
      d <- eval(d)
      d <- d$value(testData)

      #Predicted[j] <- pred_for_w(testData, 5, 3, 1)
      Predicted[j] <- d
      actual[j] <- dataIn[i+1]
      sd[j] <- sd(testData)
      err[j] <- sd[j]
      index[j] <- j
      j <- j + 1
    }

  }

  x <- NULL
  Prediction <- NULL
  y2 <- NULL
  y3 <- NULL
  y4 <- NULL

  d <- data.frame(x=index, Prediction = actual, y2= Predicted, y3=(Predicted+err), y4 = (Predicted-err))
  d <- na.omit(d)
  #d1 <- melt(d, id="index")

  jk <- ggplot(d, aes(x)) +
    geom_line(aes(y = Prediction, colour = "Observed Data"), alpha=1, size = 1) + geom_point(aes(y=Prediction)) +
    geom_line(aes(y = y2, colour = "Predicted Data"), alpha=1, size = 1) + geom_point(aes(y=y2)) +
    geom_line(aes(y = y3, colour = "Predicted Data + Std. Deviation"), linetype="dotdash", alpha=0.8) +
    geom_line(aes(y = y4, colour = "Predicted Data - Std. Deviation"), linetype="dotdash", alpha=0.8) +
    scale_color_manual(values= c("black", "blue", "purple", "red"))


  if(errorParameter == 1)
  {
    #ghnew <- rmse(actual - Predicted)
    ghnew <- rmse(d[2] - d[3])
    parameter <- "RMSE Value"
  }
  if(errorParameter == 2)
  {
    #ghnew <- mae(actual - Predicted)
    ghnew <- mae(d[2] - d[3])
    parameter <- "MAE Value"
  }
  if(errorParameter == 3)
  {
    #ghnew <- imputeTestbench::mape((actual - Predicted), actual)
    ghnew <- imputeTestbench::mape((d[2] - d[3]), d[2])
    parameter <- "MAPE Value"
  }
  if(errorParameter[1] == 4)
  {
    newPar <- parse(text = errorParameter[2])
    newPar <- eval(newPar)
    newPar <- newPar$value(d[2], d[3])
    ghnew <- newPar
    parameter <- errorParameter[3]
  }

  existing_method <- list(Plot = jk)

  MethodName <- paste(parameter,"Error", sep = "_")
  existing_method[[paste(parameter)]] <- ghnew

  return(existing_method)
}

