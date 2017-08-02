#' All Linear Regression List of Returns vs. Risk factors
#'
#'     list all regressions from a starting company index
#'     data for returns and risk factors need be free of NAs.
#'
#' @param risk.factors  data frame of risk factors for all companies
#' @param returns       data frame of return records for all companies
#' @param kStaringIndex an integer index
#' @param alfa          float number to indicate the significant level of a risk factor
#' @param sum.reg.fun   function name for specific summary regression
#'
#' @return data frame of regression results for all companies
#'
#' @examples
#' summary.regression <- AllRegressionOutput(risk.factors, returns, 2, 0.05, SummaryRegression)
#'
#' @export
# 
#create function to loop regressions
AllRegressionOutput <- function(risk.factors,returns,kStartingIndex=2, alpha=0.02, sum.reg.fun){
  
  #check if dataframe already exists
  if(exists("summary.regression")){
    rm(summary.regression)
  }

  #first company regression data point for dataframe with looped regressions
  summary.regression <- sum.reg.fun(risk.factors,returns,kStartingIndex,alpha)
  
  #total number of columns in file to loop
  ncolreturns <- ncol(returns)
  
  #starting index to run loop
  startloopindex <- kStartingIndex + 1
 
  #run loop and apend all company regressions to dataframe 
  for(i in startloopindex:ncolreturns) {
    one.sum.reg <- sum.reg.fun(risk.factors,returns,i,alpha)
  
    #rbind datapoints to totaldata
    summary.regression <- rbind(summary.regression, one.sum.reg)
  
  }
  
  #function output
  summary.regression

}