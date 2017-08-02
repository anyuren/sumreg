#' Linear Regression of Returns vs. Risk factors
#'
#'     run regression for a given company index
#'     data for returns and risk factors need be free of NAs.
#'
#' @param risk.factors  data frame of risk factors for all companies
#' @param returns       data frame of return records for all companies
#' @param kCompanyIndex an integer index for a company
#' @param alfa          float number to indicate the significant level of a risk factor
#'
#' @return data frame of one-row regression results
#'
#' @examples
#' sr.df <- SummaryRegression(risk.factors, returns, kCompanyIndex, 0.05)
#'
#' @export
# 
#create function to create a row of coefficient datapoints
SummaryRegression <- function(risk.factors,returns,kCompanyIndex,alpha=0.05) {
  
  #test run function
  comp.reg <- SingleRegression(risk.factors,returns,kCompanyIndex)
  
  #summary.lm and taking only coefficients
  sum.comp.reg <- summary(comp.reg)
  coef.comp.reg <- coef(sum.comp.reg)

  #transpose coefficient
  transpose <- t(coef.comp.reg)
  
  #take only first row with coefficients of variables
  firstrow <- transpose[1,]
  
  #take only significant value of variables
  pt <- transpose[4,]

  #using significant value of 0.05 and filtering out only significant values
  sig <- pt > alpha
  firstrow[sig] <- NA
  firstrowdf <- as.data.frame(rbind(firstrow))
  comp.names <- colnames(returns)
  row.names(firstrowdf) <- comp.names[kCompanyIndex]
  firstrowdf
}