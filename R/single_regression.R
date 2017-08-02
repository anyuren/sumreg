#' Linear Regression of Returns vs. Risk factors
#'
#'     run regression for a given company index
#'     data for returns and risk factors need be free of NAs.
#'
#' @param risk.factors  data frame of risk factors for all companies
#' @param returns       data frame of return records for all companies
#' @param kCompanyIndex an integer index for a company
#'
#' @return linear regression model of a fit
#'
#' @examples
#' sr <- SingleRegression(risk.factors, returns, kCompanyIndex)
#'
#' @export
# 
SingleRegression <- function(risk.factors,returns,kCompanyIndex){
  company <- returns[,kCompanyIndex]
  fit <- lm(company~.,risk.factors[,-1])
  fit
}