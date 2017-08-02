#' Filter out companies with imcompletle records
#'
#' This function allows you delete and filter out columns with missing values.
#'
#' @param returns the variable name for a uploaded csv file of company returns. 
#'                It may has missing values and can have many company returns 
#'                on one file.
#' 
#' @return data frame of company returns without missing values
#'
#' @export
#' @examples 
#' nmv.returns <- NoMissingValueReturns(returns)
#
#filter missing value function
NoMissingValueReturns <- function(returns) {
  nmv.returns <- data.frame(returns[, colSums(is.na(returns)) == 0])
  nmv.returns
}