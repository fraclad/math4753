#' Mean and Standard Deviation from a vector
#' compute mean and standard deviation from a given vector
#' @param x a vector of interest
#'
#' @return a list of mean and standard deviation
#' @export
#'
#' @examples x = c(1,6,3,1,8); lab1(x)
lab1 = function(x){
  m = mean(x)
  s = sd(x)
  result = list('mean ' = m, 'std dev' = s)
  return(result)
}
