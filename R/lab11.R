#' 95% Confidence Inteval
#'
#' Create a 95% confidence interval from a given vector
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples myci(x)
myci = function(x){
  n = length(x)
  tstat = qt(1 - (0.05/2), n-1)
  interval = tstat*sd(x)/(n)^0.5
  low = mean(x) - interval
  up = mean(x) + interval
  output = list('point est' = mean(x), 'low int' = low, 'up int' = up)
  return(output)
}
