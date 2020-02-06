#' Title
#' Compute Z score for outlier detection
#' Input parameter must be a vector
#' @param x
#'
#' @return z score of a vector
#' @export
#'
#' @examples x = df1$COLUMN1; lab2(x)
lab2 = function(x){
  z = (x - mean(x))/sd(x)
  return(z)
}
