#' the quadratic fit model
#' take x and y and plot the quadratic fit model
#'
#' @param x independent sample data
#' @param y random variable corresponding to x
#'
#' @return plot of quadratic fit
#' @export
#'
#' @examples df1$COLUMN1; y = df1$COLUMN2; lab4(x,y)
lab4 = function(x,y){
  quad.lm = lm(y~x + I(x^2))

  quadplot=function(x){
    quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
  }

  plot(y~x, col = 'blue')
  curve(quadplot, add = T, col = 'red')

}
