#' Title
#' Compute residual, model, and total sum of squares
#'
#' @param x
#' @param y
#'
#' @return a list containing RSS, MSS, and TSS
#' @export
#'
#' @examples x = df1$COLUMN1; y = df1$COLUMN2; lab3(x,y)
lab3 = function(x,y){

  model.lm = lm(y~x)
  yhat = fitted(model.lm)
  RSS = RSS = sum((y - yhat)^2)

  ref = mean(y)
  MSS = sum((yhat - ref)^2)

  TSS = sum((y - ref)^2)

  result = list('RSS' = RSS, 'MSS' = MSS, 'TSS' = TSS)
  return(result)

}
