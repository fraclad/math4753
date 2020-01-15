#' Title
#'Make a list of sqares and cubes
#'Intro to package making stuff
#' @param x
#'
#' @return a list of vector
#' @export
#'
#' @examples
#' x = 1:4; myf(x)
myf = function(x){
  obj1 = x^2
  obj2 = x^3
  combined = list(obj1, obj2)
  print(combined)
}
