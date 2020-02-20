#' Title
#'
#' @param mu
#' @param sigma
#' @param x
#'
#' @return
#' @export
#'
#' @examples myncurve(10,1,7)
myncurve = function(mu, sigma, x){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  lowlim = x - 2000
  xcurve = seq(lowlim,x,length =10000)
  ycurve = dnorm(xcurve, mu,sigma)

  polygon(c(lowlim,xcurve,x), c(0,ycurve,0), col = '#4cbded')

  Area = pnorm(x, mu,sigma)
  Area = round(Area, 4)
  text(x-0.5,0.05, paste0('Area= ', Area))

}
