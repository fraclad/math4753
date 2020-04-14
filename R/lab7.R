#' Two Population Chi Sq Statistics
#'
#' @param n1 n sample of y1
#' @param n2 n sample of y2
#' @param sigma1 std dev of y1
#' @param sigma2 std dev of y2
#' @param mean1 mean of y1
#' @param mean2 mean of y2
#' @param iter number of iteration
#' @param xleg enable user toggle legend by click on the graphic output
#' @param ymax max of y, for visual clearance
#' @param ...
#'
#' @return Chi sq statistics graphics
#' @export
#'
#' @examples lab7chisim2(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000, ...)
lab7chisim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000, xleg = 12, ymax=0.07,...){    # adjust ymax to make graph fit

  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)

  ssq1=apply(data1.mat,2,var) # ssq1 is s squared

  ssq2=apply(data2.mat,2,var)

  spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2) # pooled s squared

  w=(n1+n2-2)*spsq/(sigma1^2)#sigma1=sigma2,  Chi square stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)

  lines(density(w),col="Blue",lwd=3) # add a density plot

  curve(dchisq(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  title=expression(chi^2==frac((n[1]+n[2]-2)*S[p]^2,sigma^2)) #mathematical annotation -see ?plotmath

  #legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  legend(x = xleg, y = ymax - 0.01 ,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)

  invisible(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}

