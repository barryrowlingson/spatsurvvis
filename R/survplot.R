##' plot survival data using ggplot
##'
##' lets use ggplot for this
##' @title ggplot for survival data
##' @param ss a Surv object with right-censoring
##' @return a ggplot object
##' @author Barry S Rowlingson
##' @export
##' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_shape_manual
survplot <- function(ss, covar){
    d = as.data.frame(as(ss,"matrix"))
    d$endtime=d$time
    d$time=0
    d$index=1:nrow(d)
    d$censored = d$status==1
    if(missing(covar)){
        g = ggplot(d,aes(x=time,y=index,xend=endtime,yend=index))
    }else{
        d$covar=covar
        g = ggplot(d,aes(x=time,y=index,xend=endtime,yend=index,colour=covar))
    }
    g + geom_segment() + geom_point(aes(x=endtime,y=index,pch=censored),size=3) + scale_shape_manual(values=c(19,4))
    
}
