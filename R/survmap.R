##' Map survival data
##'
##' Produce a map of survival times showing censoring
##' 
##' @title Map survival data
##' @param spp a spatial points data frame
##' @param ss a Surv object
##' @return a ggplot object 
##' @author Barry S Rowlingson
##' @export
##' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual coord_fixed
##' @examples
##' require(sp)
##' require(survival)
##' d = data.frame(
##'   x=runif(40)*1.5,
##'   y = runif(40),
##'   age=as.integer(20+30*runif(40)),
##'   sex = sample(c("M","F"),40,TRUE)
##' )
##' coordinates(d)=~x+y
##' d$surv = Surv(as.integer(5+20*runif(40)),runif(40)>.9)
##' survmap(d, d$surv)
##' 
survmap <- function(spp, ss){
    ##
    d = as(spp,"data.frame")
    d$censored = ss[,2]==1
    d$time = ss[,1]
    ggplot(d,aes(x=x,y=y,pch=censored,size=time))+geom_point()+scale_shape_manual(values=c(19,4)) + coord_fixed()
}

