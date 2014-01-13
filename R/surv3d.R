##' Do a 3d plot of spatial survival data
##'
##' Uses rgl graphics to make a spinny zoomy plot
##' @title Spatial Survival Plot in 3D
##' @param spp A spatial points data frame
##' @param ss A Surv object (with right-censoring)
##' @param lwd Line width for stems
##' @param lcol Line colour for stems
##' @param size Vector of length 2 for uncensored/censored points
##' @param pcol Vector of length 2 for uncensored/censored points
##' @param title Main title for plot
##' @param basegrid add a grid at t=0
##' @param baseplane add a plane at t=0
##' @return 
##' @author Barry S Rowlingson
surv3d <- function(spp, ss, lwd=2, lcol="black", size=c(20,10), pcol=c("red","black"), title="Spatial Survival", basegrid=FALSE, baseplane=FALSE){

    nr = nrow(spp)
    xy = rbind(coordinates(spp),coordinates(spp))
    xyz = cbind(xy,c(rep(0,nr), d$surv[,"time"]))

    # weave the lines
    xyz = xyz[rep(1:nr,rep(2,nr))+rep(c(0,nr),nr),]

    # segments3d takes pairs for line segments
    segments3d(xyz,lwd=lwd, col=lcol)

    # add points for uncensored obs
    unc = ss[,"status"] == 1
    xyp = cbind(coordinates(spp),d$surv[,"time"])
    text3d(xyp[unc,],texts="X",col=pcol[1])
###points3d(xyp[unc,], size=size[1], col=pcol[1])

###    points3d(xyp[!unc,], size=size[2], col=pcol[2])
    aspect3d(c(1,1,1))
    title3d(main=title,xlab='x',ylab='y',zlab='Time')
    axes3d()

    ## minimum visible z coord (zero gets clipped)
    zminvis = min(d$surv[,"time"])/100    

    if (basegrid){
        x=seq(min(xyp[,1]),max(xyp[,1]),len=20)
        y=seq(min(xyp[,2]),max(xyp[,2]),len=20)
        abclines3d(x,min(y),zminvis, a=0, b= 1, c=0, col="gray")
        abclines3d(min(x),y,zminvis, a=1, b= 0, c=0, col="gray")
    }
    if(baseplane){
        planes3d(0,0,1,-zminvis,col="gray",alpha=0.5)
    }
    invisible(0)
}
