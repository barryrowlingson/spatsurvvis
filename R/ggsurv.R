survGrob <- function(time, index, censoring, size=1, colour="black", linetype=1){
    grid:::segmentsGrob(
        x0= rep(0,length(index)),
        y0= index,
        x1= time,
        y1= index,
        default.units="native",
        gp = gpar(col=colour, 
            lwd=size*ggplot2:::.pt, 
            lty=linetype
            ) 
        )
}

GeomSurv = proto(ggplot2:::Geom,{
    objname="surv"
    draw <- function(.,data,scales,coordinates,...){
        with(coord_transform(coordinates, data, scales), 
             ggname(.$my_name(),
                    grid:::pointsGrob(
                        x=time, y=index, size=unit(2, "mm"), pch=19, 
                        gp=gpar(col=colour)
                        )
                    )
             )
        
    }

    guide_geom <- function(.)"line"
    default_stat <- function(.)StatIdentity
    required_aes <- c("time","index")
    default_aes=function(.){aes(colour="black")}
})


geom_surv <- function (mapping = NULL, data = NULL, stat = "identity",
                       position = "identity",  ...) {
    
    GeomSurv$new(mapping = mapping, data = data, stat = stat,
                 position = position, ...)
}
