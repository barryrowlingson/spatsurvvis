fieldGrob <- function(x, y, angle, length, size, 
                      colour="black", linetype=1, arrow=NULL){
	
    grid:::segmentsGrob(
        x0= x - 0.5*length*cos(angle), 
        y0= y - 0.5*length*sin(angle),
        x1= x + 0.5*length*cos(angle), 
        y1= y + 0.5*length*sin(angle), 
        default.units="native",
        gp = gpar(col=colour, 
            lwd=size*ggplot2:::.pt, 
            lty=linetype, 
            lineend = "butt"), 
        arrow = arrow
        )
}

GeomField <- proto(ggplot2:::Geom, {

 draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
browser()
     with(coordinates$transform(data, scales), 
        fieldGrob(x, y, angle, length, size, 
        col=colour, linetype, arrow)
      )    
  }

 draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
 
    with(data, ggname(.$my_name(),
		fieldGrob(0.5, 0.5, angle, length, size,  col=colour, linetype) )
	)
  }
  objname <- "field" # name of the geom in lowercase. Must correspond to GeomField.
  desc <- "Single line segments"
 
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "angle", "length") 
  default_aes <- function(.) aes(colour="black", angle=pi/4, length=1, size=0.5, linetype=1)
  guide_geom <- function(.) "field"
 
  icon <- function(.) # a grob representing the geom for the webpage
 
  desc_params <- list( # description of the (optional) parameters of draw
	 )
 
  seealso <- list(
    geom_path = GeomPath$desc,
    geom_segment = GeomPath$desc,
    geom_line = GeomLine$desc
  )
 
  examples <- function(.) {
    # examples of the geom in use
  }
  
})

geom_field <- function (mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", arrow = NULL, lineend = "butt", na.rm = FALSE, ...) {

    GeomField$new(mapping = mapping, data = data, stat = stat,
                    position = position, arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
}
