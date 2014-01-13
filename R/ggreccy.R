geom_reccy <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) { 
  GeomReccy$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomReccy <- proto(ggplot2:::Geom, {
  objname <- "reccy"
  
  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes(colour=NA, height=0.025, fill="grey20", size=0.5, linetype=1, alpha = NA)
  
  required_aes <- c("x","y")

  draw <- draw_groups <- function(., data, scales, coordinates, ...) {
    if (!is.linear(coordinates)) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "height")
      )

      polys <- alply(data, 1, function(row) {
        poly <- with(row, rect_to_poly(0, time, index, index+0.2))
        aes <- as.data.frame(row[aesthetics], 
          stringsAsFactors = FALSE)[rep(1,5), ]
      
        grid:::GeomPolygon$draw(cbind(poly, aes), scales, coordinates)
      })
      
      ggname("bar",do.call("grobTree", polys))
    } else {
        cat("else..\n")

      with(coord_transform(coordinates, data, scales), 
        ggname(.$my_name(), grid:::rectGrob(
          0, y+height/2, 
          width = x, height = height, 
          default.units = "native", just = c("left", "top"), 
          gp=gpar(
            col=colour, fill=alpha(fill, alpha), 
            lwd=size * .pt, lty=linetype, lineend="butt"
          )
        ))
      )
    }
    
  }
  guide_geom <- function(.) "polygon"

})

# Convert rectangle to polygon
# Useful for non-Cartesian coordinate systems where it's easy to work purely in terms of locations, rather than locations and dimensions.
# 
# @keyword internal
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}
