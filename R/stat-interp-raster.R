
##### Raster #####
StatInterpRaster <- ggproto("StatInterpRaster", ggplot2:::Stat,
                            compute_group = function(data, scales,
                                                     linear = TRUE, 
                                                     rescale=TRUE, duplicate="mean",dupfun = NULL) {
                              
                              xx <- data$x
                              yy <- data$y
                              
                              # If appropriate rescale to 0-1:
                              if(rescale){
                                # Scale X
                                xr <- range(xx,na.rm = T)
                                xscale <- xr[2] - xr[1]
                                
                                xx <- (xx - xr[1]) / xscale
                                
                                # Scale Y
                                yr <- range(yy,na.rm = T)
                                yscale <- yr[2] - yr[1]
                                
                                yy <- (yy - yr[1]) / yscale
                                
                              }
                              
                              # Interpolation proper
                              ii<-akima::interp(x = xx,
                                                y = yy,
                                                z = data$fill,
                                                linear=linear,
                                                duplicate=duplicate,
                                                dupfun=dupfun)
                              
                              xout <- ii$x
                              yout <- ii$y
                              
                              # If rescale, recalculate the true coordinates
                              if(rescale){
                                xout <- xout * xscale + xr[1]
                                yout <- yout * yscale + yr[1]
                              }
                              
                              # Return the data as needed
                              data.out <- tibble(x = rep(xout, nrow(ii$z)),
                                                 y = rep(yout, each = ncol(ii$z)),
                                                 fill = as.numeric(ii$z) )
                              
                              return(data.out)
                            },
                            
                            required_aes = c("x", "y", "fill")
)

stat_interp_raster<- function(mapping = NULL, data = NULL, geom = "raster",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  #' @title Create a raster geometry from irregularly spaced data
  #'
  #' @description
  #' This stat is based on [geom_raster()],
  #' and has the same aesthetics, but is able to operate on irregular data
  #'
  #' @details
  #' The function does not like when the range of X and Y data is too dissimilar...
  #' 
  #' @param linear (boolean) if TRUE, use linear interpolation. Else use cubic (see ?akima::interp)
  #' @param duplicate (char) how to deal with duplicates (see ?akima::interp)
  #' @param dupfun (function) if duplicate = "user" (see ?akima::interp)
  #' @param rescale (boolean) if TRUE, rescale X and Y to the 0-1 range (helps in getting a smooth surface)
  #' @param ... arguments passed to base function,
  #' 
  #' @importFrom akima interp
  #' @importFrom ggplot2 ggproto layer
  #' @export
  
  layer(
    stat = StatInterpRaster, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

########### Define the same as a geom
GeomInterpRaster <- ggproto("GeomInterpRaster", GeomRaster)

geom_interp_raster <- function(mapping = NULL, data = NULL, 
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  #' @title Create a raster geometry from irregularly spaced data
  #'
  #' @description
  #' This stat is based on [geom_raster()],
  #' and has the same aesthetics, but is able to operate on irregular data
  #'
  #' @details
  #' The function does not like when the range of X and Y data is too dissimilar...
  #' 
  #' @param linear (boolean) if TRUE, use linear interpolation. Else use cubic (see ?akima::interp)
  #' @param rescale (boolean) if TRUE, rescale X and Y to the 0-1 range (helps in getting a smooth surface)
  #' @param ... arguments passed to base function,
  #' 
  #' @importFrom akima interp
  #' @importFrom ggplot2 ggproto layer GeomRaster
  #' @export
  #' 
  layer(
    stat =StatInterpRaster, geom = GeomRaster, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


  
