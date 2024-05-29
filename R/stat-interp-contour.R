
##### Contour #####
StatInterpContour <- ggproto("StatInterpContour", ggplot2:::StatContour,
                             compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL, 
                                                      breaks = NULL, na.rm = TRUE,linear = TRUE, rescale=TRUE,dupfun = NULL)  {
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
                                                 z = data$z,
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
                               data.out <-  tibble(x = rep(xout, nrow(ii$z)),
                                                   y = rep(yout, each = ncol(ii$z)),
                                                   z = as.numeric(ii$z), 
                                                   group = 1) 
                               
                               StatContour$compute_group(data.out, scales, z.range, 
                                                         bins, binwidth, breaks, na.rm)
                               
                             },
                             required_aes = c("x", "y", "z")
)


stat_interp_contour<- function(mapping = NULL, data = NULL, geom = "contour",
                               position = "identity", na.rm = FALSE, 
                               show.legend = NA,
                               inherit.aes = TRUE, bins = NULL, binwidth = NULL, 
                               breaks = NULL,  ...) {
  #' @title Create a contour geometry from irregularly spaced data
  #'
  #' @description
  #' This stat is based on [geom_contour()],
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
  #' @importFrom ggplot2 ggproto layer
  #' @export
  #' 
  layer(
    stat = StatInterpContour, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, bins = bins, binwidth = binwidth, 
                  breaks = breaks,  ...)
  )
}

########### Define the same as a geom
GeomInterpContour <- ggproto("GeomInterpContour", GeomContour)

geom_interp_contour <- function(mapping = NULL, data = NULL, 
                               position = "identity", na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, ...) {
  #' @title Create a contour geometry from irregularly spaced data
  #'
  #' @description
  #' This stat is based on [geom_contour()],
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
  #' @importFrom ggplot2 ggproto layer GeomContour
  #' @export
  #' 
  layer(
    stat =StatInterpContour, geom = GeomContour, mapping = mapping,  data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
