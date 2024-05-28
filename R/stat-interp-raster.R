
##### Raster #####
StatInterpRaster <- ggproto("StatInterpRaster", ggplot2:::Stat,
                            compute_group = function(data, scales,linear = TRUE) {
                              
                              ii<-akima::interp(x = data$x,
                                                y = data$y,
                                                z = data$fill,
                                                linear=linear)
                              
                              data.out <- tibble(x = rep(ii$x, nrow(ii$z)),
                                                 y = rep(ii$y, each = ncol(ii$z)),
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
  #' @param ... arguments passed to base function,
  #' 
  #' @importFrom akima interp
  #' @importFrom ggplot2 ggproto layer
  
  layer(
    stat = StatInterpRaster, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##### Contour #####
StatInterpContour <- ggproto("StatInterpRaster", ggplot2:::StatContour,
                             compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL, 
                                                      breaks = NULL, na.rm = FALSE)  {
                               
                               ii<-akima::interp(x = data$x,
                                                 y = data$y,
                                                 z = data$z)
                               data <-  tibble(x = rep(ii$x, nrow(ii$z)),
                                               y = rep(ii$y, each = ncol(ii$z)),
                                               z = as.numeric(ii$z), 
                                               group = 1) 
                               
                               StatContour$compute_group(data, scales, z.range, 
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
  #' @param ... arguments passed to base function,
  #' 
  #' @importFrom akima interp
  #' @importFrom ggplot2 ggproto layer
  #' 
  layer(
    stat = StatInterpContour, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, bins = bins, binwidth = binwidth, 
                  breaks = breaks,  ...)
  )
}