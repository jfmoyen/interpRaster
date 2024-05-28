# interpRaster

A small geometry for ggplot2 allowing to plot raster or contour on irregularly spaced data.

For instance:

`ggplot(diamonds)+ geom_interp_raster(aes(x=x,y=table,fill=price),duplicate="user",dupfun="max")`
