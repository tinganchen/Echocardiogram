volcano
dim(volcano)
Volcano <- volcano[seq(1, nrow(volcano), by = 3),
                   seq(1, ncol(volcano), by = 3)]
dim(Volcano)

library(plot3D)
contour2D(Volcano, 
          x = seq(1, nrow(volcano), by = 3),
          y = seq(1, ncol(volcano), by = 3))
