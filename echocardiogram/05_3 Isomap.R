# Isomap
library(vegan)
heart.isomap <- isomap(dist(cbind(del.out.data2[, 4], scale(del.out.data2[, c(3, 5:8)]))),
                       ndim = 2, k = 7)
plot(heart.isomap$points, 
     col = as.numeric(del.out.data2[, 9])+1,
     pch = 16, main = "Isomap", 
     xlab = "Dim.1", ylab = "Dim.2")
#--------------------------------------
PE0.isomap <- isomap(dist(cbind(del.out.data2[del.out.data2$PE == 0, 4], 
                                scale(del.out.data2[del.out.data2$PE == 0, c(3, 5:8)]))),
                     ndim = 2, k = 7)
plot(PE0.isomap$points, 
     col = as.numeric(del.out.data2[del.out.data2$PE == 0, 9])+1,
     pch = 16, main = "Isomap (PE=0)", 
     xlab = "Dim.1", ylab = "Dim.2")
#-----------------------------------
PE1.isomap <- isomap(dist(cbind(del.out.data2[del.out.data2$PE == 1, 4], 
                                scale(del.out.data2[del.out.data2$PE == 1, c(3, 5:8)]))),
                     ndim = 2, k = 7)
plot(PE1.isomap$points, 
     col = as.numeric(del.out.data2[del.out.data2$PE == 1, 9])+1,
     pch = 16, main = "Isomap (PE=1)", 
     xlab = "Dim.1", ylab = "Dim.2")
