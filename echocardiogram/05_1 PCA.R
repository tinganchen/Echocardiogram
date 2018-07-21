library(FactoMineR)
library(ggplot2)
library(factoextra)

## 1. Total
x.pca <- PCA(del.out.data2[, c(1, 3, 5:8)], 
             scale.unit = T, graph = F)
# scree plot - eigenvalue
eig <- get_eigenvalue(x.pca)
plot(eig[, 1], main = "Scree Plot", xlab = "PC",
     ylab = "Eigenvalue", pch = 16, 
     col = rep(c(2, 4), c(2, 4)), ylim = c(0, 2.5))
text(1:dim(eig)[1], eig[ , 1] - 0.2,
     paste(round(eig[ , 2]), "%"),
     col = rep(c(2, 4), c(2, 4)))
lines(1:dim(eig)[1], eig[ , 1], col = "gray", lwd = 2)

# PCA loadings & Loadings plot
var <- get_pca_var(x.pca)
ind <- get_pca_ind(x.pca)

pc.loading <- var$coord[, 1:2] /
  matrix(rep(sqrt(eig[1:2 , 1]), 6),
         ncol = 2, byrow = T)
library(fields)
var.contrib.col <- two.colors(n = 21, start = "blue",
                              middle = "gray", end = "red")
var.contrib <- sqrt(pc.loading[ , 1]^2 + pc.loading[ ,2]^2)
trans.var.contrib <- round(var.contrib * 20 + 1)

library(corrplot)
corrplot(pc.loading, is.corr = T,
         tl.cex = 0.9, tl.pos = "rt",
         method = "number", title = "Loadings",
         mar = c(1, 0, 3, 0), col = var.contrib.col)
# ?corrplot

layout(matrix(c(1, 1, 2), nrow = 1))
plot(pc.loading[ , 1], pc.loading[ , 2],
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
     xlab = paste("PC1 (", round(eig[1 , 2], 2), "%)"),
     ylab = paste("PC2 (", round(eig[2 , 2], 2), "%)"),
     main = "Loadings Plot with Contribution")
abline(v = 0, h = 0, lty = 2)
lines(cos(seq(0, 2 * pi, l = 100)),
      sin(seq(0, 2 * pi, l = 100)))
lines(0.5 * cos(seq(0, 2 * pi, l = 100)),
      0.5 * sin(seq(0, 2 * pi, l = 100)))
arrows(0, 0, pc.loading[ , 1], pc.loading[ , 2],
       length = 0.15, angle = 20,
       col = var.contrib.col[trans.var.contrib], lwd = 2)
text(pc.loading[ , 1] + 0.15, pc.loading[ , 2],
     row.names(pc.loading), cex = 1.2)

par(mai = rep(0.1, 4))
plot(0, type = "n", bty = "n", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
rasterImage(var.contrib.col, 1.1, 0.5, 0.9, -0.5)
text(1, 0.6, "Contribution")
text(1.15, c(-0.5, 0, 0.5), c(0, 0.5, 1))

# PCA scores & Scores plot
plot(ind$coord[, 1:2], pch = 16,
     col = as.numeric(del.out.data2[, 9])+1,
     main = "PCA")

#---------------------------------------------
## 2. PE = 0
PE0.pca <- PCA(del.out.data2[del.out.data2$PE == 0, c(1, 3, 5:8)], 
               scale.unit = T, graph = F)
# scree plot - eigenvalue
eig0 <- get_eigenvalue(PE0.pca)
plot(eig0[, 1], main = "Scree Plot", xlab = "PC",
     ylab = "Eigenvalue", pch = 16, 
     col = rep(c(2, 4), c(2, 4)), ylim = c(0, 2.5))
text(1:dim(eig0)[1], eig0[ , 1] - 0.2,
     paste(round(eig0[ , 2]), "%"),
     col = rep(c(2, 4), c(2, 4)))
lines(1:dim(eig0)[1], eig0[ , 1], col = "gray", lwd = 2)

# PCA loadings & Loadings plot
var0 <- get_pca_var(PE0.pca)
ind0 <- get_pca_ind(PE0.pca)

pc.loading0 <- var0$coord[, 1:2] /
  matrix(rep(sqrt(eig0[1:2 , 1]), 6),
         ncol = 2, byrow = T)
library(fields)
# var.contrib.col <- two.colors(n = 21, start = "blue",
#                               middle = "gray", end = "red")
var.contrib0 <- sqrt(pc.loading0[ , 1]^2 + pc.loading0[ ,2]^2)
trans.var.contrib0 <- round(var.contrib0 * 20 + 1)

library(corrplot)
corrplot(pc.loading0, is.corr = T,
         tl.cex = 0.9, tl.pos = "rt",
         method = "number", title = "Loadings",
         mar = c(1, 0, 3, 0), col = var.contrib.col)
# ?corrplot

layout(matrix(c(1, 1, 2), nrow = 1))
plot(pc.loading0[ , 1], pc.loading0[ , 2],
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
     xlab = paste("PC1 (", round(eig0[1 , 2], 2), "%)"),
     ylab = paste("PC2 (", round(eig0[2 , 2], 2), "%)"),
     main = "Loadings Plot with Contribution")
abline(v = 0, h = 0, lty = 2)
lines(cos(seq(0, 2 * pi, l = 100)),
      sin(seq(0, 2 * pi, l = 100)))
lines(0.5 * cos(seq(0, 2 * pi, l = 100)),
      0.5 * sin(seq(0, 2 * pi, l = 100)))
arrows(0, 0, pc.loading0[ , 1], pc.loading0[ , 2],
       length = 0.15, angle = 20,
       col = var.contrib.col[trans.var.contrib0], lwd = 2)
text(pc.loading0[ , 1] + 0.15, pc.loading0[ , 2],
     row.names(pc.loading0), cex = 1.2)

par(mai = rep(0.1, 4))
plot(0, type = "n", bty = "n", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
rasterImage(var.contrib.col, 1.1, 0.5, 0.9, -0.5)
text(1, 0.6, "Contribution")
text(1.15, c(-0.5, 0, 0.5), c(0, 0.5, 1))

# PCA scores & Scores plot
plot(ind0$coord[, 1:2], pch = 16,
     col = as.numeric(del.out.data2[del.out.data2$PE == 0, 9])+1,
     main = "PCA (PE = 0)")

kNNdistplot(ind0$coord[, 1:2], k =  2)
abline(h = 0.5, lty = 2)

PE0.db <- dbscan(ind0$coord[, 1:2], eps = 0.5, minPts = 4)
fviz_cluster(PE0.db, ind0$coord[, 1:2], geom = "point")

#---------------------------------------------
## 3. PE = 1
PE1.pca <- PCA(del.out.data2[del.out.data2$PE == 1, c(1, 3, 5:8)], 
               scale.unit = T, graph = F)
# scree plot - eigenvalue
eig1 <- get_eigenvalue(PE1.pca)
plot(eig1[, 1], main = "Scree Plot", xlab = "PC",
     ylab = "Eigenvalue", pch = 16, 
     col = rep(c(2, 4), c(2, 4)), ylim = c(0, 2.5))
text(1:dim(eig1)[1], eig1[ , 1] - 0.2,
     paste(round(eig1[ , 2]), "%"),
     col = rep(c(2, 4), c(2, 4)))
lines(1:dim(eig1)[1], eig1[ , 1], col = "gray", lwd = 2)

# PCA loadings & Loadings plot
var1 <- get_pca_var(PE1.pca)
ind1 <- get_pca_ind(PE1.pca)

pc.loading1 <- var1$coord[, 1:2] /
  matrix(rep(sqrt(eig1[1:2 , 1]), 6),
         ncol = 2, byrow = T)
library(fields)
# var.contrib.col <- two.colors(n = 21, start = "blue",
#                               middle = "gray", end = "red")
var.contrib1 <- sqrt(pc.loading1[ , 1]^2 + pc.loading1[ ,2]^2)
trans.var.contrib1 <- round(var.contrib1 * 20 + 1)

library(corrplot)
corrplot(pc.loading1, is.corr = T,
         tl.cex = 0.9, tl.pos = "rt",
         method = "number", title = "Loadings",
         mar = c(1, 0, 3, 0), col = var.contrib.col)
# ?corrplot

layout(matrix(c(1, 1, 2), nrow = 1))
plot(pc.loading1[ , 1], pc.loading1[ , 2],
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
     xlab = paste("PC1 (", round(eig1[1 , 2], 2), "%)"),
     ylab = paste("PC2 (", round(eig1[2 , 2], 2), "%)"),
     main = "Loadings Plot with Contribution")
abline(v = 0, h = 0, lty = 2)
lines(cos(seq(0, 2 * pi, l = 100)),
      sin(seq(0, 2 * pi, l = 100)))
lines(0.5 * cos(seq(0, 2 * pi, l = 100)),
      0.5 * sin(seq(0, 2 * pi, l = 100)))
arrows(0, 0, pc.loading1[ , 1], pc.loading1[ , 2],
       length = 0.15, angle = 20,
       col = var.contrib.col[trans.var.contrib1], lwd = 2)
text(pc.loading1[ , 1] + 0.15, pc.loading1[ , 2],
     row.names(pc.loading1), cex = 1.2)

par(mai = rep(0.1, 4))
plot(0, type = "n", bty = "n", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
rasterImage(var.contrib.col, 1.1, 0.5, 0.9, -0.5)
text(1, 0.6, "Contribution")
text(1.15, c(-0.5, 0, 0.5), c(0, 0.5, 1))

# PCA scores & Scores plot
plot(ind1$coord[, 1:2], pch = 16,
     col = as.numeric(del.out.data2[del.out.data2$PE == 1, 9])+1,
     main = "PCA (PE = 1)")
library(dbscan)
kNNdistplot(ind1$coord[, 1:2], k =  2)
abline(h = 1, lty = 2)

PE1.db <- dbscan(ind1$coord[, 1:2], eps = 1, minPts = 4)
fviz_cluster(PE1.db, ind1$coord[, 1:2], geom = "point")
