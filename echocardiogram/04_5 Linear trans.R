# square 5,6,7
median()
mean(del.out.data2[ , 8])
hist(3^(-del.out.data2[ , 8]))

(del.out.data2[ , 5]-0.18)^2
(del.out.data2[ , 6]-9)^2
(del.out.data2[ , 7]-4.8)^2
sm.density.compare((del.out.data2[ , 5]-0.18)^2, 
                   del.out.data2$Alive,
                   col = 2:3, 
                   lty = rep(1, 2), lwd = 2,
                   xlab = "")
sm.density.compare((del.out.data2[ , 6]-9)^2, 
                   del.out.data2$Alive,
                   col = 2:3, 
                   lty = rep(1, 2), lwd = 2,
                   xlab = "")
sm.density.compare((del.out.data2[ , 7]-4.8)^2, 
                   del.out.data2$Alive,
                   col = 2:3, 
                   lty = rep(1, 2), lwd = 2,
                   xlab = "")
linear.data <- del.out.data2
linear.data[ , 5] <- (del.out.data2[ , 5]-0.18)^2
linear.data[ , 6] <- (del.out.data2[ , 6]-9)^2
linear.data[ , 7] <- (del.out.data2[ , 7]-4.8)^2
#===============================================
##======
## PCA
##======
# library(FactoMineR)
# library(ggplot2)
# library(factoextra)

# 1. Total
# (a) scree plot - eigenvalue
l.pca <- PCA(linear.data[, c(3, 5:8)], 
             scale.unit = T, graph = F)
l.eig <- get_eigenvalue(l.pca)
plot(l.eig[, 1], main = "Scree Plot", xlab = "PC",
     ylab = "Eigenvalue", pch = 16, 
     col = rep(c(2, 4), c(3, 2)), ylim = c(0, 2.5))
text(1:dim(l.eig)[1], l.eig[ , 1] - 0.2,
     paste(round(l.eig[ , 2]), "%"),
     col = rep(c(2, 4), c(3, 2)))
lines(1:dim(l.eig)[1], l.eig[ , 1], col = "gray", lwd = 2)
# (b) Loading
l.var <- get_pca_var(l.pca)
l.pc.loading <- l.var$coord /
  matrix(rep(sqrt(l.eig[ , 1]), dim(l.var$coord)[2]),
         ncol = dim(l.var$coord)[2], byrow = T)
# library(fields)
# var.contrib.col <- two.colors(n = 21, start = "blue",
#                               middle = "gray", end = "red")
l.var.contrib <- sqrt(l.pc.loading[ , 1]^2 + l.pc.loading[ ,2]^2)
l.trans.var.contrib <- round(l.var.contrib * 20 + 1)
# library(corrplot)
corrplot(l.pc.loading[ , 1:3], is.corr = T,
         tl.cex = 0.9, tl.pos = "rt",
         method = "number", title = "Loadings",
         mar = c(1, 0, 3, 0), col = var.contrib.col)

layout(matrix(c(1, 1, 2), nrow = 1))
plot(l.pc.loading[ , 1], l.pc.loading[ , 2],
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
     xlab = paste("PC1 (", round(l.eig[1, 2], 2), "%)"),
     ylab = paste("PC2 (", round(l.eig[2, 2], 2), "%)"),
     main = "Loadings Plot with Contribution")
abline(v = 0, h = 0, lty = 2)
lines(cos(seq(0, 2 * pi, l = 100)),
      sin(seq(0, 2 * pi, l = 100)))
lines(0.5 * cos(seq(0, 2 * pi, l = 100)),
      0.5 * sin(seq(0, 2 * pi, l = 100)))
arrows(0, 0, l.pc.loading[ , 1], l.pc.loading[ , 2],
       length = 0.15, angle = 20,
       col = var.contrib.col[l.trans.var.contrib], lwd = 2)
text(l.pc.loading[ , 1] + 0.15, l.pc.loading[ , 2],
     row.names(l.pc.loading), cex = 1.2)

par(mai = rep(0.1, 4))
plot(0, type = "n", bty = "n", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
rasterImage(var.contrib.col, 1.1, 0.5, 0.9, -0.5)
text(1, 0.6, "Contribution")
text(1.15, c(-0.5, 0, 0.5), c(0, 0.5, 1))
# (c) Score
l.ind <- get_pca_ind(l.pca)
plot(l.ind$coord[, 1:2], pch = 16,
     col = as.numeric(linear.data[, 9])+1,
     main = "PCA")

# 2. PE = 0
# (a) scree plot - eigenvalue
l.PE0.pca <- PCA(linear.data[linear.data$PE == 0,
                             c(3, 5:8)], 
                 scale.unit = T, graph = F)
l.eig0 <- get_eigenvalue(l.PE0.pca)
plot(l.eig0[, 1], main = "Scree Plot", xlab = "PC",
     ylab = "Eigenvalue", pch = 16, 
     col = rep(c(2, 4), c(3, 2)), ylim = c(0, 2.5))
text(1:dim(l.eig0)[1], l.eig0[ , 1] - 0.2,
     paste(round(l.eig0[ , 2]), "%"),
     col = rep(c(2, 4), c(3, 2)))
lines(1:dim(l.eig0)[1], l.eig0[ , 1], col = "gray", lwd = 2)
# (b) Loading
l.var0 <- get_pca_var(l.PE0.pca)
l.pc.loading0 <- l.var0$coord /
  matrix(rep(sqrt(l.eig0[ , 1]), dim(l.var0$coord)[2]),
         ncol = dim(l.var0$coord)[2], byrow = T)
# library(fields)
# var.contrib.col <- two.colors(n = 21, start = "blue",
#                               middle = "gray", end = "red")
l.var.contrib0 <- sqrt(l.pc.loading0[ , 1]^2 + l.pc.loading0[ ,2]^2)
l.trans.var.contrib0 <- round(l.var.contrib0 * 20 + 1)
# library(corrplot)
corrplot(l.pc.loading0[ , 1:3], is.corr = T,
         tl.cex = 0.9, tl.pos = "rt",
         method = "number", title = "Loadings",
         mar = c(1, 0, 3, 0), col = var.contrib.col)

layout(matrix(c(1, 1, 2), nrow = 1))
plot(l.pc.loading0[ , 1], l.pc.loading0[ , 2],
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
     xlab = paste("PC1 (", round(l.eig0[1, 2], 2), "%)"),
     ylab = paste("PC2 (", round(l.eig0[2, 2], 2), "%)"),
     main = "Loadings Plot with Contribution")
abline(v = 0, h = 0, lty = 2)
lines(cos(seq(0, 2 * pi, l = 100)),
      sin(seq(0, 2 * pi, l = 100)))
lines(0.5 * cos(seq(0, 2 * pi, l = 100)),
      0.5 * sin(seq(0, 2 * pi, l = 100)))
arrows(0, 0, l.pc.loading0[ , 1], l.pc.loading0[ , 2],
       length = 0.15, angle = 20,
       col = var.contrib.col[l.trans.var.contrib0], lwd = 2)
text(l.pc.loading0[ , 1] + 0.15, l.pc.loading0[ , 2],
     row.names(l.pc.loading0), cex = 1.2)

par(mai = rep(0.1, 4))
plot(0, type = "n", bty = "n", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
rasterImage(var.contrib.col, 1.1, 0.5, 0.9, -0.5)
text(1, 0.6, "Contribution")
text(1.15, c(-0.5, 0, 0.5), c(0, 0.5, 1))
# (c) Score
l.ind0 <- get_pca_ind(l.PE0.pca)
plot(l.ind0$coord[, 1:2], pch = 16,
     col = as.numeric(linear.data[linear.data$PE == 0, 9])+1,
     main = "PCA (PE = 0)")

# 3. PE = 1
# (a) scree plot - eigenvalue
l.PE1.pca <- PCA(linear.data[linear.data$PE == 1,
                             c(3, 5:8)], 
                 scale.unit = T, graph = F)
l.eig1 <- get_eigenvalue(l.PE1.pca)
plot(l.eig1[, 1], main = "Scree Plot", xlab = "PC",
     ylab = "Eigenvalue", pch = 16, 
     col = rep(c(2, 4), c(2, 3)), ylim = c(0, 2.5))
text(1:dim(l.eig1)[1], l.eig1[ , 1] - 0.2,
     paste(round(l.eig1[ , 2]), "%"),
     col = rep(c(2, 4), c(2, 3)))
lines(1:dim(l.eig1)[1], l.eig1[ , 1], col = "gray", lwd = 2)
# (b) Loading
l.var1 <- get_pca_var(l.PE1.pca)
l.pc.loading1 <- l.var1$coord /
  matrix(rep(sqrt(l.eig1[ , 1]), dim(l.var1$coord)[2]),
         ncol = dim(l.var1$coord)[2], byrow = T)
# library(fields)
# var.contrib.col <- two.colors(n = 21, start = "blue",
#                               middle = "gray", end = "red")
l.var.contrib1 <- sqrt(l.pc.loading1[ , 1]^2 + l.pc.loading1[ ,2]^2)
l.trans.var.contrib1 <- round(l.var.contrib1 * 20 + 1)
# library(corrplot)
corrplot(l.pc.loading1[ , 1:2], is.corr = T,
         tl.cex = 0.9, tl.pos = "rt",
         method = "number", title = "Loadings",
         mar = c(1, 0, 3, 0), col = var.contrib.col)

layout(matrix(c(1, 1, 2), nrow = 1))
plot(l.pc.loading1[ , 1], l.pc.loading1[ , 2],
     xlim = c(-1, 1), ylim = c(-1, 1), type = "n",
     xlab = paste("PC1 (", round(l.eig1[1, 2], 2), "%)"),
     ylab = paste("PC2 (", round(l.eig1[2, 2], 2), "%)"),
     main = "Loadings Plot with Contribution")
abline(v = 0, h = 0, lty = 2)
lines(cos(seq(0, 2 * pi, l = 100)),
      sin(seq(0, 2 * pi, l = 100)))
lines(0.5 * cos(seq(0, 2 * pi, l = 100)),
      0.5 * sin(seq(0, 2 * pi, l = 100)))
arrows(0, 0, l.pc.loading1[ , 1], l.pc.loading1[ , 2],
       length = 0.15, angle = 20,
       col = var.contrib.col[l.trans.var.contrib1], lwd = 2)
text(l.pc.loading1[ , 1] + 0.15, l.pc.loading1[ , 2],
     row.names(l.pc.loading1), cex = 1.2)

par(mai = rep(0.1, 4))
plot(0, type = "n", bty = "n", xlab = "", ylab = "",
     xaxt = "n", yaxt = "n")
rasterImage(var.contrib.col, 1.1, 0.5, 0.9, -0.5)
text(1, 0.6, "Contribution")
text(1.15, c(-0.5, 0, 0.5), c(0, 0.5, 1))
# (c) Score
l.ind1 <- get_pca_ind(l.PE1.pca)
plot(l.ind1$coord[, 1:2], pch = 16,
     col = as.numeric(linear.data[linear.data$PE == 1, 9])+1,
     main = "PCA (PE = 0)")
#---------------------------------------------
##==================
## PCA scores + LDA
##==================
# 1. Total
l.pc.lda <- lda(x = l.ind$coord[, 1:2], 
                grouping = as.numeric(linear.data[, 9])-1, CV = T)
l.pc.confus.mat <- table(as.numeric(linear.data[, 9])-1, l.pc.lda$class)
l.pc.accuracy <- sum(diag(l.pc.confus.mat))/sum(l.pc.confus.mat)
l.pc.accuracy
plot(l.ind0$coord[, 1:2], pch = 16,
     col = as.numeric(linear.data[, 9])+1,
     main = "PCA (PE = 0)")
plot(l.ind$coord[, 1:2], pch = 16,
     col = as.numeric(l.pc.lda$class)+1,
     main = "PCA")


# 2. PE = 0
l.pc.lda0 <- lda(x = l.ind0$coord[, 1:2], 
                 grouping = as.numeric(linear.data[linear.data$PE == 0, 9])-1, CV = T)
l.pc.confus.mat0 <- table(as.numeric(linear.data[linear.data$PE == 0, 9])-1, l.pc.lda0$class)
l.pc.accuracy0 <- sum(diag(l.pc.confus.mat0))/sum(l.pc.confus.mat0)
l.pc.accuracy0
plot(l.ind0$coord[, 1:2], pch = 16,
     col = as.numeric(linear.data[linear.data$PE == 0, 9])+1,
     main = "PCA (PE = 0)")
plot(l.ind0$coord[, 1:2], pch = 16,
     col = as.numeric(l.pc.lda0$class)+1,
     main = "PCA")

# 3. PE = 1
l.pc.lda1 <- lda(x = l.ind1$coord[, 1:2], 
                 grouping = as.numeric(linear.data[linear.data$PE == 1, 9])-1, CV = T)
l.pc.confus.mat1 <- table(as.numeric(linear.data[linear.data$PE == 1, 9])-1, l.pc.lda1$class)
l.pc.accuracy1 <- sum(diag(l.pc.confus.mat1))/sum(l.pc.confus.mat1)
l.pc.accuracy1
plot(l.ind1$coord[, 1:2], pch = 16,
     col = as.numeric(linear.data[linear.data$PE == 1, 9])+1,
     main = "PCA (PE = 1)")
plot(l.ind1$coord[, 1:2], pch = 16,
     col = as.numeric(l.pc.lda1$class)+1,
     main = "PCA (PE = 1)")
#---------------------------------------
##======
## LDA
##======

# 2. PE = 0
l.lda0 <- lda(x = linear.data[linear.data$PE == 0, c(3, 5:8)], 
              grouping = as.numeric(linear.data[linear.data$PE == 0, 9])-1)
l.lda0.score <- as.matrix(linear.data[linear.data$PE == 0, c(3, 5:8)]) %*% as.matrix(l.lda0$scaling)
sm.density.compare(as.vector(l.lda0.score), 
                   as.numeric(linear.data[linear.data$PE == 0, 9])-1,
                   col = 2:3, lty = rep(1, 2), lwd = 2)

# 3. PE = 1
l.lda1 <- lda(x = linear.data[linear.data$PE == 1, c(3, 5:8)], 
              grouping = as.numeric(linear.data[linear.data$PE == 1, 9])-1)
l.lda1.score <- as.matrix(linear.data[linear.data$PE == 1, c(3, 5:8)]) %*% as.matrix(l.lda1$scaling)
sm.density.compare(as.vector(l.lda1.score), 
                   as.numeric(linear.data[linear.data$PE == 1, 9])-1,
                   col = 2:3, lty = rep(1, 2), lwd = 2)


