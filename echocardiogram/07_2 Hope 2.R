
# 4. Variables selection + LDA, SVM
# PE1.2[, c(1, 3:6)]
# PE0.2[, c(1, 3:6)]
# PE0.2[, c(4, 6)]

# PE1.t2[, c(1, 3:6)]
# PE0.t2[, c(1, 3:6)]
# PE0.t2[, c(4, 6)]

# (a). Variables ordered upon Wilk's lambda
shapiro.test(PE0.2[, 6])

wilks.lam.fun <- function(g1, g2) {
  attr <- c(g1, g2)
  ssw <- var(g1) + var(g2)
  ssb <- (mean(g1)-mean(attr))^2 + (mean(g2)-mean(attr))^2
  (wilks.lam <- ssw/(ssw+ssb))
}
PE1.2.o <- apply(as.matrix(c(1, 3:6)), 1, function(x)
  wilks.lam.fun(PE1.2[PE1.2$Alive == 0, x],
                PE1.2[PE1.2$Alive == 1, x])) %>% order
PE1.2.s <- PE1.2[, c(1, 3:6)][, PE1.2.o]
PE0.2.o <- apply(as.matrix(c(1, 3:6)), 1, function(x)
  wilks.lam.fun(PE0.2[PE0.2$Alive == 0, x],
                PE0.2[PE0.2$Alive == 1, x])) %>% order
PE0.2.s <- PE0.2[, c(1, 3:6)][, PE0.2.o]
PE0.2.o2 <- apply(as.matrix(c(4, 6)), 1, function(x)
  wilks.lam.fun(PE0.2[PE0.2$Alive == 0, x],
                PE0.2[PE0.2$Alive == 1, x])) %>% order
PE0.2.s2 <- PE0.2[, c(4, 6)][, PE0.2.o2]

PE1.t2.o <- apply(as.matrix(c(1, 3:6)), 1, function(x)
  wilks.lam.fun(PE1.t2[PE1.t2$Alive == 0, x],
                PE1.t2[PE1.t2$Alive == 1, x])) %>% order
PE1.t2.s <- PE1.t2[, c(1, 3:6)][, PE1.t2.o]
PE0.t2.o <- apply(as.matrix(c(1, 3:6)), 1, function(x)
  wilks.lam.fun(PE0.t2[PE0.t2$Alive == 0, x],
                PE0.t2[PE0.t2$Alive == 1, x])) %>% order
PE0.t2.s <- PE0.t2[, c(1, 3:6)][, PE0.t2.o]
PE0.t2.o2 <- apply(as.matrix(c(4, 6)), 1, function(x)
  wilks.lam.fun(PE0.t2[PE0.t2$Alive == 0, x],
                PE0.t2[PE0.t2$Alive == 1, x])) %>% order
PE0.t2.s2 <- PE0.t2[, c(4, 6)][, PE0.t2.o2]

#------------------------------------------
# (b) Variables selection + LDA, SVM
#   (i). Predicted class 
# factor.fun
# classifier.pred

pred.class <- function(data.x, data.y) {
  pred <- apply(as.matrix(2:dim(data.x)[2]), 1, function(dim)
    classifier.pred(data.x[, 1:dim], factor.fun(data.y)))
  lda.class <- head(pred, dim(data.x)[1])
  svm.class <- tail(pred, dim(data.x)[1])
  list(LDA = lda.class, SVM = svm.class)
}

PE1.2.p <- pred.class(scale(PE1.2.s), PE1.2$Alive)
PE0.2.p <- pred.class(scale(PE0.2.s), PE0.2$Alive)
PE0.2.p2 <- pred.class(scale(PE0.2.s2), PE0.2$Alive)

PE1.t2.p <- pred.class(scale(PE1.t2.s), PE1.t2$Alive)
PE0.t2.p <- pred.class(scale(PE0.t2.s), PE0.t2$Alive)
PE0.t2.p2 <- pred.class(scale(PE0.t2.s2), PE0.t2$Alive)

#   (ii). Measure
# accur.fun
# precision.fun
# recall.fun 

# PE1
PE1.m <- function(FUN) {
  PE1.2.m <- lapply(PE1.2.p, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE1.2$Alive))) %>% data.frame
  PE1.t2.m <- lapply(PE1.t2.p, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE1.t2$Alive))) %>% data.frame
  data.frame(PE1.2.m, PE1.t2.m)
}
PE1.a <- PE1.m(accur.fun)
PE1.p <- PE1.m(precision.fun)
PE1.r <- PE1.m(recall.fun)
# Best method: transformation + var.1~3 + LDA
PE1.b.pred <- pred.class(scale(PE1.t2.s), PE1.t2$Alive)$LDA[, 2]

# PE0
PE0.m <- function(FUN) {
  PE0.2.m <- lapply(PE0.2.p, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.2$Alive))) %>% data.frame
  PE0.2.m2 <- lapply(PE0.2.p2, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.2$Alive))) %>% data.frame
  
  PE0.t2.m <- lapply(PE0.t2.p, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.t2$Alive))) %>% data.frame
  PE0.t2.m2 <- lapply(PE0.t2.p2, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.t2$Alive))) %>% data.frame
  data.frame(PE0.2.m, PE0.2.m2,
             PE0.t2.m, PE0.t2.m2)
}
PE0.a <- PE0.m(accur.fun)
PE0.p <- PE0.m(precision.fun)
PE0.r <- PE0.m(recall.fun)
# Best method: transformation + var.1~2 + LDA 
PE0.b.pred <- pred.class(scale(PE0.t2.s2), PE0.t2$Alive)$LDA[, 1]

#============================================
# 5. Plot the best method & Explain it
# PE1.best.pred, transformation + var.1~3 + LDA
# PE0.best.pred, transformation + var.1~2 + LDA 

# PE1
# (a). Plot var.1~3
library(rgl)
#   (i). Actual
plot3d(PE1.t2.s[, 1:3], 
       type = "s", 
       col = as.numeric(PE1.t2$Alive)+1,
       size = 1.3, xlab = "LVDD_t",
       ylab = "Age", zlab = "FS")
#   (ii). Predicted class
plot3d(PE1.t2.s[, 1:3], 
       type = "s", 
       col = PE1.b.pred + 2,
       size = 1.3, xlab = "LVDD_t",
       ylab = "Age", zlab = "FS")
#   (iii). Predicted risks
PE1.sample.death.risk0 <- apply(as.matrix(1:dim(PE1.t2.s)[1]), 1, function(x) {
  ## LDA # library(MASS)
  model.lda <- lda(x = PE1.t2.s[-x, 1:3], 
                   grouping = PE1.t2$Alive[-x])
  lda.pred <- predict(model.lda, PE1.t2.s[x, 1:3])
  lda.pred$posterior[1]
})
# library(fields)
# trans <- function(x) round(x/max(x)*11)+1
# risk.col <- designer.colors(12, col = c("green", 
#                                         "gray", "red"))
plot3d(PE1.t2.s[, 1:3], 
       type = "s", 
       col = risk.col[trans(PE1.sample.death.risk0)],
       size = 1.3, xlab = "LVDD_t",
       ylab = "Age", zlab = "FS")

# (b). Effects
cor(PE1.t2.s[, 1:3], PE1.sample.death.risk0)

lm(PE1.sample.death.risk0~., 
   data = cbind(PE1.t2.s[, 1:3], 
                PE1.sample.death.risk0))
?lm
#-------------------------------------------
#  PE0
#  (a). Plot var.1~2
#   (i). Actual
plot(PE0.t2.s2, 
     pch = 16, cex = 1.3,
     col = as.numeric(PE0.t2$Alive)+1,
     xlab = "EPSS_t", ylab = "Wall.Motion_t")
#   (ii). Predicted
plot(PE0.t2.s2, 
     pch = 16, cex = 1.3,
     col = PE0.b.pred + 2,
     xlab = "EPSS_t", ylab = "Wall.Motion_t")
#   (iii). Predicted risks
PE0.sample.death.risk0 <- apply(as.matrix(1:dim(PE0.t2.s2)[1]), 1, function(x) {
  ## LDA # library(MASS)
  model.lda <- lda(x = PE0.t2.s2[-x, ], 
                   grouping = PE0.t2$Alive[-x])
  lda.pred <- predict(model.lda, PE0.t2.s2[x,])
  lda.pred$posterior[1]
})
plot(PE0.t2.s2, 
     pch = 16, cex = 1.3,
     col = risk.col[trans(PE0.sample.death.risk0)],
     xlab = "EPSS_t", ylab = "Wall.Motion_t")

# (b). Effects
cor(PE0.t2.s2, PE0.sample.death.risk0)

lm(PE0.sample.death.risk0~., 
   data = cbind(PE0.t2.s2, 
                PE0.sample.death.risk0))

#===========================================
# 6. Death risks prediction

# PE1
# (a). Range
  # colnames(PE1.t2.s[, 1:3])
R.PE1 <- apply(PE1[, c("LVDD", "Age", "FS")], 2, range)

# (b). Simulate attr.'s values to predict
PE1.test.v <- apply(R.PE1, 2, function(col)
  seq(col[1], col[2], length.out = 100))

PE1.test.v2 <- cbind(rep(PE1.test.v[, 1], each = 100^2),
                     rep(rep(PE1.test.v[, 2], each = 100), 100),
                     rep(PE1.test.v[, 3], 100^2))

# (c). Transform the simulated values
t.PE1.test.v <- PE1.test.v2
t.PE1.test.v[, 1] <- log((PE1.test.v2[, 1]-5)^2)
  # t.PE1.test.v

# (d). Predict death risks
## LDA # library(MASS)
PE1.lda.model <- lda(x = PE1.t2.s[, 1:3], 
                     grouping = factor.fun(PE1.t2$Alive))
PE1.death.risk0 <- apply(t.PE1.test.v, 1, function(row) {
  lda.pred <- predict(PE1.lda.model, row)
  lda.pred$posterior[1]
})
PE1.12.risk0 <- tapply(PE1.death.risk0,
                       list(t.PE1.test.v[, 1],
                            t.PE1.test.v[, 2]), mean)
PE1.13.risk0 <- tapply(PE1.death.risk0,
                       list(t.PE1.test.v[, 1],
                            t.PE1.test.v[, 3]), mean)
PE1.23.risk0 <- tapply(PE1.death.risk0,
                       list(t.PE1.test.v[, 2],
                            t.PE1.test.v[, 3]), mean)

## Contour plot
library(plot3D)
#   (i). LVDD vs. Age
contour2D(z = PE1.12.risk0, lwd = 2,
          x = sort(PE1.test.v[, 1]),
          y = sort(PE1.test.v[, 2]),
          xlab = "LVDD", ylab = "Age")
image2D(z = PE1.12.risk0, lwd = 2,
        x = sort(PE1.test.v[, 1]),
        y = sort(PE1.test.v[, 2]),
        xlab = "LVDD", ylab = "Age", 
        contour = T)
#   (ii). LVDD vs. FS
contour2D(z = PE1.13.risk0, lwd = 2,
          x = sort(PE1.test.v[, 1]),
          y = sort(PE1.test.v[, 3]),
          xlab = "LVDD", ylab = "FS")
image2D(z = PE1.13.risk0, lwd = 2,
        x = sort(PE1.test.v[, 1]),
        y = sort(PE1.test.v[, 3]),
        xlab = "LVDD", ylab = "FS", 
        contour = T)
#   (iii). LVDD vs. Age
contour2D(z = PE1.23.risk0, lwd = 2,
          x = sort(PE1.test.v[, 2]),
          y = sort(PE1.test.v[, 3]),
          xlab = "Age", ylab = "FS")
image2D(z = PE1.23.risk0, lwd = 2,
        x = sort(PE1.test.v[, 2]),
        y = sort(PE1.test.v[, 3]),
        xlab = "Age", ylab = "FS", 
        contour = T)

#---------------------------------------
# PE0
# (a). Range
# colnames(PE0.t2.s2)
R.PE0 <- apply(PE0[, c("EPSS", "Wall.Motion")], 2, range)

# (b). Simulate attr.'s values to predict
PE0.test.v <- apply(R.PE0, 2, function(col)
  seq(col[1], col[2], length.out = 100))

PE0.test.v2 <- cbind(rep(PE0.test.v[, 1], each = 100),
                     rep(PE0.test.v[, 2], 100))

# (c). Transform the simulated values
t.PE0.test.v <- PE0.test.v2
t.PE0.test.v[, 1] <- log((PE0.test.v2[, 1]-10)^2 + 1)
t.PE0.test.v[, 2] <- log(PE0.test.v2[, 2])
# t.PE0.test.v

# (d). Predict death risks
## LDA # library(MASS)
PE0.lda.model <- lda(x = PE0.t2.s2, 
                     grouping = factor.fun(PE0.t2$Alive))
PE0.death.risk0 <- apply(t.PE0.test.v, 1, function(row) {
  lda.pred <- predict(PE0.lda.model, row)
  lda.pred$posterior[1]
})
PE0.risk0 <- tapply(PE0.death.risk0,
                    list(t.PE0.test.v[, 1],
                         t.PE0.test.v[, 2]), mean)
## Contour plot
library(plot3D)
contour2D(z = PE0.risk0, lwd = 2,
          x = sort(PE0.test.v[, 1]),
          y = sort(PE0.test.v[, 2]),
          xlab = "EPSS", ylab = "Wall Motion")
image2D(z = PE0.risk0, lwd = 2,
        x = sort(PE0.test.v[, 1]),
        y = sort(PE0.test.v[, 2]),
        xlab = "EPSS", ylab = "Wall Motion", 
        contour = T)

