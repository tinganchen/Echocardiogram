# 1. Imputed data without Alive = 2
imp.data
dim(imp.data)
imp.data2 <- imp.data[imp.data$Alive != 2, -(1:2)]
dim(imp.data2)
colnames(imp.data2)

#=======================================
# 2. Y distr.
library(sm)
densityPlotFun <- function(data) {
  apply(as.matrix(c(1, 3:6)), 1, function(x) {
    sm.density.compare(data[ , x], 
                       data$Alive,
                       col = 2:3, 
                       lty = rep(1, 2), lwd = 2,
                       xlab = "", ylab = "")
    title(paste("\n\n", colnames(data)[x]), cex.main = 1.3)})
  plot(0, type = "n", xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", bty = "n")
  legend("center", c("Alive", "Not alive"),
         lty = 1, lwd = 2, col = 3:2, cex = 1.3)}

par(mfrow = c(2, 3), mai = c(0.3, 0.5, 0.7, 0.1))
# (a). All subjects
densityPlotFun(imp.data2)
title("\nDensity plot (All subjects)", outer = T, cex.main = 1.7)
# (b). PE = 1
densityPlotFun(imp.data2[imp.data2$PE == 1, ])
title("\nDensity plot (with PE)", outer = T, cex.main = 1.7)
# (c). PE = 0
densityPlotFun(imp.data2[imp.data2$PE == 0, ])
title("\nDensity plot (without PE)", outer = T, cex.main = 1.7)
# (d). Data partition- PE = 1, 0
PE1 <- imp.data2[imp.data2$PE == 1, ] # dim(PE1)
PE0 <- imp.data2[imp.data2$PE == 0, ] # dim(PE0)
# (e). Two sample t-test -> 
#      if Age, FS or LVDD distr. dept. on Alive is diff.
# Age
shapiro.test(PE0$Age[PE0$Alive == 0]) # Normal assumption
shapiro.test(PE0$Age[PE0$Alive == 1])
var.test(PE0$Age[PE0$Alive == 0], 
         PE0$Age[PE0$Alive == 1]) # Equal variance
t.test(PE0$Age ~ PE0$Alive, var.equal = T) # Equal mean
# FS
shapiro.test(PE0$FS[PE0$Alive == 0]) # Normal assumption
shapiro.test(PE0$FS[PE0$Alive == 1])
var.test(PE0$FS[PE0$Alive == 0], 
         PE0$FS[PE0$Alive == 1]) # Equal variance
t.test(PE0$FS ~ PE0$Alive, var.equal = T) # Equal mean
# LVDD
shapiro.test(PE0$LVDD[PE0$Alive == 0]) # Normal assumption
shapiro.test(PE0$LVDD[PE0$Alive == 1])
var.test(PE0$LVDD[PE0$Alive == 0], 
         PE0$LVDD[PE0$Alive == 1]) # Equal variance
t.test(PE0$LVDD ~ PE0$Alive, var.equal = T) # Equal mean
# Subjects with no PE whose survival outcome 
# is indept. of Age, FS and LVDD.

#------------------------------------------------------
# 2-1. X's transformation -> moving + square (+log)
PE1.t <- PE1
PE1.t$EPSS <- log((PE1$EPSS-10)^2)
PE1.t$LVDD <- log((PE1$LVDD-5)^2)
PE1.t$Wall.Motion <- (PE1$Wall.Motion-1.3)^2
names(PE1.t)[4:6] <- c("EPSS_t", "LVDD_t", "Wall.Motion_t")
par(mfrow = c(2, 3), mai = c(0.3, 0.5, 0.7, 0.1))
densityPlotFun(PE1.t)
title("\nDensity plot after Transformation (with PE)",
      outer = T, cex.main = 1.7)

PE0.t <- PE0
PE0.t$EPSS <- log((PE0$EPSS-10)^2 + 1)
PE0.t$Wall.Motion <- log(PE0$Wall.Motion)
names(PE0.t)[c(4, 6)] <- c("EPSS_t", "Wall.Motion_t")
par(mfrow = c(2, 3), mai = c(0.3, 0.5, 0.7, 0.1))
densityPlotFun(PE0.t)
title("\nDensity plot after Transformation (without PE)",
      outer = T, cex.main = 1.7)

#-----------------------------------------
# 2-2. Y imbalanced data problem -> 
prop.table(table(as.numeric(PE1.t$Alive)-1))
prop.table(table(as.numeric(PE0.t$Alive)-1))

#==============================================
# 3. X's relationship
outlierFun <- function(var) {
  which(scale(var) < -3 | scale(var) > 3)
}
# (a). Before transformation
apply(PE1[, c(1, 3:6)], 2, outlierFun) # No outliers
outlier.PE0 <- unlist(apply(PE0[, c(1, 3:6)], 2, outlierFun)) # No outliers
PE1.2 <- PE1
PE0.2 <- PE0[-outlier.PE0, ]

# (a). After transformation
#   (i). PE = 1
apply(PE1.t[, c(1, 3:6)], 2, outlierFun) # No outliers
pairs(PE1.t[, c(1, 3:6)], 
      col = as.numeric(PE1.t$Alive)+1,
      pch = 16)
cor(PE1.t[, c(1, 3:6)]) # No highly correlated var.s
PE1.t2 <- PE1.t

#   (ii). PE = 0
outlier.PE0.t <- unlist(apply(PE0.t[, c(1, 3:6)], 2, outlierFun)) # No outliers
pairs(PE0.t[-outlier.PE0.t, c(1, 3:6)], 
      col = as.numeric(PE0.t$Alive)+1,
      pch = 16)
cor(PE0.t[, c(1, 3:6)]) # No highly correlated var.s
PE0.t2 <- PE0.t[-outlier.PE0.t, ]

#============================================
# 4. Dimension reduction + Modeling(Classifier)
#    upon several imbalanced data solving methods
# (a). PCA
library(FactoMineR)
library(ggplot2)
library(factoextra)
pcaScoreFun <- function(data) {
  data.pca <- PCA(data, scale.unit = T, graph = F)
  eig <- get_eig(data.pca)
  ind <- get_pca_ind(data.pca)
  score <- ind$coord *
    matrix(rep(ifelse(eig[ , 1] >= 1, 1, -1), dim(ind$coord)[1]),
           nrow = dim(ind$coord)[1], byrow = T)
  score
}
PE1.2.pc <- pcaScoreFun(PE1.2[, c(1, 3:6)])
PE0.2.pc <- pcaScoreFun(PE0.2[, c(1, 3:6)])
# PE0.2.pc2 <- pcaScoreFun(PE0.2[, c(4, 6)])

PE1.t2.pc <- pcaScoreFun(PE1.t2[, c(1, 3:6)])
PE0.t2.pc <- pcaScoreFun(PE0.t2[, c(1, 3:6)])
# PE0.t2.pc2 <- pcaScoreFun(PE0.t2[, c(4, 6)])

#---------------------------------------
# (b). LDA, SVM
#   (i). Predicted class 
factor.fun <- function(x) factor(x, levels = c(0, 1))

classifier.pred <- function(data.x, data.y) {
    # Leave-one-out CV  
    apply(as.matrix(1:length(data.y)), 1, function(row) {
      test.id <- row
      train.id <- setdiff(1:length(data.y), test.id)
  
      x.train <- data.x[train.id, ]
      y.train <- data.y[train.id]
      x.test <- data.x[test.id, ]
        
      ## LDA # library(MASS)
      model.lda <- lda(x = x.train, grouping = y.train)
      lda.pred <- predict(model.lda, x.test)
      lda.class <- factor.fun(lda.pred$class)
      
      ## SVM # library(e1071)
      model.svm <- svm(x.train, y.train)
      svm.pred <- predict(model.svm, matrix(x.test, nrow = 1))
      svm.class <- factor.fun(svm.pred)
      cbind(lda.class, svm.class)-1
      }) %>% t
}
pc.pred.class <- function(data.x, data.y) {
  pred <- apply(as.matrix(2:dim(data.x)[2]), 1, function(dim)
    classifier.pred(data.x[, 1:dim], factor.fun(data.y)))
  lda.class <- head(pred, dim(data.x)[1])
  svm.class <- tail(pred, dim(data.x)[1])
  list(LDA = lda.class, SVM = svm.class)}

PE1.2.pred <- pc.pred.class(PE1.2.pc, PE1.2$Alive)
PE0.2.pred <- pc.pred.class(PE0.2.pc, PE0.2$Alive)
# PE0.2.pred2 <- pc.pred.class(PE0.2.pc2, PE0.2$Alive)
PE0.2.pred2 <- pc.pred.class(scale(PE0.2[, c(4, 6)]), PE0.2$Alive)
  
PE1.t2.pred <- pc.pred.class(PE1.t2.pc, PE1.t2$Alive)
PE0.t2.pred <- pc.pred.class(PE0.t2.pc, PE0.t2$Alive)
# PE0.t2.pred2 <- pc.pred.class(PE0.t2.pc2, PE0.t2$Alive)
PE0.t2.pred2 <- pc.pred.class(scale(PE0.t2[, c(4, 6)]), PE0.t2$Alive)

#   (ii). Measure
accur.fun <- function(pred, actual) {
  confus.mat <- table(pred, actual)
  (accuracy <- sum(diag(confus.mat))/ sum(confus.mat))
}
precision.fun <- function(pred, actual) {
  confus.mat <- table(Actual = actual, Predicted = pred)
  (precision <- confus.mat[1, 1]/ sum(confus.mat[ , 1]))
}
recall.fun <- function(pred, actual) {
  confus.mat <- table(Actual = actual, Predicted = pred)
  (recall <- confus.mat[1, 1]/ sum(confus.mat[1, ]))
}
# PE1
PE1.measure <- function(FUN) {
  PE1.2.m <- lapply(PE1.2.pred, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE1.2$Alive))) %>% data.frame
  PE1.t2.m <- lapply(PE1.t2.pred, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE1.t2$Alive))) %>% data.frame
  data.frame(PE1.2.m, PE1.t2.m)
}
PE1.accur <- PE1.measure(accur.fun)
PE1.precision <- PE1.measure(precision.fun)
PE1.recall <- PE1.measure(recall.fun)
  # Best method: transformation + dim.1~3 + SVM
PE1.best.pred <- pc.pred.class(PE1.t2.pc, PE1.t2$Alive)$SVM[, 2]

# PE0
PE0.measure <- function(FUN) {
  PE0.2.m <- lapply(PE0.2.pred, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.2$Alive))) %>% data.frame
  PE0.2.m2 <- lapply(PE0.2.pred2, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.2$Alive))) %>% data.frame
  
  PE0.t2.m <- lapply(PE0.t2.pred, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.t2$Alive))) %>% data.frame
  PE0.t2.m2 <- lapply(PE0.t2.pred2, function(y)
    apply(y, 2, function(x) 
      FUN(x, PE0.t2$Alive))) %>% data.frame
  data.frame(PE0.2.m, PE0.2.m2,
             PE0.t2.m, PE0.t2.m2)
}
PE0.accur <- PE0.measure(accur.fun)
PE0.precision <- PE0.measure(precision.fun)
PE0.recall <- PE0.measure(recall.fun)
  # Best method: transformation + var.1~2 + LDA 
PE0.best.pred <- pc.pred.class(scale(PE0.t2[, c(4, 6)]), PE0.t2$Alive)$LDA[, 1]


#============================================
# 5. Plot the best method & Explain it
# PE1.best.pred, transformation + dim.1~3 + SVM
# PE0.best.pred, transformation + var.1~2 + LDA 

# (a). PE1
#   (i). PC Scores plot
library(rgl)
#   Actual
plot3d(PE1.t2.pc[, 1:3], 
       type = "s", 
       col = as.numeric(PE1.t2$Alive)+1,
       size = 1.3, xlab = "Dim.1",
       ylab = "Dim.2", zlab = "Dim.3")
#   Predicted class
plot3d(PE1.t2.pc[, 1:3], 
       type = "s", 
       col = PE1.best.pred+2,
       size = 1.3, xlab = "Dim.1",
       ylab = "Dim.2", zlab = "Dim.3")
#   Predicted risks
PE1.sample.death.risk <- apply(as.matrix(1:dim(PE1.t2.pc)[1]), 1, function(x) {
  model.svm <- svm(PE1.t2.pc[-x, 1:3], PE1.t2$Alive[-x],
                   probability = T)
  svm.pred <- predict(model.svm, 
                      matrix(PE1.t2.pc[x, 1:3], nrow = 1),
                      probability = T)
  attributes(svm.pred)$probabilities[2]
})
library(fields)
trans <- function(x) round(x/max(x)*11)+1
risk.col <- designer.colors(12, col = c("green", 
                                        "gray", "red"))
plot3d(PE1.t2.pc[, 1:3], 
       type = "s", 
       col = risk.col[trans(PE1.sample.death.risk)],
       size = 1.3, xlab = "Dim.1",
       ylab = "Dim.2", zlab = "Dim.3")
#   Loadings/Effects
cor(PE1.t2.pc[, 1:3], PE1.sample.death.risk)
cor(PE1.t2.pc[, 1:5], PE1.best.pred)

#-------------------------------------------
# (b). PE0
#   (i). PC Scores plot
#   Actual
plot(scale(PE0.t2[, c(4, 6)]), 
     pch = 16, cex = 1.3,
     col = as.numeric(PE0.t2$Alive)+1,
     xlab = "EPSS_t", ylab = "Wall.Motion_t")
#   Predicted
plot(scale(PE0.t2[, c(4, 6)]), 
     pch = 16, cex = 1.3,
     col = PE0.best.pred+2,
     xlab = "EPSS_t", ylab = "Wall.Motion_t")

#=======================================
# Skip
#=======================================
# (b). Imbalance method
library(ROSE)
# (i). over-sampling  
# (ii). under-sampling
# table(PE1.t2$Alive)
#-------------------
# bal1. over(2), LDA
# bal2. over(2), SVM
# bal3. under(1), LDA
# bal4. under(1), SVM
#-------------------
bal4 <- function(data.x, data.y) {
  Data <- data.frame(data.x, data.y)
  colnames(Data)[dim(Data)[2]] <- "Alive"

  # Leave-one-out CV
  apply(as.matrix(1:length(data.y)), 1, function(row) {
    test.id <- row
    train.id <- setdiff(1:length(data.y), test.id)
    apply(as.matrix(1:300), 1, function(x) {
      bal <- ovun.sample(Alive ~ ., data = Data[train.id, ], 
                         method = "under", # "over" 
                         N = table(data.y[train.id])[1]*2,
                         seed = x)$data # 1
      x.train <- bal[ , -dim(Data)[2]]
      y.train <- bal[ , dim(Data)[2]]
      x.test <- data.x[test.id, ]
      
      ## LDA # library(MASS)
      #model.lda <- lda(x = x.train, grouping = y.train)
      #lda.pred <- predict(model.lda, x.test)
      #factor(lda.pred$class, levels = c(0, 1))
      
      ## SVM # library(e1071)
      model.svm <- svm(x.train, y.train)
      svm.pred <- predict(model.svm, matrix(x.test, nrow = 1))
      factor(svm.pred, levels = c(0, 1))
    }) %>% table %>% which.max-1
   
    })
}

PE1.over.lda <- bal1(PE1.pc, PE1.t2$Alive)
PE1.over.svm <- bal2(PE1.pc, PE1.t2$Alive)
PE1.under.lda <- bal3(PE1.pc, PE1.t2$Alive)
PE1.under.svm <- bal4(PE1.pc, PE1.t2$Alive)

PE0.over.lda <- bal1(PE0.pc, PE0.t2$Alive)
PE0.over.svm <- bal2(PE0.pc, PE0.t2$Alive)
PE0.under.lda <- bal3(PE0.pc, PE0.t2$Alive)
PE0.under.svm <- bal4(PE0.pc, PE0.t2$Alive)

# (c) Measure
accur.fun <- function(pred, actual) {
  confus.mat <- table(pred, actual)
  (accuracy <- sum(diag(confus.mat))/ sum(confus.mat))
}
FPR.fun <- function(pred, actual) {
  confus.mat <- table(Actual = actual, Predicted = pred)
  (FPR <- confus.mat[2, 1]/ sum(confus.mat[2, ]))
}
TPR.fun <- function(pred, actual) {
  confus.mat <- table(Actual = actual, Predicted = pred)
  (TPR <- confus.mat[1, 1]/ sum(confus.mat[1, ]))
}
factor.fun <- function(x) factor(x, levels = c(1, 0))

PE1.pred <- data.frame(PE1.over.lda, PE1.over.svm,
                       PE1.under.lda, PE1.under.svm)
PE0.pred <- data.frame(PE0.over.lda, PE0.over.svm,
                       PE0.under.lda, PE0.under.svm)

PE1.measure <- t(apply(PE1.pred, 2, function(x) {
  Actual <- factor.fun(PE1.t2$Alive)
  Predict <- factor.fun(x)
  c(FPR.fun(Actual, Predict), 
    TPR.fun(Actual, Predict), accur.fun(Actual, Predict))
}))
colnames(PE1.measure) <- c("FPR", "TPR", "Accuracy")

PE0.measure <- t(apply(PE0.pred, 2, function(x) {
  Actual <- factor.fun(PE0.t2$Alive)
  Predict <- factor.fun(x)
  c(FPR.fun(Actual, Predict), 
    TPR.fun(Actual, Predict), accur.fun(Actual, Predict))
}))
colnames(PE0.measure) <- c("FPR", "TPR", "Accuracy")
#=========================================
#=========================================

