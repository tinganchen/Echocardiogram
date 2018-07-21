library (MASS)
# 1. Total
fit.lda <- lda(x = del.out.data2[, 3:8], 
               grouping = as.numeric(del.out.data2[, 9])-1, CV = T)
confus.mat <- table(as.numeric(del.out.data2[, 9])-1, fit.lda$class)
accuracy <- sum(diag(confus.mat))/sum(confus.mat)
accuracy

# prop.table(table(del.out.data2[ , 9]))

raw.lda <- lda(x = del.out.data2[, 3:8], 
               grouping = as.numeric(del.out.data2[, 9])-1)
LDscores <- apply(del.out.data2[, 3:8], 2, as.numeric) %*% 
  as.matrix(raw.lda$scaling)
sm.density.compare(as.vector(LDscores), 
                   as.numeric(del.out.data2[, 9])-1,
                   col = 2:3, lty = rep(1, 2), lwd = 2)        
# 2. PE = 0
fit.lda0 <- lda(x = del.out.data2[del.out.data2$PE == 0, c(3, 5:8)], 
               grouping = as.numeric(del.out.data2[del.out.data2$PE == 0, 9])-1, CV = T)
confus.mat0 <- table(as.numeric(del.out.data2[del.out.data2$PE == 0, 9])-1, fit.lda0$class)
accuracy0 <- sum(diag(confus.mat0))/sum(confus.mat0)
accuracy0
# 3. PE = 1
fit.lda1 <- lda(x = del.out.data2[del.out.data2$PE == 1, c(3, 5:8)], 
                grouping = as.numeric(del.out.data2[del.out.data2$PE == 1, 9])-1, CV = T)
confus.mat1 <- table(as.numeric(del.out.data2[del.out.data2$PE == 1, 9])-1, fit.lda1$class)
accuracy1 <- sum(diag(confus.mat1))/sum(confus.mat1)
accuracy1
