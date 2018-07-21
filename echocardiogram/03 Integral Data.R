# EDA after NAs Imputation
# (Ignore outliers)
outlier2 <- apply(imp.data[ , c(1, 3, 5:8)], 2, function(x)
  which(scale(x) > 3 | scale(x) < -3)) %>% unlist %>% unique
del.out.data <- imp.data[-outlier2 , ]

# (a) Discrete variables("Still alive", "PE", "Alive)
par(mfcol = c(1, 3))
disc.var2 <- apply(as.matrix(c(2, 4, 9)), 1, function(num){
  var <- del.out.data[ , num]
  barplot(prop.table(table(var)), 
          main = colnames(del.out.data)[num])
})

# (b) Continuous variables
#   1. class
par(mfcol = c(2, 3))
cont.var2 <- apply(as.matrix(c(1, 3, 5:8)), 1, function(num){
  var <- del.out.data[ , num]
  hist(var, xlab = "",
       main = colnames(del.out.data)[num],
       col = tim.colors(8)[num])
})
pairs(del.out.data[ , c(1, 3, 5:8)], 
      col = as.numeric(del.out.data$Alive)+1)
mds.data <- cmdscale(dist(scale(del.out.data[,c(1, 3, 5:8)])), k = 2)
plot(mds.data[ , 1], mds.data[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS 1", col = as.numeric(del.out.data$Alive) + 1, pch = 16)

#   2. Alive = 2 ignored
del.out.data2 <- del.out.data[del.out.data$Alive != 2, ]
pairs(del.out.data2[ , c(1, 3, 5:8)], 
      col = as.numeric(del.out.data2$Alive)+1)
mds.data2 <- cmdscale(dist(scale(del.out.data2[,c(1, 3, 5:8)])), k = 2)
plot(mds.data2[ , 1], mds.data2[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS 2", col = as.numeric(del.out.data2$Alive) + 1, pch = 16)

#   3.
pairs(del.out.data[ , c(1, 3, 5:8)], 
      col = terrain.colors(58)[del.out.data$Survival+1],
      pch = c("x", "o")[del.out.data$Still.alive])
plot(mds.data[ , 1], mds.data[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS 3", 
     col = terrain.colors(58)[del.out.data$Survival+1],
     pch = c("x", "o")[del.out.data$Still.alive])
#---------------------------------------------------
temp <- del.out.data[del.out.data$Still.alive == 0, ]
# dim(temp)
mds.temp <- cmdscale(dist(scale(temp[,c(-1, -2, -4, -9)])), k = 2)
trans <- function(x) round(x/max(x)*11)+1
col12 <- designer.colors(12, col = c("red", "gray", "blue"))
plot(mds.temp[ , 1], mds.temp[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS", pch = 16, cex = 1.2, 
     col = col12[trans(temp[ , 1])])
pairs(temp[, c(-1, -2)], col = col12[trans(temp[ , 1])],
      pch = 16)
