### Imputation

## Association between variables
# (a) conti.-conti.
var.corr <- cor(del.na.data[ , c(1, 3, 5:8)])
library(fields)
corr.col <- two.colors(n = 21, start = "blue",
                       middle = "white", end = "red")
heatmap((var.corr[dim(var.corr)[1]:1, ]+1)*10+1,
        scale = "none", Rowv = NA, Colv = NA,
        col = corr.col)
  # age is not correlated to others
# (b) discrete-conti.
library(sm)
sm.density.compare(del.na.data[ , 8],
                   del.na.data[ , 4])


## KNN Imputation 
# Integral data with no NA used to train and test
integ.data <- num.heart[r.pMiss == 0, ] # No NAs

# Simulate missing values
library(missForest)
set.seed(123)
na.simulate.data <- prodNA(integ.data, noNA = na.ratio)
# Scaled
num.var <- unlist(lapply(na.simulate.data, is.numeric))
scale.na.simul.data <- na.simulate.data
scale.na.simul.data[ , num.var] <- scale(na.simulate.data[ , num.var])
  # scale.na.simul.data

# kNN
# ?knnImputation
library(DMwR)
simul.knnOutput <- knnImputation(scale.na.simul.data, 
                                 k = 5, meth = 'weighAvg')
# scale integ.data by mean/sd of na.simulate.data
sd.scale.na.simul <- apply(na.simulate.data[, num.var], 2, sd, na.rm = T)
mat.simul.sd <- matrix(rep(sd.scale.na.simul, dim(integ.data)[1]),
                       nrow = dim(integ.data)[1], byrow = T)
mean.scale.na.simul <- apply(na.simulate.data[, num.var], 2, mean, na.rm = T)
mat.simul.mean <- matrix(rep(mean.scale.na.simul, dim(integ.data)[1]),
                         nrow = dim(integ.data)[1], byrow = T)

scale.integ.data <- integ.data
scale.integ.data[ , num.var] <- (integ.data[ , num.var]-mat.simul.mean)/mat.simul.sd

# continuous variables
simul.RMSE <- 
  (sum((simul.knnOutput[, num.var] - scale.integ.data[, num.var])^2)/
  sum(is.na(na.simulate.data[ , num.var]))) %>% sqrt
# categorical variables
categ.var.err.rate <- 
  (sum(simul.knnOutput[ , 2] != scale.integ.data[ , 2])+
    sum(simul.knnOutput[ , 4] != scale.integ.data[ , 4]))/
    sum(is.na(na.simulate.data[ , num.var]))


## KNN Imputation (consider #of simulation, k, method)
#(input seed, k, method; output RMSE, error rate)
# Simulate missing values
library(missForest)
library(DMwR)
simul.knnImpute.fun <- function(seed, K, method) {
  set.seed(seed)
  na.simulate.data <- prodNA(integ.data, noNA = na.ratio)
  # Scaled
  num.var <- unlist(lapply(na.simulate.data, is.numeric))
  scale.na.simul.data <- na.simulate.data
  scale.na.simul.data[ , num.var] <- scale(na.simulate.data[ , num.var])
  # scale.na.simul.data
  
  # kNN
  simul.knnOutput <- knnImputation(scale.na.simul.data, 
                                   k = K, meth = method)
  # scale integ.data by mean/sd of na.simulate.data
  sd.scale.na.simul <- apply(na.simulate.data[, num.var], 2, sd, na.rm = T)
  mat.simul.sd <- matrix(rep(sd.scale.na.simul, dim(integ.data)[1]),
                         nrow = dim(integ.data)[1], byrow = T)
  mean.scale.na.simul <- apply(na.simulate.data[, num.var], 2, mean, na.rm = T)
  mat.simul.mean <- matrix(rep(mean.scale.na.simul, dim(integ.data)[1]),
                           nrow = dim(integ.data)[1], byrow = T)
  
  scale.integ.data <- integ.data
  scale.integ.data[ , num.var] <- (integ.data[ , num.var]-mat.simul.mean)/mat.simul.sd
  
  # continuous variables
  simul.RMSE <- 
    (sum((simul.knnOutput[, num.var] - scale.integ.data[, num.var])^2)/
       sum(is.na(na.simulate.data[ , num.var]))) %>% sqrt
  # categorical variables
  categ.var.err.rate <- 
    (sum(simul.knnOutput[ , 2] != scale.integ.data[ , 2])+
       sum(simul.knnOutput[ , 4] != scale.integ.data[ , 4]))/
    sum(is.na(na.simulate.data[ , num.var]))
  
  data.frame(RMSE = simul.RMSE, Error = categ.var.err.rate)
}
simulation <- 1:500
knn.k <- 1:50
simul.knn.measure <- apply(simulation %>% as.matrix, 1, function(x)
  apply(knn.k %>% as.matrix, 1, function(y) 
    apply(c('median', 'weighAvg') %>% as.matrix, 1, function(z)
      simul.knnImpute.fun(x, y, z))))
# mean in simulations
knn.median <- apply(as.matrix(1:length(simulation)), 1, function(y)lapply(simul.knn.measure, function(x)
  matrix(unlist(x), nrow = 2)[, 2*(1:length(knn.k))-1])[[y]]) %>% 
  rowSums %>% matrix(nrow = 2)/length(simulation)
knn.weight <- apply(as.matrix(1:length(simulation)), 1, function(y)lapply(simul.knn.measure, function(x)
  matrix(unlist(x), nrow = 2)[, 2*(1:length(knn.k))])[[y]]) %>% 
  rowSums %>% matrix(nrow = 2)/length(simulation)
# sd in simulations
knn.median.sd <- apply(as.matrix(1:length(simulation)), 1, function(y)lapply(simul.knn.measure, function(x)
  matrix(unlist(x), nrow = 2)[, 2*(1:length(knn.k))-1])[[y]]) %>% 
  apply(1, sd) %>% matrix(nrow = 2)
knn.weight.sd <- apply(as.matrix(1:length(simulation)), 1, function(y)lapply(simul.knn.measure, function(x)
  matrix(unlist(x), nrow = 2)[, 2*(1:length(knn.k))])[[y]]) %>% 
  apply(1, sd) %>% matrix(nrow = 2)

# Plot-- Error rate
# knn.Error <- data.frame(Median = knn.median[2, ], 
#                         Weight.Average = knn.weight[2, ])

# matplot(knn.Error, type = "l", col = c(4, 2),
#         lty = 1, lwd = 2, xaxt = "n", xlab = "K", 
#         ylab = "Error rate", ylim = c(0.19, 0.25),
#         main = paste("KNN Imputation Measure II  ( N=", 
#                      length(simulation), ")"))
# axis(1, c(1, (1:(length(knn.k)/5))*5), 
#      c(1, (1:(length(knn.k)/5))*5))
# legend("topright", c("Median", "Weighted Average"),
#        lty = 1, lwd = 2, col = c(4, 2))

# Plot-- RMSE
knn.RMSE <- data.frame(Median = knn.median[1, ], 
                       Weight.Average = knn.weight[1, ])
knn.RMSE.sd <- cbind(knn.median.sd[1, ], 
                     knn.weight.sd[1, ])

matplot(knn.RMSE, xaxt = "n", xlab = "K", ylab = "RMSE",
        main = paste("KNN Imputation Measure ( N=", 
                     length(simulation), ")"),
        type = "l", lty = 1, lwd = 2, col = c(4, 2))
axis(1, c(1, (1:(length(knn.k)/5))*5), 
     c(1, (1:(length(knn.k)/5))*5))
legend("topright", c("Median", "Weighted Average"),
       lty = 1, lwd = 2, col = c(4, 2))
  # k = 7, method = weighted average

# Impute the whole data(standardized) 
scale.num.heart <- num.heart
scale.num.heart[, num.var] <- scale(num.heart[, num.var])
scale.knnImp.heart <- knnImputation(scale.num.heart, 
                                    k = 7, meth = 'weighAvg')
# 
sd.scale.na <- apply(num.heart[, num.var], 2, sd, na.rm = T)
mat.sd <- matrix(rep(sd.scale.na, dim(num.heart)[1]),
                 nrow = dim(num.heart)[1], byrow = T)
mean.scale.na <- apply(num.heart[, num.var], 2, mean, na.rm = T)
mat.mean <- matrix(rep(mean.scale.na, dim(num.heart)[1]),
                   nrow = dim(num.heart)[1], byrow = T)

knnImp.heart <- scale.knnImp.heart
knnImp.heart[, num.var] <- scale.knnImp.heart[, num.var]*mat.sd+mat.mean
# knnImp.heart
  # dim(knnImp.heart)

knnImp.Alive <- ifelse(is.na(knnImp.heart$Survival) | 
                         is.na(knnImp.heart$`Still alive`), NA,
                       ifelse(knnImp.heart$Survival >= 24, 1,
                              ifelse(knnImp.heart$`Still alive` == 0, 0, 2)))

imp.data <- data.frame(knnImp.heart, Alive = as.factor(knnImp.Alive))
# dim(imp.data)
# str(imp.data)
# summary(imp.data)
write.csv(imp.data,
          file = "C:\\Users\\Grace\\Desktop\\107-NTPU-Project\\echocardiogram\\data\\imputeData.csv",
          row.names = FALSE)
