# Input data, which line 8,10,11,12 are ignored
heart <- read.table("data\\echocardiogram.data.txt",
                    sep = ",")[, c(1:7, 9)]
# Variables' name
colnames(heart) <- c("Survival", "Still alive", 
                     "Age", "PE", "FS", "EPSS", "LVDD", 
                     "Wall Motion")
# Replace ? with NA
heart[heart == "?"] <- NA
# De-factorized continuous variables
num.heart <- as.data.frame(apply(heart, 2, as.numeric))
num.heart[, "Still alive"] <- as.factor(num.heart[, "Still alive"])
num.heart[, "PE"] <- as.factor(num.heart[, "PE"])
# View the dataset, num.heart
dim(num.heart)
str(num.heart)
head(num.heart)
summary(num.heart)

# Missing value Visualization
# (a) Prob. of NAs for each variable/record
pMiss <- apply(num.heart, 2, 
               function(var) sum(is.na(var))/length(var)) %>% round(4)
r.pMiss <- apply(num.heart, 1, 
                 function(row) sum(is.na(row))/length(row)) %>% round(4)
# (b) Visualize
#   1. Histogram of NAs in Prob.
s.pMiss <- sort(pMiss, d = T)
barplot(s.pMiss, ylim = c(0, 0.15),
        col = "orange",
        main = "Histogram of Missing Values 
in Probabilities", xlab = "",
        ylab = "Probability")
text((1:length(s.pMiss))*1.2 - 0.5, s.pMiss + 0.005,
     paste(s.pMiss*100, "%"))
#   2. Heatmap of NAs through dataset (sorted)
# Raw
heatmap(ifelse(is.na(num.heart), 1, 0), 
        scale = "none", 
        Rowv = NA, Colv = NA, revC = T,
        col = c("gray", "orange"),
        main = "Missing values in dataset",
        margins = c(6, 12), cexCol = 1.2,
        cex = 0.7)

?heatmap
# Ordered
s.na.data <- num.heart[order(r.pMiss), 
                       rev(order(pMiss))]
s.na.boolData <- ifelse(is.na(s.na.data), 1, 0)
heatmap(s.na.boolData, scale = "none",
        Rowv = NA, Colv = NA, 
        col = c("gray", "orange"),
        RowSideColors = ifelse(sort(r.pMiss) > 0, 
                               "red", "green"),
        main = "Missing values in ordered dataset",
        margins = c(7, 16), cexCol = 1.3,
        cex = 0.7)
(r.int.ratio <- sum(r.pMiss == 0)/length(r.pMiss))
  # 81%: integral records(no NAs)
na.ratio <- sum(s.na.boolData)/(dim(s.na.boolData)[1]*dim(s.na.boolData)[2])
  # 4%: missing data

# Add one variable, "Alive"(alive in a year),
# determined by "Surrvival" and "Still alive"
Alive <- ifelse(is.na(num.heart$Survival) | 
                  is.na(num.heart$`Still alive`) , NA,
                ifelse(num.heart$Survival >= 24, 1,
                       ifelse(num.heart$`Still alive` == 0, 0, 2)))
d.heart <- data.frame(num.heart, Alive = as.factor(Alive))
# str(d.heart)
# summary(d.heart)

# EDA before NAs Imputation
# (Ignore missimg values and outliers)
del.na.data <- d.heart[r.pMiss == 0, ]
outlier <- apply(del.na.data[ , c(1, 3, 5:8)], 2, function(x)
  which(scale(x) > 3 | scale(x) < -3)) %>% unlist %>% unique
del.na.out.data <- del.na.data[-outlier , ]

# (a) Discrete variables("Still alive", "PE", "Alive)
par(mfcol = c(1, 3))
disc.var <- apply(as.matrix(c(2, 4, 9)), 1, function(num){
  var <- del.na.out.data[ , num]
  barplot(prop.table(table(var)), 
          main = colnames(del.na.out.data)[num])
})

# (b) Continuous variables
#   1. class
par(mfcol = c(2, 3))
cont.var <- apply(as.matrix(c(1, 3, 5:8)), 1, function(num){
  var <- del.na.out.data[ , num]
  hist(var, xlab = "",
       main = colnames(del.na.out.data)[num],
       col = tim.colors(8)[num])
})
pairs(del.na.out.data[ , c(1, 3, 5:8)], 
      col = as.numeric(del.na.out.data$Alive)+1)
mds.heart <- cmdscale(dist(scale(del.na.out.data[,c(1, 3, 5:8)])), k = 2)
plot(mds.heart[ , 1], mds.heart[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS 1", col = as.numeric(del.na.out.data$Alive) + 1, pch = 16)

#   2. Alive = 2 ignored
del.na.out.data2 <- del.na.out.data[del.na.out.data$Alive != 2, ]
pairs(del.na.out.data2[ , c(1, 3, 5:8)], 
      col = as.numeric(del.na.out.data2$Alive)+1)
mds.heart2 <- cmdscale(dist(scale(del.na.out.data2[,c(1, 3, 5:8)])), k = 2)
plot(mds.heart2[ , 1], mds.heart2[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS 2", col = as.numeric(del.na.out.data2$Alive) + 1, pch = 16)

#   3.
pairs(del.na.out.data[ , c(1, 3, 5:8)], 
      col = terrain.colors(58)[del.na.out.data$Survival+1],
      pch = c("x", "o")[del.na.out.data$Still.alive])
plot(mds.heart[ , 1], mds.heart[ , 2], xlab = "dim1", ylab = "dim2",
     main = "MDS 3", 
     col = terrain.colors(58)[del.na.out.data$Survival+1],
     pch = c("x", "o")[del.na.out.data$Still.alive])


