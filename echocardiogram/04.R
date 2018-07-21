imp.data.no2 <- imp.data[imp.data$Alive != 2,]
# dim(imp.data.no2)

## Density plots for cont. var.
#  (check non-linear relation)
library(sm)
# (a) Consider outliers
par(mfrow = c(2, 3))
par(mai = c(0.3, 0.6, 0.6, 0.2))
apply(as.matrix(c(3, 5:8)), 1, function(x) {
  sm.density.compare(imp.data.no2[ , x], 
                     imp.data.no2$Alive,
                     col = 2:3, 
                     lty = rep(1, 2), lwd = 2,
                     xlab = "")
  title(paste("\n\n", colnames(imp.data.no2)[x]))
})
legend("topright", c("Survival", "Death"),
       col = 3:2, cex = 1.1, lty = 1, lwd = 2)
title("\nDensity plot",
      outer = T, cex.main = 2)

# (b) Not consider outliers
# del.out.data2
par(mfrow = c(2, 3))
par(mai = c(0.3, 0.6, 0.6, 0.2))
apply(as.matrix(c(3, 5:8)), 1, function(x) {
  sm.density.compare(del.out.data2[ , x], 
                     del.out.data2$Alive,
                     col = 2:3, 
                     lty = rep(1, 2), lwd = 2,
                     xlab = "")
  title(paste("\n\n", colnames(del.out.data2)[x]))
})
legend("topright", c("Survival", "Death"),
       col = 3:2, cex = 1.1, lty = 1, lwd = 2)
title("\nDensity plot without Outliers",
      outer = T, cex.main = 2)
