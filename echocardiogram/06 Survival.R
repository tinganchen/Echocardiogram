library(KMsurv)
library(survival)

# imp.data

delta <- ifelse(del.out.data$Still.alive == 1, 0, 1)
  # mean(delta)
y <- Surv(del.out.data$Survival, delta)

# kaplan-meier curve
kmfit <- survfit(y~1, type = "kaplan-meier")
summary(kmfit)
plot(kmfit, mark.time = F,
     main = "Kaplan Meier Plot",
     xlab = "Time (Months)", ylab = "Survival function") 
legend("topright", c('K-M survival estimate',
                     'Pointwise intervals'), lty = 1:2)

# H(t)_t圖
Hazard.fit <- survfit(y~1, type = "fh")
Hazard <- -log(Hazard.fit$surv)

plot(Hazard.fit$time, Hazard, type = "l",
     main = "Cumulative Hazard", 
     xlab = "Time (Months)", ylab = "") 

# Log-rank test for two sample
  # Test Survival Curve Differences 
LogRank.fit <- survdiff(y~del.out.data$PE)
LogRank.fit # No sig. diff. on hazard rate.

# Cox Proportional Hazards Model
Coxph.fit <- coxph(y~., data = del.out.data[ , 3:8])
Coxph.fit

