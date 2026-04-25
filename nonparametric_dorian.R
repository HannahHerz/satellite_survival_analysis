################################################################################
# PART B: Survival Analysis using Satellite dataset
################################################################################
library(KMsurv)
library(survival)
library(survminer)
library(survMisc)

Satellite <- read.csv("data/02_data_grouped.csv")
str(Satellite)
#Had to do this otherwise I got too much errors
Satellite$time_days <- as.numeric(Satellite$time_days)
Satellite$event <- as.numeric(Satellite$event)
table(Satellite$event)


table(Satellite$purpose_grouped)
table(Satellite$purpose_grouped, Satellite$event)
Satellite$purpose_3 <- ifelse(
  Satellite$purpose_grouped %in% c("Navigation", "Other", "Science", "Technology"),
  "Other",
  Satellite$purpose_grouped
)

Satellite$purpose_3 <- as.factor(Satellite$purpose_3)

# Check grouping
table(Satellite$purpose_3)
table(Satellite$purpose_3, Satellite$event)

# PART B(a): Kaplan-Meier estimator
par(mfrow=c(1,1))
km_fit <- survfit(
  Surv(time_days, event) ~ purpose_3,
  data = Satellite,
  conf.type = "log-log"
)

print(km_fit)
summary(km_fit)

plot(
  km_fit,
  col = 1:length(levels(Satellite$purpose_3)),
  lwd = 2,
  lty = 1,
  mark.time = TRUE,
  xlab = "Time since launch (days)",
  ylab = "Survival probability",
  main = "Kaplan-Meier Curves by Satellite Purpose"
)

legend(
  "topright",
  legend = levels(Satellite$purpose_3),
  col = 1:length(levels(Satellite$purpose_3)),
  lwd = 2,
  lty = 1,
  cex = 0.8
)


# PART B(b): Quartiles + Confidence Intervals
quartiles <- quantile(
  km_fit,
  probs = c(0.25, 0.50, 0.75)
)

quartiles

quartile_table <- data.frame(
  Group = rownames(quartiles$quantile),
  Q1 = quartiles$quantile[, 1],
  Q1_lower = quartiles$lower[, 1],
  Q1_upper = quartiles$upper[, 1],
  Median = quartiles$quantile[, 2],
  Median_lower = quartiles$lower[, 2],
  Median_upper = quartiles$upper[, 2],
  Q3 = quartiles$quantile[, 3],
  Q3_lower = quartiles$lower[, 3],
  Q3_upper = quartiles$upper[, 3]
)

quartile_table


# PART B(c): Hypothesis Testing
# Log-rank test 
logrank_test <- survdiff(
  Surv(time_days, event) ~ purpose_3,
  data = Satellite,
  rho = 0
)

logrank_test

# p-value
logrank_p <- 1 - pchisq(
  logrank_test$chisq,
  df = length(logrank_test$n) - 1
)

logrank_p

# Wilcoxon test 
wilcoxon_test <- survdiff(
  Surv(time_days, event) ~ purpose_3,
  data = Satellite,
  rho = 1
)

wilcoxon_test

wilcoxon_p <- 1 - pchisq(
  wilcoxon_test$chisq,
  df = length(wilcoxon_test$n) - 1
)

wilcoxon_p



#Extra diagnostics
table(Satellite$purpose_3, Satellite$event)
prop.table(table(Satellite$purpose_3, Satellite$event), margin = 1)
