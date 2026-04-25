df<- readr::read_csv("02_data_grouped.csv")   # tidyverse (faster)
# Take a random sample of 20 rows
sample_df <- df[sample(nrow(df), 20), ]

# Export to CSV
write.csv(sample_df, "sample_data.csv", row.names = FALSE)
###################################################################################
################ Purpose as categorical variable #########################################
#################################################################################
table(df$purpose_grouped)
table(df$purpose_grouped, df$event)

#Course requires max 4 categorical variables so grouping required

df$purpose_3 <- ifelse(df$purpose_grouped %in% c("Navigation", "Other", "Science", "Technology"),
                       "Other", df$purpose_grouped)

df$purpose_3 <- as.factor(df$purpose_3)

table(df$purpose_3, df$event)


library(survival)

km_fit <- survfit(Surv(time_days, event) ~ purpose_3, data = df)
plot(km_fit,
     col = c("blue", "red", "green"),
     lty = 1,
     xlab = "Time (days)",
     ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curves by Satellite Purpose")

legend("topright",
       legend = levels(df$purpose_3),
       col = c("blue", "red", "green"),
       lty = 1)

print(km_fit)

quantile(km_fit, probs = c(0.25, 0.50, 0.75))

survdiff(Surv(time_days, event) ~ purpose_3, data = df, rho = 1)

###################################################################################
################ COuntry of origin as categorical varaible #################################
#################################################################################
table(df$country_grouped, df$event)
#Again 5 groups are in the data we merge Russian and Other and make a new group called rest of the world

df$country_4 <- ifelse(df$country_grouped %in% c("Russia", "Other"),
                       "Rest of World", df$country_grouped)

df$country_4 <- as.factor(df$country_4)

table(df$country_4, df$event)

km_fit_2 <- survfit(Surv(time_days, event) ~ country_4, data=df)
print(km_fit_2
    )



plot(km_fit_2,
     col = c("blue", "red", "green", "purple"),
     lty = 1,
     xlab = "Time (days)",
     ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curves by Country of Origin")

legend("topright",
       legend = levels(df$country_4),
       col = c("blue", "red", "green", "purple"),
       lty = 1)


quantile(km_fit_2, probs = c(0.25, 0.50, 0.75))
survdiff(Surv(time_days, event) ~ country_4, data = df, rho = 1)




##################################################################################
################ Purpose as categorical varaible #################################
#################################################################################




table(df$users_grouped, df$event)

#Again 5 groups Government and Civil seemed the most logical to group 

df$users_4 <- ifelse(df$users_grouped %in% c("Civil", "Government"),
                     "Government/Civil", df$users_grouped)

df$users_4 <- as.factor(df$users_4)

table(df$users_4, df$event)






km_fit_3 <- survfit(Surv(time_days, event) ~ users_4, data = df)


print(km_fit_3)


quantile(km_fit_3, probs = c(0.25, 0.50, 0.75))


plot(km_fit_3,
     col = c("blue", "red", "green", "purple"),
     lty = 1,
     xlab = "Time (days)",
     ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curves by Satellite User Type")

legend("topright",
       legend = levels(df$users_4),
       col = c("blue", "red", "green", "purple"),
       lty = 1)

survdiff(Surv(time_days, event) ~ users_4, data = df, rho = 1)



install.packages("gridExtra")


library(survival)
library(gridExtra)
library(grid)

# ── 1. KM PLOTS ──────────────────────────────────────────────────────────────
png("KM_plots.png", width = 1800, height = 600, res = 150)
par(mfrow = c(1, 3))

# Purpose
plot(km_fit,
     col = c("blue", "red", "green"), lty = 1,
     xlab = "Time (days)", ylab = "Survival Probability",
     main = "KM Curves by Satellite Purpose")
legend("topright", legend = levels(df$purpose_3),
       col = c("blue", "red", "green"), lty = 1, cex = 0.8)

# Country
plot(km_fit_2,
     col = c("blue", "red", "green", "purple"), lty = 1,
     xlab = "Time (days)", ylab = "Survival Probability",
     main = "KM Curves by Country of Origin")
legend("topright", legend = levels(df$country_4),
       col = c("blue", "red", "green", "purple"), lty = 1, cex = 0.8)

# User type
plot(km_fit_3,
     col = c("blue", "red", "green", "purple"), lty = 1,
     xlab = "Time (days)", ylab = "Survival Probability",
     main = "KM Curves by User Type")
legend("topright", legend = levels(df$users_4),
       col = c("blue", "red", "green", "purple"), lty = 1, cex = 0.8)

dev.off()

# ── 2. QUARTILE TABLES ───────────────────────────────────────────────────────
make_table <- function(km_fit, groups) {
  q <- quantile(km_fit, probs = c(0.25, 0.50, 0.75))
  data.frame(
    Group    = groups,
    Q1       = q$quantile[, 1],
    Q1_lower = q$lower[, 1],
    Q1_upper = q$upper[, 1],
    Median   = q$quantile[, 2],
    Med_lower= q$lower[, 2],
    Med_upper= q$upper[, 2],
    Q3       = q$quantile[, 3],
    Q3_lower = q$lower[, 3],
    Q3_upper = q$upper[, 3]
  )
}

t1 <- make_table(km_fit,  levels(df$purpose_3))
t2 <- make_table(km_fit_2, levels(df$country_4))
t3 <- make_table(km_fit_3, levels(df$users_4))

png("Quartile_tables.png", width = 2000, height = 900, res = 150)
grid.arrange(
  tableGrob(t1, rows = NULL, theme = ttheme_default(base_size = 9)),
  tableGrob(t2, rows = NULL, theme = ttheme_default(base_size = 9)),
  tableGrob(t3, rows = NULL, theme = ttheme_default(base_size = 9)),
  nrow = 3,
  top = textGrob("Quartile Estimates (days) with 95% CIs", 
                 gp = gpar(fontsize = 14, fontface = "bold"))
)
dev.off()