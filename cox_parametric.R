################################################################################
# ── Cox PH model + parametric AFT models
################################################################################

library(survival)
library(survminer)
library(MASS)
library(car)
source("lib/ggcoxzphFixed.R")

# Helper methods for printing
catn  <- function(...) { cat(...); cat("\n") }
ncatn <- function(...) { cat("\n"); cat(...); cat("\n") }

Satellite <- read.csv("data/02_data_grouped.csv")
Satellite$time_days <- as.numeric(Satellite$time_days)
Satellite$event <- as.numeric(Satellite$event)

# Categorical variable from Part B (must remain in the model)
Satellite$purpose_3 <- ifelse(
  Satellite$purpose_grouped %in% c("Navigation", "Other", "Science", "Technology"),
  "Other",
  Satellite$purpose_grouped
)
Satellite$purpose_3 <- factor(Satellite$purpose_3,
  levels = c("Communications", "Earth Observation", "Other"))

# Other categorical predictors -> factor
Satellite$country_grouped     <- factor(Satellite$country_grouped, levels = c("US","China","Europe","Russia","Other"))
Satellite$users_grouped       <- factor(Satellite$users_grouped,   levels = c("Commercial","Civil","Government","Military","Mixed"))
Satellite$orbit_altitude_class<- factor(Satellite$orbit_altitude_class, levels = c("LEO","MEO","GEO","ELLIPTICAL"))
Satellite$orbit_geometry_type <- factor(Satellite$orbit_geometry_type,  levels = c("Inclined","Polar","Sun-Sync","Other"))

table(Satellite$event)

# ── Cox PH model ──────────────────────────────────────────────────────────────

# Full model: all candidate covariates.
# highly correlated vars so select one:
# perigee/apogee                       -> log_perigee.
# country_grouped/launch_vehicle_group -> country_grouped
fit_full <- coxph(
  Surv(time_days, event) ~ purpose_3 + country_grouped + users_grouped +
    orbit_altitude_class + orbit_geometry_type +
    log_mass + log_perigee + inclination +
    expected_lifetime + expected_lifetime_missing,
  data = Satellite,
  ties = "breslow"
)
summary(fit_full)

# Stepwise selection with purpose_3 forced in
fit_step <- stepAIC(
  fit_full,
  scope = list(
    lower = ~ purpose_3,
    upper = ~ purpose_3 + country_grouped + users_grouped +
              orbit_altitude_class + orbit_geometry_type +
              log_mass + log_perigee + inclination +
              expected_lifetime + expected_lifetime_missing
  ),
  direction = "both"
)
summary(fit_step)

exp(coef(fit_step))
exp(confint(fit_step))

# Test of the categorical Part-B variable in the selected model
fit_no_purpose <- update(fit_step, . ~ . - purpose_3)
anova(fit_no_purpose, fit_step, test = "Chisq")
linearHypothesis(fit_step,
  c("purpose_3Earth Observation = 0", "purpose_3Other = 0"))

# PH assumption teseting with Schoenfeld residuals
zph <- cox.zph(fit_step)
print(zph)

png("plots/cox_Schoenfeld.png", width = 1800, height = 1200, res = 130)
plot(zph)
dev.off()

png("plots/cox_Schoenfeld_gg.png", width = 1800, height = 2200, res = 130)
print(ggcoxzphFixed(zph))
dev.off()


# ── Parametric AFT models ──────────────────────────────────────────────────────────────

# same covariates as the selected Cox model
form <- formula(fit_step)

fit_weib <- survreg(form, data = Satellite, dist = "weibull")
fit_exp  <- survreg(form, data = Satellite, dist = "exponential")
fit_logn <- survreg(form, data = Satellite, dist = "lognormal")
fit_logl <- survreg(form, data = Satellite, dist = "loglogistic")

AIC <- c(
  weibull       = extractAIC(fit_weib)[2],
  exponential   = extractAIC(fit_exp )[2],
  lognormal     = extractAIC(fit_logn)[2],
  loglogistic   = extractAIC(fit_logl)[2]
)
print(AIC)

best_name <- names(AIC)[which.min(AIC)]
fit_best  <- list(weibull = fit_weib, exponential = fit_exp,
                  lognormal = fit_logn, loglogistic = fit_logl)[[best_name]]
ncatn("Best AFT model by AIC:", best_name)
summary(fit_best)

# Log-linear coefficients and 95% CI
gamma_hat <- coef(fit_best)
gamma_ci  <- confint(fit_best)
log_linear <- cbind(estimate = gamma_hat, gamma_ci)
ncatn("Log-linear coefficients with 95% CI:")
print(round(log_linear, 4))

# AFT representation: theta = -gamma
theta_hat <- -gamma_hat
# flip CI bounds as well to convert to AFT
theta_ci  <- -gamma_ci[, 2:1]
colnames(theta_ci) <- c("2.5 %", "97.5 %")
aft <- cbind(estimate = theta_hat, theta_ci, AF = exp(theta_hat),
             AF_low = exp(theta_ci[,1]), AF_up = exp(theta_ci[,2]))
ncatn("AFT representation (theta = -gamma) and acceleration factors:")
print(round(aft, 4))