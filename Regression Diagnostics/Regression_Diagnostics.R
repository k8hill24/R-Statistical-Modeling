# ============================================
# Project: Regression Diagnostics
# Dataset: Mushroom and Soil Data
# Author: Kaitlyn Hill
#
# Description:
# This analysis fits a linear regression model relating mushroom
# abundance to soil measurements. Diagnostic tools are used to
# evaluate model assumptions and detect influential observations.
#
# Methods:
# - Linear regression
# - Residual diagnostics
# - Cook's distance
# - Standardized residual analysis
# - Log transformation model
# ============================================

# ==== Load Libraries ====
library(ggplot2)
library(MASS)


# ==== Load Dataset ====
path <- read.csv("ex1111.csv")
head(path)


# ==== Exploratory Visualization ====
ggplot(data = path, aes(x = Mushroom, y = Soil)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Mushroom vs. Soil")


# ==== Fit Linear Regression Model ====
model <- lm(data = path, Mushroom ~ Soil)
summary(model)


# ==== Residual Diagnostics ====
plot(model$fitted.values, model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "blue")


# ==== Cook's Distance (Influential Points) ====
plot(cooks.distance(model1), type = "h",
     ylab = "Cook's Distance", main = "Cook's Distance Plot")
abline(h = 4/length(path$Mushroom), col = "red", lty = 2)


# ==== Standardized Residual Analysis ====
std_res <- rstandard(model1)
plot(std_res, type = "h", main = "Standardized Residuals")
abline(h = c(-2, 2), col = "red", lty = 2)


# ==== Remove Influential Observation (Observation 17) ====
path_no17 <- path[-17, ]
model2 <- lm(Mushroom ~ Soil, data = path_no17)
summary(model2)

ggplot(data = path_no17, aes(x = Mushroom, y = Soil)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Mushroom vs. Soil")

plot(model2$fitted.values, model2$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot (without obs 17)")
abline(h = 0, col = "blue")

model2 <- lm(data = path_no17, Mushroom ~ Soil)
summary(model2)


# ==== Log Transformation Model ====
log_model <- lm(log(Mushroom) ~ log(Soil), data = path)
summary(log_model)


# ==== Diagnostics for Log-Log Model ====
plot(log_model$fitted.values, log_model$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot (log-log model)")
abline(h = 0, col = "blue")
