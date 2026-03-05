# ============================================
# Project: Predictive Model Selection
# Dataset: Body Fat Prediction Data
# Author: Kaitlyn Hill
#
# Description:
# This analysis builds predictive models for estimating body fat
# percentage using multiple regression. Several model selection
# techniques are applied and evaluated using predictive accuracy.
#
# Methods:
# - Multiple regression
# - Forward selection
# - Backward elimination
# - AIC, BIC, and Cp model comparison
# - RMSE evaluation using a test dataset
# ============================================

# ==== Load Libraries ====
library(ggplot2)
library(olsrr)
library(leaps)


# ==== Load Dataset ====
path <- read.csv("fat.csv")
head(path)


# ==== Explore Dataset Dimensions ====
dim(path)


# ==== Train-Test Split ====
n <- nrow(path)
remove.ind <- seq(10, n, by=10)
test <- path[remove.ind, ]
train <- path[-remove.ind, ]


# ==== Fit Full Multiple Regression Model ====
full <- lm(siri ~ . - brozek - density, data = train)
summary(full)


# ==== Forward Stepwise Model Selection ====
forwardmod <- ols_step_forward_p(full, penter=0.05, progress=FALSE)
summary(forwardmod$model)


# ==== Backward Elimination Model Selection ====
backmod <- ols_step_backward_p(full, prem=0.05, progress=FALSE)
summary(backmod$model)


# ==== Best Subset Model Selection ====
p <- ncol(train) - 3
all <- regsubsets(siri ~ . - brozek - density, data=train, nvmax=p)

n <- nrow(train)
calc.AIC <- n * log(summary(all)$rss / n) + 2 * (2:(p + 1))  


# ==== Model with Minimum AIC ====
min.AIC.vars <- summary(all)$which[which.min(calc.AIC),]
print(min.AIC.vars)

vars.AIC <- names(min.AIC.vars[min.AIC.vars][-1])  # exclude intercept
min.AIC <- lm(as.formula(paste("siri ~", paste(vars.AIC, collapse=" + "))), data=train)
summary(min.AIC)


# ==== Model with Minimum BIC ====
min.BIC.vars <- summary(all)$which[which.min(summary(all)$bic),]
print(min.BIC.vars)

vars.BIC <- names(min.BIC.vars[min.BIC.vars][-1])
min.BIC <- lm(as.formula(paste("siri ~", paste(vars.BIC, collapse=" + "))), data=train)
summary(min.BIC)


# ==== Model with Minimum Cp ====
min.cp.vars <- summary(all)$which[which.min(summary(all)$cp),]
print(min.cp.vars)

vars.cp <- names(min.cp.vars[min.cp.vars][-1])
min.cp <- lm(as.formula(paste("siri ~", paste(vars.cp, collapse=" + "))), data=train)
summary(min.cp)


# ==== Model Evaluation Using RMSE ====
truth <- test$siri

# Predict on test set for each model
pred.AIC <- predict(min.AIC, newdata=test)
pred.BIC <- predict(min.BIC, newdata=test)
pred.cp <- predict(min.cp, newdata=test)

# Calculate RMSE
rmse.AIC <- sqrt(mean((truth - pred.AIC)^2))
rmse.BIC <- sqrt(mean((truth - pred.BIC)^2))
rmse.cp  <- sqrt(mean((truth - pred.cp)^2))

rmse.AIC
rmse.BIC
rmse.cp


