# ============================================
# Project: Two-Way ANOVA Experiment
# Dataset: Flower Growth Experiment
# Author: Kaitlyn Hill
#
# Description:
# This analysis investigates the effects of light intensity and
# timing on flower production using a two-way ANOVA model.
# Interaction effects between factors are evaluated and compared
# with additive models.
#
# Methods:
# - Two-way ANOVA
# - Interaction effects
# - Additive vs interaction model comparison
# - ESS F-tests
# ============================================

# ==== Load Libraries ====
library(ggplot2)


# ==== Load Dataset ====
path <- read.csv("case0901-1.csv")
head(path)


# ==== Convert Variables to Factors ====
path$Time <- factor(path$Time)
path$Intensity <- factor(path$Intensity)


# ==== Exploratory Visualization ====

# Boxplot: Flowers by Timing
ggplot(path, aes(x = Time, y = Flowers)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Number of Flowers by Timing", x = "Timing", y = "Number of Flowers")

# Boxplot: Flowers by Light Intensity
ggplot(path, aes(x = Intensity, y = Flowers)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Number of Flowers by Light Intensity", x = "Light Intensity", y = "Number of Flowers")


# ==== Two-Way ANOVA with Interaction ====
non_add <- aov(Flowers ~ Time * Intensity, data = path)
summary(non_add)


# ==== Additive ANOVA Model ====
additive <- aov(Flowers ~ Time + Intensity, data = path)
summary(additive)


# ==== Model Comparison: Extra Sum-of-Squares F-Test ====
anova(additive, non_add)


# ==== Parallel Lines Regression Model ====

# Convert Intensity to numeric
path$IntensityNum <- as.numeric(as.character(path$Intensity))

# Fit parallel lines model
parallel <- lm(Flowers ~ Time * IntensityNum, data = path)

# Output summary and ANOVA
summary(parallel)
anova(parallel)





