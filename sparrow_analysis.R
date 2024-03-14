### Winter Project - Are polygamous females fitter? ###

rm(list=ls())     # Clean up working space

# Set working directory
setwd("~/Desktop/Masters/Winter Project/Data")
list.files()

# Import data 
female <- read.csv("sparrow_female.csv", head = T, sep = ";", fill = T)
head(female)

female <- na.omit(female)

# Import libraries
install.packages("lattice")
install.packages("MCMCglmm")
install.packages("cowplot")
install.packages("lsmeans")
install.packages("Rmisc")

library(tidyr)
library(dplyr)
library(ggplot2)
library(MCMCglmm)
library(coda)
library(scales)
library(cowplot)
library(lsmeans)
library(Rmisc)
library(stringr)

### PART 1 - Summarise data ###

## Check distribution of data

# Lifespan
hist(female$Lifespan)
summary(female$Lifespan)

# Number of offspring 
hist(female$Offspring)
summary(female$Offspring)

# Number of EP offspring 
hist(female$EP_Offspring)
summary(female$EP_Offspring)

# Number of recruits
hist(female$Recruits, breaks = 15)
summary(female$Recruits)

## EP vs WP

# Proportions
table(female$MatingStrategy)

# % of EP offspring 
proportion_EP <- sum(female$EP_Offspring) / sum(female$Offspring)
proportion_EP

# Summary statistics of Offspring for each category in MatingStrategy
offspring_summary_stats <- tapply(female$Offspring, female$MatingStrategy, summary)

offspring_summary_stats <- aggregate(Offspring ~ MatingStrategy, data = female,
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))

  # Calculate standard error (assuming sample size is large enough for approximation)
offspring_summary_stats$se <- offspring_summary_stats$Offspring[, "sd"] / 
                              sqrt(table(female$MatingStrategy))
print(offspring_summary_stats)

## Lifespan 

# Summary statistics of lifespan for each category in MatingStrategy
lifespan_summary_stats <- tapply(female$Lifespan, female$MatingStrategy, summary)

lifespan_summary_stats <- aggregate(Lifespan ~ MatingStrategy, data = female,
                                    FUN = function(x) c(mean = mean(x), sd = sd(x)))

  # Calculate standard error (assuming sample size is large enough for approximation)
lifespan_summary_stats$se <- lifespan_summary_stats$Lifespan[, "sd"] / 
  sqrt(table(female$MatingStrategy))
print(lifespan_summary_stats)

# Testing significance 

  # Selecting lifespan data for each category
monogamous_lifespan <- female[female$MatingStrategy == 0, "Lifespan"]
polygamous_lifespan <- female[female$MatingStrategy == 1, "Lifespan"]

  # Performing Mann-Whitney U test
result <- wilcox.test(monogamous_lifespan, polygamous_lifespan)
print(result)

## Within-pair Offspring

# Add column to dataset to show number of WP offspring 
female$WP_Offspring <- female$Offspring - female$EP_Offspring
female

histogram(female$WP_Offspring)

# Summary statistics of WP offspring for each category in MatingStrategy
WP_offspring_summary_stats <- tapply(female$WP_Offspring, female$MatingStrategy, summary)

WP_offspring_summary_stats <- aggregate(WP_Offspring ~ MatingStrategy, data = female,
                                        FUN = function(x) c(mean = mean(x), sd = sd(x)))

  # Calculate standard error (assuming sample size is large enough for approximation)
WP_offspring_summary_stats$se <- WP_offspring_summary_stats$WP_Offspring[, "sd"] / 
  sqrt(table(female$MatingStrategy))
print(WP_offspring_summary_stats)

## Recruits 

# Summary statistics of recruits for each category in MatingStrategy
recruits_summary_stats <- tapply(female$Recruits, female$MatingStrategy, summary)

recruits_summary_stats <- aggregate(Recruits ~ MatingStrategy, data = female,
                                    FUN = function(x) c(mean = mean(x), sd = sd(x)))

  # Calculate standard error (assuming sample size is large enough for approximation)
recruits_summary_stats$se <- recruits_summary_stats$Recruits[, "sd"] / 
  sqrt(table(female$MatingStrategy))
print(recruits_summary_stats)

# Testing significance 

  # Selecting lifespan data for each category
monogamous_recruits <- female[female$MatingStrategy == 0, "Recruits"]
polygamous_recruits <- female[female$MatingStrategy == 1, "Recruits"]

  # Performing Mann-Whitney U test
result1 <- wilcox.test(monogamous_recruits, polygamous_recruits)
print(result1)

# Percentage of females that recruited
count(monogamous_recruits > 0) 

count(polygamous_recruits > 0)

### PART 2 - Showing proportion of monogamous and polygamous females ###

## Figure 1 - Mating Strategy of females across lifespan 

mate <- as.factor(female$MatingStrategy)

ggplot(female, aes(x = Lifespan, fill = mate)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("darkorange3", "cadetblue"), 
                    labels = c("Monogamous", "Polygamous")) +
  labs(x = "Lifespan", y = "Number of Females", fill = "Mating Strategy \n") +
  scale_x_continuous(breaks = seq(min(female$Lifespan), max(female$Lifespan), by = 1)) +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8, 0.9),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        axis.title = element_text(face = "bold", size = 17),
        axis.text.x = element_text(vjust = 6, size = 15),
        axis.text.y = element_text(hjust = 1.5, size = 15))

## GLM to see the probability of being polygamous

# Create a binary variable indicating if a female is polygamous
female$polygamous <- female$MatingStrategy == 1

# Conduct a logistic regression on the new binary variable
lifespan.glm <- glm(formula = polygamous ~ Lifespan, data = female, family = binomial)
summary.glm(lifespan.glm)$coefficients

  # Transform the estimate of lifespan by applying reverse logit 
prob <- (exp(0.6749847))/(1 + exp(0.6749847))
prob   ## For every 1 year, polygamy increases by 66.3% 


# Add logistic fitted values back to dataframe as new column
female$pred.polygamous <- lifespan.glm$fitted.values
head(female)

# Predict the 'probability' that a bird will be polygamous
logit.predictions <- predict(object = lifespan.glm)

# Apply inverse logit to transform to probabilities
prob.predictions <- 1 / (1 + exp(-logit.predictions))
prob.predictions

plot(female$Lifespan, prob.predictions)

# Plot Figure 2 - Probability a female is polygamous across lifespans
ggplot(female, aes(x = Lifespan, y = prob.predictions)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), level = 0.95, color = "cadetblue", fill = "lightblue") +
  labs(x = "Lifespan", y = "Probability of Polygamy") +
  scale_x_continuous(breaks = seq(min(female$Lifespan), max(female$Lifespan), by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8, 0.9),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        axis.title = element_text(face = "bold", size = 17),
        axis.title.y = element_text(vjust = 1, margin = margin(r = 10, l = 5)),
        axis.text.x = element_text(vjust = 2, size = 15),
        axis.text.y = element_text(hjust = 1.5, size = 15))

### PART 3 - Offspring ~ Mating Strategy Model ###

## Model
offspring_model <- MCMCglmm(Offspring ~ MatingStrategy + Lifespan,
                             random = ~ Cohort, 
                             data = female, 
                             family = "ztpoisson",
                             nitt = 500000, 
                             thin = 15, 
                             burnin = 5000,
                             verbose = FALSE)
summary(offspring_model)

# Traceplots
plot(offspring_model) 

# Checking convergence (aim to be within -2 and 2)
geweke.diag(offspring_model$Sol)
geweke.plot(offspring_model$Sol)

# ACF (aim to be < 0.1)
acfplot(offspring_model$Sol, lag.max = 5)

## Post hoc analysis 
# Compute estimated marginal means
offspring_means <- lsmeans(offspring_model, specs = "MatingStrategy", data = female)

# Perform pairwise comparisons
offspring_pairwise_comparisons <- pairs(offspring_means)

# Print the pairwise comparisons
print(offspring_pairwise_comparisons)

## Plot figure 4a - Lifetime number of offspring according to female mating strategy
female$MatingStrategy <- factor(female$MatingStrategy)

offspring_plot <- ggplot(female, aes(x = MatingStrategy, y = Offspring, color = MatingStrategy)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.5), size = 9, shape = 15) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.05, position = position_dodge(0.5)) +
  labs(x = "Female Mating Strategy", y = "Lifetime Number of Offspring") + 
  scale_x_discrete(labels = c("Monogamous", "Polygamous")) +
  scale_y_continuous(breaks = seq(0, 50, by = 2)) +
  scale_color_manual(values = c("0" = "darkorange3", "1" = "cadetblue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 17),
        axis.text.x = element_text(vjust = 0.5, margin = margin(b = 10), size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.2, margin = margin(b = 10), size = 17),
        axis.title.y = element_text(vjust = 2, margin = margin(r = 15, l = 5), size = 17))
offspring_plot

plot_grid(offspring_plot, labels = c("a)"), nrow = 1)

## Plot predicted - Supplementary figure 1. Predicted Lifetime number of offspring according to female mating strategy. 
offspring_predicted <- predict.MCMCglmm(offspring_model)
print(offspring_predicted)

female$MatingStrategy <- factor(female$MatingStrategy)

offspring_predicted_plot <- ggplot(female, aes(x = MatingStrategy, y = offspring_predicted, color = MatingStrategy)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.5), size = 7, shape = 15) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", width = 0.05, position = position_dodge(0.5)) +
  labs(x = "Female Mating Strategy", y = "Lifetime Number of Offspring") + 
  scale_x_discrete(labels = c("Monogamous", "Polygamous")) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  scale_color_manual(values = c("0" = "darkorange3", "1" = "cadetblue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 17),
        axis.text.x = element_text(vjust = 0.5, margin = margin(b = 10), size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.2, margin = margin(b = 10), size = 17),
        axis.title.y = element_text(vjust = 2, margin = margin(r = 15, l = 5), size = 17))
offspring_predicted_plot 

### PART 4 - Validating the results - Offspring without EP ###

## Model
WP_offspring_model <- MCMCglmm(WP_Offspring ~ MatingStrategy + Lifespan,
                            random = ~ Cohort, 
                            data = female, 
                            family = "poisson",
                            nitt = 350000, 
                            thin = 20, 
                            burnin = 3500,
                            verbose = FALSE)
summary(WP_offspring_model)

# Traceplots
plot(WP_offspring_model) 

# Checking convergence (aim to be within -2 and 2)
geweke.diag(WP_offspring_model$Sol)
geweke.plot(WP_offspring_model$Sol)

# ACF (aim to be < 0.1)
acfplot(WP_offspring_model$Sol, lag.max = 5)

## Post hoc analysis 
# Compute estimated marginal means
WP_offspring_means <- lsmeans(WP_offspring_model, specs = "MatingStrategy", data = female)

# Perform pairwise comparisons
WP_offspring_pairwise_comparisons <- pairs(WP_offspring_means)

# Print the pairwise comparisons
print(WP_offspring_pairwise_comparisons)

## Plot figure 4b - Lifetime number of offspring according to female mating strategy
WP_offspring_plot <- ggplot(female, aes(x = MatingStrategy, y = WP_Offspring, color = MatingStrategy)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.5), size = 9, shape = 15) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.05, position = position_dodge(0.5)) +
  labs(x = "Female Mating Strategy", y = "Lifetime Number of Within-pair Offspring") + 
  scale_x_discrete(labels = c("Monogamous", "Polygamous")) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  scale_color_manual(values = c("0" = "darkorange3", "1" = "cadetblue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(vjust = 0.5, margin = margin(b = 10), size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.2, margin = margin(b = 10), size = 17),
        axis.title.y = element_text(vjust = 2, margin = margin(r = 15, l = 5), size = 17))
WP_offspring_plot

# Combine with offspring plot to export
plot_grid(offspring_plot, WP_offspring_plot, labels = c("a)", "b)"), nrow = 2)
plot_grid(WP_offspring_plot, labels = c("b)"), nrow = 1)

## Plot predicted - Supplementary figure 5. Predicted Lifetime number of within-pair offspring according to female mating strategy. 
WP_offspring_predicted <- predict.MCMCglmm(WP_offspring_model)
print(WP_offspring_predicted)

WP_offspring_predicted_plot <- ggplot(female, aes(x = MatingStrategy, y = WP_offspring_predicted, color = MatingStrategy)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.5), size = 9, shape = 15) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", width = 0.05, position = position_dodge(0.5)) +
  labs(x = "Female Mating Strategy", y = "Lifetime Number of Within-pair Offspring") + 
  scale_x_discrete(labels = c("Monogamous", "Polygamous")) +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  scale_color_manual(values = c("0" = "darkorange3", "1" = "cadetblue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(vjust = 0.5, margin = margin(b = 10), size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.2, margin = margin(b = 10), size = 17),
        axis.title.y = element_text(vjust = 2, margin = margin(r = 15, l = 5), size = 17))
WP_offspring_predicted_plot

### PART 5 - Recruits ~ Mating Strategy Model ###

## Model 
recruits_model <- MCMCglmm(Recruits ~ MatingStrategy + Lifespan, 
                           random = ~ Cohort, 
                           data = female, 
                           family = "poisson",
                           nitt = 400000,
                           thin = 35, 
                           burnin = 4000,
                           verbose = FALSE)
summary(recruits_model)

# Trace plots
plot(recruits_model)

# Checking convergence (aim to be within -2 and 2)
geweke.diag(recruits_model$Sol)
geweke.plot(recruits_model$Sol)

# ACF (aim to be < 0.1)
acfplot(recruits_model$Sol, lag.max = 5)

## Post hoc analysis 
# Compute estimated marginal means
recruits_means <- lsmeans(recruits_model, specs = "MatingStrategy", data = female)

# Perform pairwise comparisons
recruits_pairwise_comparisons <- pairs(recruits_means)

# Print the pairwise comparisons
print(recruits_pairwise_comparisons)

## Plot figure 5 - Lifetime number of recruits according to female mating strategy
recruits_plot <- ggplot(female, aes(x = MatingStrategy, y = Recruits, color = MatingStrategy)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.5), size = 9, shape = 15) +
  stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.05, position = position_dodge(0.5)) +
  labs(x = "Female Mating Strategy", y = "Lifetime Number of Recruits") + 
  scale_x_discrete(labels = c("Monogamous", "Polygamous")) +
  scale_color_manual(values = c("0" = "darkorange3", "1" = "cadetblue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 17),
        axis.text.x = element_text(vjust = 0.5, margin = margin(b = 10), size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.2, margin = margin(b = 7), size = 17),
        axis.title.y = element_text(vjust = 2, margin = margin(r = 15, l = 5), size = 17))
recruits_plot

## Plot predicted - Supplementary figure 9. Predicted lifetime number of recruits according to female mating strategy
recruits_predicted <- predict.MCMCglmm(recruits_model)
print(recruits_predicted)

recruits_predicted_plot <- ggplot(female, aes(x = MatingStrategy, y = recruits_predicted, color = MatingStrategy)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.5), size = 9, shape = 15) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", width = 0.05, position = position_dodge(0.5)) +
  labs(x = "Female Mating Strategy", y = "Lifetime Number of Recruits") + 
  scale_x_discrete(labels = c("Monogamous", "Polygamous")) +
  scale_color_manual(values = c("0" = "darkorange3", "1" = "cadetblue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 17),
        axis.text.x = element_text(vjust = 0.5, margin = margin(b = 10), size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.2, margin = margin(b = 7), size = 17),
        axis.title.y = element_text(vjust = 2, margin = margin(r = 15, l = 5), size = 17))
recruits_predicted_plot

### PART 6 - Lifetime Strategy Choice ###

# Import new data
mating <- read.csv("sparrow_mate.csv", head = T, sep = ";", fill = T)
head(mating)

# Remove repeats
mating <- unique(mating)
head(mating)

# Filter for only birds were age is known
mating <- filter(mating, ! is.na(Age))

# Combine the ID and Age columns into one 
mating$ID.age = paste0(mating$SocialMotherID, '_', mating$Age)

# Select the max valuee of mating strategy to ensure a polygamous female only has mating startegy = 1
mating2 <- tapply(mating$MatingStrategy, mating$ID.age, max)
mating2 <- data.frame(strategy = mating2, id.age = names(mating2))

# Split columns 
mating2$id <- strsplit(mating2$id.age, '_', simplify = TRUE)
mating2$ID <- mating2$id[,1]
mating2$Age <- mating2$id[,2]

# Calculate the transitions of mating startegy between ages 
transition <- c()
for (i in 2:nrow(mating2)){
  if (mating2$ID[i] == mating2$ID[i-1]){
    if (mating2$strategy[i] == 0 & mating2$strategy[i-1] == 0) {transition <- c(transition, 'MM')}  # Remains monogamous
    if (mating2$strategy[i] == 0 & mating2$strategy[i-1] == 1) {transition <- c(transition, 'MP')}  # Swaps from monogamous to polygamous
    if (mating2$strategy[i] == 1 & mating2$strategy[i-1] == 0) {transition <- c(transition, 'PM')}  # Swaps from polygamous to monogamous
    if (mating2$strategy[i] == 1 & mating2$strategy[i-1] == 1) {transition <- c(transition, 'PP')}  # Remains polygamous
  }
}
table(transition)

### END ###
