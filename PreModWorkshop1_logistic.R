#####################################
# Predictive modeling in R I
# Logistic regression

library(effects) # plotting effects
library(tidyverse)
library(caret)
library(gridExtra) # arrange ggplots
library(ISLR2) # dataset in Introduction to Statistical Learning, Second Edition
library(rsample) # split dataset

data("Default") # load the dataset in ISLR2
?Default # credit card default data

# split the data randomly into training and test
set.seed(1234) # ensure we get the same split
split = initial_split(Default, prop = .7) # 70% training, 30% testing
train = training(split)
test = testing(split)

# fit logistic regression models
mod_log <- glm(default ~ student + balance + income, family = "binomial",
               data = train)
summary(mod_log)

# interaction model
mod_inter <- glm(default ~ student*balance*income, family = "binomial",
                 data = train)
summary(mod_inter)

# do backward selection with model that has all pairwise combination 
mod_step <- step(mod_inter, direction = "backward")

# backward selection using AIC as the metric
# AIC: scores models in terms of potential predictive performance
# small AICs are better than bigger ones

summary(mod_step)
# model selected by backward stepwise selection
# has variables student, balance, and income 
# interaction: student and income

###
# Regularized logistic regression

reg_logistic <- train(
  default ~ student*balance*income,
  train,
  preProc = c("center", "scale", "zv"),
  method = "glmnet",
  family = "binomial", # for logistic model, add this parameter; for OLS, remove it
  trControl = trainControl(
    method = "cv",
    number = 10
  )
  tuneLength = 10
)

# find coefficients
coef(reg_logistic$finalModel, reg_logistic$finalModel$lambdaOpt)

# Four models
# mod_log: logistic model with additive effects
# mod_inter: logisitc model with interactions
# mod_step: backward selection outupt with logistic model
# reg_logistic:regularized logistic model

# compare the predictive perofrmance of the models 
# in the test set

test = test %>% mutate(pred_log = predict(mod_log, newdata = test, type = "response"),
                       pred_inter = predict(mod_inter, newdata = test, type = "response"),
                       pred_step = predict(mod_step, newdata = test, type= "response"),
                       pred_reg = predict(reg_logistic, newdata = test, type = "prob")$Yes)

# reg_logistic is fitted using the 'train' function and is different
# predict(reg_logistic, newdata = test, type = "prob") %>% select(Yes)

head(test)

# find boxplot of prob predictions versus observed outcomes
ggplot(test) + 
  aes(x = default, y = pred_log) + 
  geom_boxplot()

# the predicted probabilities of default 
# are higher for those who defaulted 
# than for those who didn't 

ggplot(test) + 
  aes(x = default, y = pred_inter) + 
  geom_boxplot()

ggplot(test) + 
  aes(x = default, y = pred_step) + 
  geom_boxplot()

ggplot(test) + 
  aes(x = default, y = pred_reg) + 
  geom_boxplot()  

# find grouped by means and sd of prob predictions versus observed outcomes 

# additive model 
test %>% group_by(default) %>% summarize(avg_pred = mean(pred_add),
                                         sd_pred = sd(pred_add))

# avg predicted prob. default for those who do not default: .0221
# avg predicted prob. default for those who default: .330

test %>% group_by(default) %>% summarize(avg_pred = mean(pred_log),
                                         sd_pred = sd(pred_log))

test %>% group_by(default) %>% summarize(avg_pred = mean(pred_inter),
                                         sd_pred = sd(pred_inter))

test %>% group_by(default) %>% summarize(avg_pred = mean(pred_step),
                                         sd_pred = sd(pred_step))

test %>% group_by(default) %>% summarize(avg_pred = mean(pred_reg),
                                         sd_pred = sd(pred_reg))

# find confusion matrix for threshold .5
# compare observed outcomes against predicted outcomes 

# so far we got probabilities of default
# we don't have predicted outcomes 
# outcomes: "Yes" or "No"
 
# in the absence of any prior information, .5 is a reasonable threshold 

# find these predicted outcomes given the 
# predicted probabilities of default
out_log = factor(ifelse(test$pred_log > 0.5, "Yes", "No"))

out_inter = factor(ifelse(test$pred_inter > 0.5, "Yes", "No"))

out_step = factor(ifelse(test$pred_step > 0.5, "Yes", "No"))

out_reg = factor(ifelse(test$pred_reg > 0.5, "Yes", "No"))

# I can compare my predictions against the outcomes 
# ?confusionMatrix()

confusionMatrix(out_log, test$default, positive = "Yes")

confusionMatrix(out_inter, test$default, positive = "Yes")

confusionMatrix(out_reg, test$default, positive = "Yes")

# conclusion: these models have similar performance

### Tips: 
# in backward selection, by default it uses AIC for selecting models
# if you use BIC, can do
# k = log(n); n is the sample size

###################################################
# Practice: using the Boston housing data in ISLR2
# A data set containing housing values in 506 suburbs of Boston.

?Boston
data(Boston)
glimpse(Boston)

# chas should be converted into factor (encoded as categorical variable)
Boston$chas <- as.factor(Boston$chas)

# Outcome variable 
Boston <- Boston %>% 
  mutate(highcrim = as.factor(ifelse(crim > median(crim), "yes", "no")))

# Exclude "crim"
Boston <- Boston %>% select(-crim)

#######################################################
# Task: predict if a suburb has above median crime rate

# set seed 
set.seed(1234)
split <- initial_split(Boston, prop = .7)
train <- training(split)
test <- testing(split)

# fit a logistic model 
mod_log <- glm(highcrim ~., data = train, family = 'binomial')
summary(mod_log)

# regularized logistic regression
reg_logistic <- train(
  highcrim ~.,
  train, 
  preProc = c("center", "scale", "zv"),
  method = "glmnet",
  family = "binomial",
  trControl = trainControl(
    method = "cv",
    number = 10
  )
)

reg_logistic$bestTune

# find variable importance
vip(reg_logistic)

# predict
test = test %>% 
  mutate(
    pred_reg = predict(reg_logistic, newdata = test, type = "prob")$yes,
    pred_log = predict(mod_log, newdata = test, type = "response"))

# find boxplot of prob predictions versus observed outcomes 
ggplot(test) + 
  aes(x = highcrim, y = pred_reg) + 
  geom_boxplot()

ggplot(test) + 
  aes(x = highcrim, y = pred_log) + 
  geom_boxplot()

# convert predicted probabilities to predicted outcomes 
out_reg <- factor(ifelse(test$pred_reg > 0.5, "yes", "no"))
out_log <- factor(ifelse(test$pred_log > 0.5, "yes", "no"))

# compare predicted outcomes ('yes' or 'no')
# against observed outcomes via a confusion matrix
confusionMatrix(out_reg, test$highcrim, positive = "yes")
confusionMatrix(out_log, test$highcrim, positive = "yes")

# regularized rgression doesn't do well: 
# all predictors are independent 
# when we have moderate number of predictors and most of them are useful 






