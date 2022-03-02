######################################
# Predictive modeling in R
# build a model to do classification 

library(here)
library(tidyverse) 
library(effects) # plotting effects
library(titanic) # titanic data
library(caret) # model tuning 
library(gridExtra) # arrange ggplots
library(ggplot2)
library(GGally)

# sets the starting number used to generate a sequence of random numbers
set.seed(2021) 

data("titanic_train")
data("titanic_test")

str(titanic_train)
titanic_train <- titanic_train %>% select(Survived, Pclass, Age, Sex) %>% 
  mutate(Survived = as.factor(Survived), Pclass = as.factor(Pclass)) %>% na.omit()

titanic_test <- titanic_test %>% select(Pclass, Age, Sex) %>% 
  mutate(Pclass = as.factor(Pclass)) %>% na.omit()

# descriptive analysis
ggpairs(titanic_train, axisLabels = "internal")

# fit logistic regression model 
logistic <- glm(Survived ~ Pclass + Age + Sex, family = "binomial", data = titanic_train)
summary(logistic)

#######################################
# interaction terms (moderation effect)
#######################################
mod_inte <- glm(Survived ~ Pclass*Age*Sex, family = "binomial", data = titanic_train)
summary(mod_inte)

mod_inte <- glm(Survived ~ Pclass + Age + Sex + Age*Pclass, family = "binomial", data = titanic_train)
summary(mod_inte)

# model with nothing
nullmod <- glm(Survived ~ 1, family = "binomial", data = titanic_train)

# model with all possible interaction term
fullmod <- glm(Survived ~ Pclass*Age*Sex, family = "binomial", data = titanic_train)
summary(fullmod)

######################
# model selection 
######################

# backward Stepwise algorithm AIC
bwd <- step(fullmod, direction = "backward")
# show the model backward chose
summary(bwd)

# forward stepwise algoirthm
fwd <- step(nullmod, scope = list(upper = fullmod), direction = "forward")
# show the model forward chose
summary(fwd)

# stepwise regression 
both <- step(nullmod, scope = list(upper = fullmod), direction = "both")
summary(both)

# visualize the effects of the variables 
plot(effects::allEffects(both))

# fitting regularized logistic regression
library(glmnet)
reg_logistic <- caret::train(
  Survived ~ Pclass*Age*Sex,
  data = titanic_train, 
  preProc = c("center", "scale", "zv"),
  method = "glmnet",
  family = "binomial",
  trControl = trainControl(
    method = "cv", number = 10
  ),
  tuneLength = 7
)

# use ?preProcess see pre-processing options of training data
# use ?trainControl see training options
# tuneLength is for regularization parameter alpha
summary(reg_logistic)
reg_logistic$bestTune

# plotting variable importance
vip::vip(reg_logistic)

# find average bootstrap accuracy
confusionMatrix(reg_logistic)
# accuracy: 0.8039
# accuracy estimates how often I classify right
# % of times that my model predicts correctly
# I estimate that my regularized logistic
# regression predicts correctly ~80%

# fit equivalent logistic regression
logistic <- train(
  Survived ~ Sex + Pclass + Age + Sex:Pclass + Pclass:Age + 
    Sex:Age,
  titanic_train,
  preProc = c("center", "scale", "zv"),
  method = "glm",
  family = "binomial"
)

# plot vips & find average bootstrapped accuracy
vip::vip(logistic)
confusionMatrix(logistic)

# estimated accuracy: 0.7981
# slightly smaller than regularized regression

# append predictions 
titanic_test <- titanic_test %>% mutate(
  preds_log = predict(logistic, newdata = titanic_test, type = "prob"),
  preds_reg = predict(reg_logistic, newdata = titanic_test, type = "prob"),
)

head(titanic_test)

ggplot(titanic_test) + aes(x = preds_log[,1], y = preds_reg[,1]) + 
  geom_point() + geom_smooth()

# visualize parameter tuning for regularized regression 
ggplot(reg_logistic)
# predcit test set with reg_regression
pred <- predict(reg_logistic, titanic_train)
# assess performance on test set 
confusionMatrix(pred, titanic_train$Survived)





























