#####################################
# Predictive modeling in R II
# Build a model to do regression 
# Predicting continuous output 

library(GGally) # some useful graphics
library(caret) # automatic model tunning
library(tidyverse) # plots, data-wrangling functions
library(glmnet) # regularized regression
library(vip) # variance importance plots
library(rsample) # splitting data

#############################
# Italian restaurants in NYC #
##############################
# read in https://github.com/YuxiaoLuo/PredictiveModeling/blob/main/data/nycrest.csv
# nyc dataset: Italian restaurants in nyc

nycrest <- read_csv("https://raw.githubusercontent.com/YuxiaoLuo/PredictiveModeling/main/data/nycrest.csv") %>%
  subset(select = Restaurant:East)

# split the data randomly into training and test
set.seed(123) # this ensures that we get the same random split 

# use rsample for splitting dataset
split <- initial_split(nycrest, prop = 0.7) # 70% training, 30% testing 

train <- training(split)
test <- testing(split)

# For panel data, do stratified sampling, which respect the structure of panel 

# use ggpairs to explore the data 
train %>% select(Price, Food, Service, Decor, East) %>% ggpairs

# use ggcorr to gind correlations, heatmap using color-coding to quantify 
train %>% select(Price, Food, Service, Decor) %>% ggcorr(label = T)

# Our goal is going to be 
# predict the price of a meal, given the quality of the food 
# service, decor, and the variable that tells me whether the restaurant is on the East or West

# run regularized regression, called elastic net
# it combines the lasso (absolute value penalty) with ridge (squared error penalty)
names(getModelInfo("glmnet"))

# use the train() function caret package
regularized_reg <- caret::train(
  Price ~ Food + Decor + Service + East, 
  train,  # training set
  preProc = c("center", "scale"), # preprocess predictors 
  method = "glmnet",
  metric = "RMSE", 
  trControl = trainControl(
    method = "cv",
    number = 10 # by default, it's 10-fold
  )
)

# center: subtracts the mean of the predictor's data
# scale: divides by the standard deviation 
# you have other options: zv, corr, conditionalX
# ?preProcess will give you more details

regularized_reg

# variable importance plot
vip(regularized_reg)

# run vanilla linear regression 
mod_lm <- lm(Price ~ Food + Decor + Service + East, data = train)

# predict prices of restaurants in test set
# given their Decor, Food, EastWest, and Service
test <- test %>% mutate(preds_reg = predict(regularized_reg, newdata = test), 
                        preds_lm = predict(mod_lm, newdata = test))
# find mean squared error
test %>% summarize(MSE_reg = mean((preds_reg - Price)^2), 
                   MSE_lm = mean((preds_lm - Price)^2))

# plot predictions against actual prices
library(plotly)
plot <- ggplot(test) + 
  aes( x= preds_reg, y = Price) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplotly(plot)
dev.off()

############
# mpg data #
############

data(mpg)
?mpg

# goal is to predict hwy given other variables
# excluding manufacturer and model from
# regression
mpg <- mpg %>% select(-model, -manufacturer)

set.seed(123)

library(rsample) # allows to split data easily 
split <- initial_split(mpg, prop = .7)
train <- training(split)
test <- testing(split)

# explore data with ggpairs and ggcorr
library(GGally) #contains useful plots
train %>% ggpairs
train %>% select(hwy, cty, displ, cyl) %>% ggcorr(label = T)

# there are categorical var, check consistency 
train %>% count(fl)
test %>% count(fl)
# some categories in training set are not in testing set --> remove fl

test <-  test %>% filter(fl != "c")
test %>% count(fl)

train %>% count(trans)
test %>% count(trans)
# not enough observations for the categories
# auto(13) not in training set
# auto(s5) not in test set

# run regularized regression
library(glmnet)
library(caret)

regularized_reg <- train(
  hwy ~ displ + year + cyl + drv + cty + class,
  train,
  preProc = c("center", "scale", "zv"),
  method = "glmnet",
  metric = "RMSE",
  trControl = trainControl(
    method = "cv",
    number = 20
  )
)

# zv gets rid of variables that have zero variance 
# which makes sense since if a variable takes on the same value 
# all the time is that really variable? 

# variable importance plot
library(vip)
vip(regularized_reg)

# run vanilla linear regression
lm_mpg <- lm(hwy ~ displ + year + cyl + drv + cty + class, data = train)

# predict hwy in test set
test <- test %>% mutate(preds_reg = predict(regularized_reg, newdata = test),
                        preds_lm = predict(lm_mpg, newdata = test))

# find MSE
test %>% summarize(MSE_reg = mean((hwy - preds_reg)^2),
                   MSE_lm = mean((hwy - preds_lm)^2))

# plot predictions against actual prices
ggplot(test) + 
  aes(x = preds_lm, y = hwy) + 
  geom_point() + 
  geom_smooth(method = "lm")

dev.off()
