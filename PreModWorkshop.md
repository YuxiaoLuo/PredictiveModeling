Build a logistic regression model
---------------------------------

### Titanic dataset description

The `titanic` library in R has 4 different datasets, they are
**titanic\_train**, **titanic\_test**, **titanic\_gender\_model**, and
**titanic\_gender\_class\_model**. They describe the survival status of
individual passengers on the Titanic. The variable description can be
found below:

1.  Pclass: Passenger Class (1 = 1st, 2 = 2nd, 3 = 3rd)

2.  survived: Survival (0 = No; 1 = Yes)

3.  name: Name

4.  sex: Sex

5.  age: Age

6.  sibsp: Number of Siblings/Spouses Aboard

7.  parch: Number of Parents/Children Aboard

8.  ticket: Ticket Number

9.  fare: Passenger Fare (British pound)

10. cabin: Cabin

11. embarked: Port of Embarkation

    (C = Cherbourg; Q = Queenstown; S = Southampton)

12. boat: Lifeboat

13. body: Body Identification Number

14. home.dest: Home/Destination

### Data preprocessing

We use the `titanic` library to load the data.

``` r
library(titanic)
library(tidyverse)
data("titanic_train")
data("titanic_test")
str(titanic_train)
```

    ## 'data.frame':    891 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : chr  "male" "female" "female" "female" ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr  "" "C85" "" "C123" ...
    ##  $ Embarked   : chr  "S" "C" "S" "S" ...

Alternatively, you can input the data by downloading it from my Github
repo and reading it into R with library `readr`.

``` r
library(readr)
getwd()
```

    ## [1] "C:/Users/Yuxiao Luo/Documents/R/Workshop/PredictiveModeling"

``` r
titanic_train <- read_csv("data/titanic_train.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Survived = col_double(),
    ##   Pclass = col_double(),
    ##   Age = col_double(),
    ##   Sex = col_character()
    ## )

``` r
titanic_test <- read_csv("data/titanic_test.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Survived = col_double(),
    ##   Pclass = col_double(),
    ##   Age = col_double(),
    ##   Sex = col_character()
    ## )

Let’s clean the dataset by extracting four variables (`Survived`,
`Pclass`, `Age`,`Sex`) and change the variable type to `factor` for
`Survived` and `Pclass`. Since the data has been pre-processed by other
people and is pretty clean, we don’t do much thing here. Usually, you
will have more work to do for your raw data.

``` r
# clean the data
titanic_train <- titanic_train %>% 
  select(Survived, Pclass, Age, Sex) %>% 
  mutate(Survived = as.factor(Survived), Pclass = as.factor(Pclass)) %>%  na.omit()

titanic_test <- titanic_test %>% 
  select(Pclass, Age, Sex) %>% 
  mutate(Pclass = as.factor(Pclass)) %>% na.omit()
```

Let’s do preliminary descriptive analysis using `ggpairs` function in
`GGally` library. The function will generate a matrix of selected
variables.

![](PreModWorkshop_files/figure-markdown_github/descriptive-1.png)

### Modeling

Then, let’s fit a logistic regression model to it and use `summary`
function to see the model details.

``` r
logistic <- glm(Survived ~ Pclass + Age + Sex, family = "binomial", data = titanic_train)
summary(logistic)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Age + Sex, family = "binomial", 
    ##     data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0249  -0.6779  -0.3781   0.6163   2.5022  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.927549   0.477099   8.232  < 2e-16 ***
    ## Pclass2     -1.385308   0.338906  -4.088 4.36e-05 ***
    ## Pclass3     -2.674247   0.335473  -7.972 1.57e-15 ***
    ## Age         -0.040304   0.009203  -4.380 1.19e-05 ***
    ## Sexmale     -2.525422   0.250179 -10.094  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 449.22  on 495  degrees of freedom
    ## AIC: 459.22
    ## 
    ## Number of Fisher Scoring iterations: 5

The model looks good and all selected variables are significant, we want
to explore and see whether there are better models. Let’s try
interaction terms.

#### Interaction terms

An interaction occurs when an independent variable has a different
effect on the outcome depending on the values of another independent
variable. We want to check `Age` and `Pclass` first because typically
older may earn more money and thus, buy expensive tickets.

``` r
mod_inte <- glm(Survived ~ Pclass + Age + Sex + Age*Pclass, family = "binomial", data = titanic_train)
summary(mod_inte)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Age + Sex + Age * Pclass, family = "binomial", 
    ##     data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9856  -0.6314  -0.4068   0.6508   2.3995  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  4.442113   0.725402   6.124 9.15e-10 ***
    ## Pclass2     -1.587757   0.903010  -1.758  0.07870 .  
    ## Pclass3     -3.588736   0.799733  -4.487 7.21e-06 ***
    ## Age         -0.052415   0.016240  -3.228  0.00125 ** 
    ## Sexmale     -2.553110   0.252870 -10.097  < 2e-16 ***
    ## Pclass2:Age  0.002058   0.024173   0.085  0.93214    
    ## Pclass3:Age  0.030339   0.021830   1.390  0.16459    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 446.82  on 493  degrees of freedom
    ## AIC: 460.82
    ## 
    ## Number of Fisher Scoring iterations: 5

The result doesn’t show any trace. Then, let’s do a full interaction
model and try every possible combination.

``` r
mod_inte <- glm(Survived ~ Pclass*Age*Sex, family = "binomial", data = titanic_train)
summary(mod_inte)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass * Age * Sex, family = "binomial", 
    ##     data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6060  -0.6337  -0.4067   0.4140   3.0623  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)          8.75379    4.99563   1.752   0.0797 .
    ## Pclass2             -5.24863    5.17796  -1.014   0.3108  
    ## Pclass3             -8.60149    5.01532  -1.715   0.0863 .
    ## Age                 -0.10785    0.09848  -1.095   0.2735  
    ## Sexmale             -7.11886    5.05514  -1.408   0.1591  
    ## Pclass2:Age          0.06284    0.10515   0.598   0.5501  
    ## Pclass3:Age          0.09709    0.10012   0.970   0.3322  
    ## Pclass2:Sexmale      4.22680    5.29911   0.798   0.4251  
    ## Pclass3:Sexmale      6.36284    5.10200   1.247   0.2124  
    ## Age:Sexmale          0.05537    0.10021   0.553   0.5806  
    ## Pclass2:Age:Sexmale -0.09573    0.11143  -0.859   0.3903  
    ## Pclass3:Age:Sexmale -0.09197    0.10401  -0.884   0.3766  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 421.83  on 488  degrees of freedom
    ## AIC: 445.83
    ## 
    ## Number of Fisher Scoring iterations: 8

It looks good though. Why don’t we let the algorithm do the model
selection for us? I will use **Stepwise Algorithm** to do that job.
First, I will create two models as the starting point for the algorithm.

-   null model means no variable is in the model.

-   full model means the model contains all variables and possible
    interaction terms.

``` r
# model with nothing
nullmod <- glm(Survived ~ 1, family = "binomial", data = titanic_train)

# model with all possible interaction term
fullmod <- glm(Survived ~ Pclass*Age*Sex, family = "binomial", data = titanic_train)

summary(fullmod)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass * Age * Sex, family = "binomial", 
    ##     data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6060  -0.6337  -0.4067   0.4140   3.0623  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)          8.75379    4.99563   1.752   0.0797 .
    ## Pclass2             -5.24863    5.17796  -1.014   0.3108  
    ## Pclass3             -8.60149    5.01532  -1.715   0.0863 .
    ## Age                 -0.10785    0.09848  -1.095   0.2735  
    ## Sexmale             -7.11886    5.05514  -1.408   0.1591  
    ## Pclass2:Age          0.06284    0.10515   0.598   0.5501  
    ## Pclass3:Age          0.09709    0.10012   0.970   0.3322  
    ## Pclass2:Sexmale      4.22680    5.29911   0.798   0.4251  
    ## Pclass3:Sexmale      6.36284    5.10200   1.247   0.2124  
    ## Age:Sexmale          0.05537    0.10021   0.553   0.5806  
    ## Pclass2:Age:Sexmale -0.09573    0.11143  -0.859   0.3903  
    ## Pclass3:Age:Sexmale -0.09197    0.10401  -0.884   0.3766  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 421.83  on 488  degrees of freedom
    ## AIC: 445.83
    ## 
    ## Number of Fisher Scoring iterations: 8

The are two ways for the **Stepwise Algorithm** to work, one is backward
selection and the other is forward selection. Both will give you the
same result though. Let me try backward selection first.

``` r
# backward Stepwise algorithm AIC
bwd <- step(fullmod, direction = "backward")
```

    ## Start:  AIC=445.83
    ## Survived ~ Pclass * Age * Sex
    ## 
    ##                  Df Deviance    AIC
    ## - Pclass:Age:Sex  2   422.84 442.84
    ## <none>                421.83 445.83
    ## 
    ## Step:  AIC=442.84
    ## Survived ~ Pclass + Age + Sex + Pclass:Age + Pclass:Sex + Age:Sex
    ## 
    ##              Df Deviance    AIC
    ## - Pclass:Age  2   424.72 440.72
    ## - Age:Sex     1   424.63 442.63
    ## <none>            422.84 442.84
    ## - Pclass:Sex  2   436.57 452.57
    ## 
    ## Step:  AIC=440.72
    ## Survived ~ Pclass + Age + Sex + Pclass:Sex + Age:Sex
    ## 
    ##              Df Deviance    AIC
    ## <none>            424.72 440.72
    ## - Age:Sex     1   427.78 441.78
    ## - Pclass:Sex  2   437.13 449.13

``` r
# show the model backward chose
summary(bwd)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Pclass + Age + Sex + Pclass:Sex + Age:Sex, 
    ##     family = "binomial", data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7922  -0.6427  -0.4090   0.4568   2.6879  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      4.92606    1.17396   4.196 2.72e-05 ***
    ## Pclass2         -2.21545    1.11884  -1.980   0.0477 *  
    ## Pclass3         -4.55819    1.06634  -4.275 1.91e-05 ***
    ## Age             -0.02097    0.01605  -1.307   0.1913    
    ## Sexmale         -3.11707    1.30060  -2.397   0.0165 *  
    ## Pclass2:Sexmale  0.34856    1.21728   0.286   0.7746    
    ## Pclass3:Sexmale  2.35787    1.13962   2.069   0.0385 *  
    ## Age:Sexmale     -0.03592    0.02046  -1.756   0.0792 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 424.72  on 492  degrees of freedom
    ## AIC: 440.72
    ## 
    ## Number of Fisher Scoring iterations: 6

Then, let’s try forward selection.

``` r
# forward stepwise algorithm
fwd <- step(nullmod, scope = list(upper = fullmod), direction = "forward")
```

    ## Start:  AIC=680.28
    ## Survived ~ 1
    ## 
    ##          Df Deviance    AIC
    ## + Sex     1   528.30 532.30
    ## + Pclass  2   613.97 619.97
    ## + Age     1   673.19 677.19
    ## <none>        678.28 680.28
    ## 
    ## Step:  AIC=532.3
    ## Survived ~ Sex
    ## 
    ##          Df Deviance    AIC
    ## + Pclass  2   470.13 478.13
    ## <none>        528.30 532.30
    ## + Age     1   527.75 533.75
    ## 
    ## Step:  AIC=478.13
    ## Survived ~ Sex + Pclass
    ## 
    ##              Df Deviance    AIC
    ## + Age         1   449.22 459.22
    ## + Pclass:Sex  2   450.25 462.25
    ## <none>            470.13 478.13
    ## 
    ## Step:  AIC=459.22
    ## Survived ~ Sex + Pclass + Age
    ## 
    ##              Df Deviance    AIC
    ## + Pclass:Sex  2   427.78 441.78
    ## + Age:Sex     1   437.13 449.13
    ## <none>            449.22 459.22
    ## + Pclass:Age  2   446.82 460.82
    ## 
    ## Step:  AIC=441.78
    ## Survived ~ Sex + Pclass + Age + Sex:Pclass
    ## 
    ##              Df Deviance    AIC
    ## + Age:Sex     1   424.72 440.72
    ## <none>            427.78 441.78
    ## + Pclass:Age  2   424.63 442.63
    ## 
    ## Step:  AIC=440.72
    ## Survived ~ Sex + Pclass + Age + Sex:Pclass + Sex:Age
    ## 
    ##              Df Deviance    AIC
    ## <none>            424.72 440.72
    ## + Pclass:Age  2   422.84 442.84

``` r
# show the model forward chose
summary(fwd)
```

    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Sex:Pclass + Sex:Age, 
    ##     family = "binomial", data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7922  -0.6427  -0.4090   0.4568   2.6879  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      4.92606    1.17396   4.196 2.72e-05 ***
    ## Sexmale         -3.11707    1.30060  -2.397   0.0165 *  
    ## Pclass2         -2.21545    1.11884  -1.980   0.0477 *  
    ## Pclass3         -4.55819    1.06634  -4.275 1.91e-05 ***
    ## Age             -0.02097    0.01605  -1.307   0.1913    
    ## Sexmale:Pclass2  0.34856    1.21728   0.286   0.7746    
    ## Sexmale:Pclass3  2.35787    1.13962   2.069   0.0385 *  
    ## Sexmale:Age     -0.03592    0.02046  -1.756   0.0792 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 424.72  on 492  degrees of freedom
    ## AIC: 440.72
    ## 
    ## Number of Fisher Scoring iterations: 6

The most convenient way is to use the Stepwise regression.

    ## Start:  AIC=680.28
    ## Survived ~ 1
    ## 
    ##          Df Deviance    AIC
    ## + Sex     1   528.30 532.30
    ## + Pclass  2   613.97 619.97
    ## + Age     1   673.19 677.19
    ## <none>        678.28 680.28
    ## 
    ## Step:  AIC=532.3
    ## Survived ~ Sex
    ## 
    ##          Df Deviance    AIC
    ## + Pclass  2   470.13 478.13
    ## <none>        528.30 532.30
    ## + Age     1   527.75 533.75
    ## - Sex     1   678.28 680.28
    ## 
    ## Step:  AIC=478.13
    ## Survived ~ Sex + Pclass
    ## 
    ##              Df Deviance    AIC
    ## + Age         1   449.22 459.22
    ## + Pclass:Sex  2   450.25 462.25
    ## <none>            470.13 478.13
    ## - Pclass      2   528.30 532.30
    ## - Sex         1   613.97 619.97
    ## 
    ## Step:  AIC=459.22
    ## Survived ~ Sex + Pclass + Age
    ## 
    ##              Df Deviance    AIC
    ## + Pclass:Sex  2   427.78 441.78
    ## + Age:Sex     1   437.13 449.13
    ## <none>            449.22 459.22
    ## + Pclass:Age  2   446.82 460.82
    ## - Age         1   470.13 478.13
    ## - Pclass      2   527.75 533.75
    ## - Sex         1   573.90 581.90
    ## 
    ## Step:  AIC=441.78
    ## Survived ~ Sex + Pclass + Age + Sex:Pclass
    ## 
    ##              Df Deviance    AIC
    ## + Age:Sex     1   424.72 440.72
    ## <none>            427.78 441.78
    ## + Pclass:Age  2   424.63 442.63
    ## - Sex:Pclass  2   449.22 459.22
    ## - Age         1   450.25 462.25
    ## 
    ## Step:  AIC=440.72
    ## Survived ~ Sex + Pclass + Age + Sex:Pclass + Sex:Age
    ## 
    ##              Df Deviance    AIC
    ## <none>            424.72 440.72
    ## - Sex:Age     1   427.78 441.78
    ## + Pclass:Age  2   422.84 442.84
    ## - Sex:Pclass  2   437.13 449.13

    ## 
    ## Call:
    ## glm(formula = Survived ~ Sex + Pclass + Age + Sex:Pclass + Sex:Age, 
    ##     family = "binomial", data = titanic_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7922  -0.6427  -0.4090   0.4568   2.6879  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      4.92606    1.17396   4.196 2.72e-05 ***
    ## Sexmale         -3.11707    1.30060  -2.397   0.0165 *  
    ## Pclass2         -2.21545    1.11884  -1.980   0.0477 *  
    ## Pclass3         -4.55819    1.06634  -4.275 1.91e-05 ***
    ## Age             -0.02097    0.01605  -1.307   0.1913    
    ## Sexmale:Pclass2  0.34856    1.21728   0.286   0.7746    
    ## Sexmale:Pclass3  2.35787    1.13962   2.069   0.0385 *  
    ## Sexmale:Age     -0.03592    0.02046  -1.756   0.0792 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 678.28  on 499  degrees of freedom
    ## Residual deviance: 424.72  on 492  degrees of freedom
    ## AIC: 440.72
    ## 
    ## Number of Fisher Scoring iterations: 6

Let’s visualize the effects of the variables then.

``` r
plot(effects::allEffects(both))
```

![](PreModWorkshop_files/figure-markdown_github/unnamed-chunk-10-1.png)
