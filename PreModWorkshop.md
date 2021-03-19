Titanic Dataset Description
---------------------------

The `titanic` library in R has 4 different datasets, they are
**titanic\_train**, **titanic\_test**, **titanic\_gender\_model**, and
**titanic\_gender\_class\_model**. They describe the survival status of
individual passengers on the Titanic. The variable description can be
found below:

1.  Pclass: Passenger Class (1 = 1st, 2 = 2nd, 3 = 3rd)

2.  survival: Survival (0 = No; 1 = Yes)

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

Coding
------

### Data preprocessing

``` r
library(titanic)
```

    ## Warning: package 'titanic' was built under R version 4.0.3

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.0.6     v dplyr   1.0.4
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.0.3

    ## Warning: package 'tibble' was built under R version 4.0.3

    ## Warning: package 'dplyr' was built under R version 4.0.3

    ## Warning: package 'forcats' was built under R version 4.0.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
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

``` r
# clean the data
titanic_train <- titanic_train %>% select(Survived, Pclass, Age, Sex) %>% 
  mutate(Survived = as.factor(Survived), Pclass = as.factor(Pclass)) %>% na.omit()

titanic_test <- titanic_test %>% select(Pclass, Age, Sex) %>% 
  mutate(Pclass = as.factor(Pclass)) %>% na.omit()
```

Descriptive analysis of data

![](PreModWorkshop_files/figure-markdown_github/descriptive-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
