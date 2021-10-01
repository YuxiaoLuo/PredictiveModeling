##############################################
## Intro to R Workshop 
## 10/01/2021
## GC Digital Fellows
##############################################

2+2
# ctrl + enter

########################
# Object and Functions #
########################

## create objects: <- or =
# shortcut in RStudio: Alt + -
# create an object a with any number in your mind
a <- 2

# rules of name: 
# 1. cannot start with a number
# 2. case sensitive
# 3. do not use names of fundamental functions in R (https://stat.ethz.ch/R-manual/R-devel/library/base/html/Reserved.html)
# 4. better not to use other function names (ex., sd, mean)
# 5. better not to use dots 

## Q1 in Zoom Poll

# print value
print(a)
(a <- 2)

# using objects (square, plus, times)
a^2
a*3
(b <- 2)

# modifying objects
(a <- a*2)

## Exercise 2
# create a new object, b, and assign it the value 10. Now, multiply b by 10. 
# Assign this new value to the object, c.
rm(b)
b <- 10 
c <- b * 10

############
## Functions
# function(argument1, argument2, ...)
# function that takes single argument
sqrt(16)
factorial(3)
args(round)
args(sqrt)

# function that takes multiple arguments
log(100, 10)
log(10, 100)

args(log)

# args(): what intpus a function recognizes

args(round)

log(x = 100, base = 10)
log(base = 10, x = 100)

# function for computing correlation matrix
cor

## Exercise 3
# See what arguments are within the sum() function. One of them is ...(ellipsis)
# use Google to figure out what ... argument means
?sum

# If you don't use argument names, R will match your inputs based on the order written
# Specifying input with argument names can help avoid potential errors

# Default arguments
# NA: missing values, not available 

sum(c(1,2,NA))
sum(c(1,2,NA), na.rm = FALSE)

# Function code body
cor

###############
## Getting help
# Google: round a number in R
# ? or help() in R (exact search of the name)
# ?? in R for a broader search (all functions containing the name)

# calculate the mean
?mean 

??mean

# An example using Google to search "round to the tens place in R"
data <- c(152.335, 39.341, 21.892)


##########################
# Vectors and data types #
##########################

#########
## Vector

## c() function
my_vector <- c(1,2,3,4) 
my_vector

char_vector <- c("I", "love", "to", "code")
char_vector

## Exercise 1
# You go to a restaurant with 2 of your friends, the bill of the meal looks like
bill <- c(James = 32, Tony = 28, Alpha = 34)
bill

# the tip is 20% for each person, how to calculate it in the vector?
bill_total <- bill * (1 + 0.2)
bill_total

############
## Data Type

# int, dbl, chr, dttm, lgl, fctr, date

# typeof(), data type of your vector
dec <- c(3.24, 4.2, 2.04, 7.5)
typeof(dec)

# double: decmials 
# int: integer, whole number without decmials 

# a vector cannot store more than 1 type of data
mix_vec <- c(1 ,"two", 3)
typeof(mix_vec) 

## Q2 in Zoom Poll
# Check the type of this vector in poll
# mix_nums <- c(1L, 2L, 2.5, 3)
# 1L: integer 1
# 2L: integer 2

mix_nums <- c(1L, 2L, 2.5, 3)
typeof(mix_nums)

## Fuction uses vectors
my_num <- c(3,5,5)
mean(my_num)

# Vectorized functions
factorial(my_num)

# pmax()
vec <- c(1,2,3,4,5)
vec2 <- c(4,3,2,6,8)
pmax(vec, vec2)

# Exercise 3
# You have two vectors that contain the words in a sentence 
sentence_1 <- c("I", "want", "a", "pet", "turtle")
sentence_2 <- c("It", "will", "be", "green", "and", "brown")

# Count the number of words in each sentence
length(sentence_1)
length(sentence_2)

# Concatenate the two vectors and assign them to a new object (for simplicity, call it sentence)
sentence <- c(sentence_1, sentence_2)
sentence

# Now for a tricky task. Count the number of letters in the combined sentences. 
# Hint: nchar() function can count the number of letters in each word
nchar(sentence)

## Subsetting vectors
z <- c(10, 11, 12, 13, 14, 15)
z[3]

# Exercise 4
# Create a new object that contains all of the words that contain more than one letter. 
# Note: use numerical indexing to do this
sentence_1 <- c("I", "want", "a", "pet", "dog")
sentence_1[c(2,4,5)]

####################
# Write a function

## Example of a function
function_name <- function(arg_1, arg_2, arg_3 = 2) {
  
  # do some useful stuff
  new_var <- sum(arg_1) + sum(arg_2) 
  
  # do some more useful stuff
  div_out <- new_var / arg_3
  
  # return the value
  return(div_out) 
}


## Function to take an average
take_avg <- function(x){
  
  # this function takes the same of all values in x. Assumes x is a numeric vector
  s <- sum(x)
  
  # this counts the number of elements in x
  l <- length(x)
  
  # we're dividing the sum of values by the number of values to calculate the mean
  avg <- s / l
  
  # this specifies the value to be returned by the function
  return(avg)
}


# let's validate the function using the vector below
v <- c(2.4, 3.65, 7.5, 1.22)
take_avg(v)
mean(v)

##################
# Data Structure #
##################

# Primary data structures in R: vector, list, data frame

## List
# Lists are objects where you can store different data types
# While vectors only store a single data type, each "slot" in a list can contain something different, 
# and they can be almost arbitrarily complex.

list_data <- list("Bob", "Rachel", c(10, 2, 3), TRUE, FALSE, 22.4)
list_data

# extract slots using bracket indexing [[]]
my_list <- list("All", "dogs", "are", "good", "dogs")
my_list[[1]]

# extract slots by naming 
my_named_list <-
  list(
    char_vec = c("This", "is", "cool"),
    num_vec = c(12, 23, 44),
    log_vec = c(TRUE, FALSE, FALSE)
  )

# use names() function to name the slots
names(my_named_list) <- c("char", "num", "log")
my_named_list

## Exercise 1
# You recorded the latitude/longitude coordinates of some of your favorite restaurants, 
# but forgot to name the list slots.

restaurant_list <-
  list(
    c(40.7594889, 40.7379186, 40.7467251),
    c(-73.9847482,-73.9814907,-73.9861467),
    c("Authentic Italian", "Authentic Mexican", "Best Deli")
  )

restaurant_list

# 1. Name each list slot latitude, longitude, and restaurant in that order
# 2. You forgot whether you included the negative signs to indicate that the restaurants are in 
# the western hemisphere. Access the longitude slot to double-check.
# 3. What are the data types of each slot? (typeof() should help you out)

##############
# Data Frame

# Data frames are tabular representations of data where the columns are vectors of equal length. 
# Each column must contain a single type of data (remember, they're vectors!). 
# Typically, you'll either load in data that is recorded in spreadsheet format (e.g. an Excel sheet), 
# or you'll coerce non-tabular data into a data frame format 
# (e.g. JSON files, which aren't natively represented as a table).

example_df <-
  data.frame(numbers = c(1, 2, 3, 4, 5),
             letters = c("a", "b", "c", "d", "e"))
example_df

# select columns using: dataframe$column
example_df$numbers

# Exercise 2: access the numbers column in example_df
example_df$letters

#############
# Tidy Data

# Tidy data is tabular data, where each column is a variable and each row is an observation. 
# Tidy data is standardized in a way that R can easily analyze. 
# Messy data is anything that deviates from this format.

# Non-tidy format 
stockPrice <- data.frame(
  Date = c("2009-01-01","2009-01-02"),
  Boeing = c(173.55,172.61),
  Amazon = c(174.90,171.42),
  Google = c(174.34,170.04)
)
stockPrice

# Tidy format
stockPrice_tidy <- tidyr::pivot_longer(
  stockPrice,
  names_to = "Company",
  values_to = "StockPrice",
  cols = -Date
)
stockPrice_tidy

# Three common problems seen in messy data sets are:
# 1. Column headers are values, not variable names.
# 2. Multiple variables are stored in one column.
# 3. Variables are stored in both rows and columns.


############
# Tibble

# Tibbles are data frames that cut out the extra fluff that the base R data.frame class adds 
# to your data structure. Tibbles also add a bit of useful information to your data frame output.

# The heading tells you the dimensions of your data.
# The data type of each variable is printed under the variable name. 
# Tibbles will conveniently limit the number of rows printed for easy viewing

stockPrice_tidy

##########################
# Nested data structures

## JSON (JavaScript Object Notation) is a common data type used in a variety of applications, 
# ex., transmitting data between servers and web applications.

# we need to use package like jsonlite to analyze JSON data

# {
#   "Students": [
#     
#     { "Name":"Amit Goenka" ,
#       "Major":"Physics" }, 
#     { "Name":"Smita Pallod" ,
#       "Major":"Chemistry" }, 
#     { "Name":"Rajeev Sen" , 
#       "Major":"Mathematics" }
#   ]
# }
# example pulled from https://www.w3resource.com/JSON/structures.php


## XML is a data structure similar to JSON in that it is hierarchical and flexible
# A widely used example of the XML data format is the Microsoft Excel .xlsx file. 
# In fact, XML can handle an even wider array of data types than JSON, like images. 
# It is a markup language, very similar to HTML, that can be rendered.

# we need to use package like xml2 to help parsing the data. 

# "<employees>
#   <employee>
#     <firstName>John</firstName> <lastName>Doe</lastName>
#   </employee>
#   <employee>
#     <firstName>Anna</firstName> <lastName>Smith</lastName>
#   </employee>
#   <employee>
#     <firstName>Peter</firstName> <lastName>Jones</lastName>
#   </employee>
# </employees>"

################
# Packages
# R packages are collections of functions and data sets developed by the community. 
# They increase the power of R by improving existing base R functionalities or by adding new ones.

# description of package
packageDescription('stats')
# open help window
help(package = 'stats')

## Repositories 
# A repository is a place where packages are located so you can install them from it. 
# Although you or your organization might have a local repository, 
# typically they are online and accessible to everyone.

# 3 most popular repositories: 
# CRAN: official repository, it is a network of ftp and web servers maintained by the R community
# Bioconductor: a topic specific repository, intended for open source software for bioinformatics
# Github: not R specific, most popular repository for open source projects

## Installing packages from CRAN 
# install.packages("package")
# install.packages(c("stars", "sf"))

# choose specific mirror from CRAN
chooseCRANmirror()


## Installing packages via devtools
# devtools package can facilitate the process of installing packages 
# from different sources, including CRAN

# To use devtools package:
# You need to install Rtools on Windows (https://cran.r-project.org/bin/windows/Rtools/)
# Xcode command line tools on Mac (xcode-select --install)

install.packages("devtools")

# install from CRAN
install_cran()
# install_cran("ggplot2")

# install from Github 
install_github()
# install_github("klutometis/roxygen")

# install from a URL
install_url()
#install_url("https://github.com/hadley/stringr/archive/HEAD.zip")

# install a specific version of a CRAN package
install_version
# install_version("mypackage", "1.15")


## Update, remove, and check installed packages
# check what packages are installed 

# uninstall a package 

# check what packages need an update with a call to the function 

# update all packages 

# update a specific package, just install it again 



## How to load packages
# packagename::functionname()

# Exercise 1
# Do you remember how to see an overview of what functions and data are contained in a package?
# Hint: try help() function


# access the band instruments dataset in dplyr package
dplyr::band_instruments


# load a package in the memory use library()
library(dplyr)

# sometimes, multiple packages have functions with same names
# designate the package name before the function would be a good choice


# Unload a package using detach()
detach("package:dplyr", unload = T)


## Vignettes
# documents where the authors show some functionalities of their package in a more detailed way
vignette(package = "dplyr")
vignette("colwise")

# open browser to check the vignettes
browseVignettes(package = "dplyr")


## Reproducibility 

# When you are approaching the end of a project and are planning to publish your code and results, 
# it's a good idea to include a metadata file with your published code. This metadata file should 
# include information about your session that others may need to accurately reproduce your analysis.

sessionInfo()

# write the output to a file
capture.output(sessionInfo(), file = "sessionInfo.txt")

## credit to the open source developer

# An R citation
citation()

# citation of a specific package 
citation("ggplot2")

##################
# Importing data #
##################

## Reading rectangular data
# use readr package from the tidyverse ecosystem

# read_csv() comma-delimited files, ex., csv txt
# read_csv2() semicolon separated files, ex., csv txt
# read_tsv() tab-delimited files, ex., tsv txt
# read_text() space-delimited files, ex., txt
# read_delim() reads files with any delimiter (specify it within the function) ex., txt

# install.packages("readr")

library(readr)

# read_csv() function can download files from URL
# use first line of the data as column names

# store path to file as an object. This is generally good practice because 
# it makes modifying code down the line easier
titan_path <- "https://raw.githubusercontent.com/YuxiaoLuo/PredictiveModeling/main/data/titanic_train.csv"

titan <- read_csv(titan_path)


# supply an inline csv file
read_csv("a,b,c
1,2,3
4,5,6")


# use skip = n to skip the first n lines
# how to revise the following code?
read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3")

# use comment = "#" to drop all lines starting with #
# how to revise the following code?
read_csv("# A commenet I want to skip
  x,y,z
  1,2,3")


# What if the data doesn't have column names
# use col_names = FALSE and label them sequentially from X1 to Xn

# how to revise the following code?
read_csv("1,2,3\n4,5,6")


# you can also name the columns yourself
# use col_names() to pass a character vector

# how to revise the code to name the columns as "x", "y", "z"?
read_csv("1,2,3\n4,5,6")


# handle missing values 
# use na = "" to tell R what characters should be treated as missing values

# how to revise the code to tag "." as NA (missing value)
read_csv("a,b,c\n1,2,.")



##############################
## Other data types

##################
# Rectangular data

# haven reads SPSS, STATA, and SAS files

# readxl reads excel files (ex., xls, xlsx)

# odbc, reads database specific backend (ex., RMySQL, RSQLite, etc) and 
# run SQl queries against a database and return a data frame


##################
# Hierarchical data

# jsonlite reads json format
# xml2 for xml format










