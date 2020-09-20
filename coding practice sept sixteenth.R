library(tidyverse)

data("iris")
iris

vec <- 1:10

vec > 2
#this asks what in the vector named "vec" is greater than 2 

vec >= 2
#greater than or equal to

vec == # is equal to asking
  
iris$Species == "setosa"

setosa_rows <- iris$Species == "setosa"

iris[setosa_rows, ]

5 %in% vec
#is 5 found in the vector called "vec"

iris$Species == "setosa" | iris$Species == "virginica"
set_vir <- iris$Species %in% c("setosa","virginica")


filter(iris,Species == "virginica")
filter(iris,Species != "versicolor")
filter(iris,Species == "setosa" | Species == "viginica")
#all ways to do the same thing 

data("mtcars")
?mtcars
glimpse(mtcars)

# subset mtcars so we only have rows that match practice
# hp > 100 and disp < 150
#use filter function from tidyverse 

filter(mtcars, hp > 100 & disp < 150)
#practice with mtcars to come up with subsets 

