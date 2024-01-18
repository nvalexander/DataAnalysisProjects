library(datasets)
data(iris)
#  In this dataset, what is the mean of 'Sepal.Length' for the species virginica?
mean(iris[iris$Species == "virginica", ][["Sepal.Length"]])
# What R code returns a vector of the means of the variables
# 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
sapply(c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), function(x) mean(iris[[x]]))
# Syynonymous, thanks to requested ordfer being the same as the order of columns in the irirs data frame:
apply(iris[,1:4], 2, mean)

library(datasets)
data(mtcars)
# calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)
tapply(mtcars$mpg, mtcars$cyl, mean, na.rm = T)
# what is the absolute difference between the average horsepower of 4-cylinder cars and 
# the average horsepower of 8-cylinder cars
meanhps <- tapply(mtcars$hp, mtcars$cyl, mean, na.rm = T)
meanhps[["4"]] - meanhps[["8"]]
