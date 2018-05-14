##############################
#       DEMO code for        #
#  Nonparametric estimation  #
#   Author : Dongwoo Kim     #
##############################

#install.packages("np") 
# you need to install the package "np" to use nonparametric 
# estimation commands written by Li and Racine.

library(np) # load the package "np"
cat("\014") # this clears the console window (clc in Matlab)
rm(list=ls()) # this clears the data space (clear all in Matlab)

set.seed(1004) # it allows you to generate the same random numbers given n
# 1004 means "angel" in Korean (the same pronunciation)
# you can choose any number you like

n         <- 1000 # sample size for random number generation

x         <- rnorm(n) # generate n random numbers from standard normal dist
u         <- rnorm(n) # generate n random numbers from standard normal dist
grid      <- seq(-4, 4, 0.01) # fine grid points on [-4, 4]

y         <- exp(x)/(1+exp(x)) + u # outcome y generated
g         <- exp(grid)/(1+exp(grid)) # true conditional mean function of y given x

density_x <- density(x) # density estimator of f(x)
density_y <- density(y) # density estimator of f(x)
# density() uses a rule-of-thumb bandwidth so not optimal in general

true_den  <- dnorm(grid) # true density of x (standard normal pdf)
# dnorm() gives standard normal density at the argument (f(x))
# pnorm() gives standard normal cumulative distribution at the argument (F(x))

plot(grid, true_den, type = "l", col = "red", ylim = c(0, 0.4), xlim = c(-4, 4))
# draw the plot of true density function
# type = "l" gives a line graph
# col gives colour 

lines(density_x) # draw the density estimator of x on the plot above

h1 <- npudensbw(x) # compute bandwidth for np unconditional density of x
summary(h1) # you can see the information about h1

plot(h1, ylim = c(0, 0.4), xlim = c(-4, 4)) # draw the density estimator from np package
lines(grid, true_den, col = "red") # draw the true density of x on the plot above

#h2 <- npcdensbw(formula = y~x) # np conditional density of y given x
#plot(h2) # show you the density plot

h3 <- npregbw(formula = y~x) # nonparametric regression of y on x
plot(h3) # draw the estimated conditional mean function
lines(grid, g, col = "red") # draw the true conditional mean function

