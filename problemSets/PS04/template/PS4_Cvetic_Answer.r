#set wd

#setwd("C:\\Users\Andrej Cvetic\Documents\GitHub\StatsI_Fall2021\problemSets\PS04\template")

#TASK 1#
#load libraries

install.packages("car")
library(car)
data(Prestige)
help(Prestige)


#coding the variable professional 

professional <- ifelse(Prestige$type == 'prof', 1,0)

mod1 <- lm(Prestige$prestige ~ Prestige$income + professional + Prestige$income:professional, data = Prestige)
options(scipen=999)
summary(mod1)
plot_coefs <- mod1$coefficients

#TASK 2# 

p1 <- 2*pt(2.625, 128, lower.tail = F)
p1

p2 <- 2*pt(3.23076923077, 128, lower.tail = F)
p2

