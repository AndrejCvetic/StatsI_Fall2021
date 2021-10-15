#####################
# load libraries
# set wd
# clear global .envir
#####################

library(ggplot2)

#####################
# Problem 1
#####################
# (A) #
mat1.data <- c(14, 6, 7, 7, 7, 1)
mat1 <- matrix(mat1.data,nrow=2,ncol=3,byrow=TRUE)
mat1
rownames(mat1) <- c("Upper class","Lower class")
colnames(mat1) <-c("Not Stopped","Bribe requested","Stopped/given warning")
mat1

#these are all f observed 
#for expected (row total - grand total)*column total 
#calculate expected 

rowt1 <- sum(14,6,7)
rowt2 <- (7+7+1)
colt1 <- (14+7)
colt2 <- (6+7)
colt3 <- (7+1)

grad_total <- sum(rowt1, rowt2, colt1, colt2, colt3)

#I am making my expected variables by rows!

f1exp <- (rowt1/grad_total)*colt1
f2exp <- (rowt1/grad_total)*colt2
f3exp <- (rowt1/grad_total)*colt3
f4exp <- (rowt2/grad_total)*colt1
f5exp <- (rowt2/grad_total)*colt2
f6exp <- (rowt2/grad_total)*colt3

f1 <- (14-f1exp)^2/f1exp
f2 <- (6-f2exp)^2/f2exp
f3 <- (7-f3exp)^2/f3exp
f4 <- (7-f4exp)^2/f4exp
f5 <- (7-f5exp)^2/f5exp
f6 <- (1-f6exp)^2/f6exp

chisq <- sum(f1,f2,f2,f4,f5,f6)
chisq

?chisq.test
?Chisquare

# (B) #
df <- (2-1)*(3-1)
p.value <- pchisq(chisq, df, lower.tail=FALSE)
p.value
# What if alpha = .1?

# JUST AN ADDITIONAL CHECK OF THE DATA # 
chisq.results <- chisq.test(mat1, correct = FALSE)
chisq.results

# (C) #

# Z for every number: (fobserved - fexp)/sqrt(fexp*(1-rowtotal/grad)*(1-coltotal-grad))

z1 <- ((14-f1exp)/(sqrt(f1exp*(1-(rowt1/grad_total))*(colt1/grad_total))))
z2 <- ((6-f2exp)/(sqrt(f2exp*(1-(rowt1/grad_total))*(colt2/grad_total))))
z3 <- ((7-f3exp)/(sqrt(f3exp*(1-(rowt1/grad_total))*(colt3/grad_total))))
z4 <- ((7-f4exp)/(sqrt(f4exp*(1-(rowt2/grad_total))*(colt1/grad_total))))
z5 <- ((7-f5exp)/(sqrt(f5exp*(1-(rowt2/grad_total))*(colt2/grad_total))))
z6 <- ((1-f6exp)/(sqrt(f6exp*(1-(rowt2/grad_total))*(colt3/grad_total))))

# more or less likely to solicit a bribe from drivers depending on their class
# H0: Bribe and class are statistically independent > if H0 is true then f = fexp
# H1: Bribe and class are statistically dependent

# I can say that bribe and class are not statistically independent, as per my calculations. H1 true
# As per computer calculations the relationship is statistically independent.               H0 true

# The larger the chi square value is, the stronger the evidence against H0 (f=fexp)

############## e symbol ##############
# In statistics, the symbol e is a mathematical constant approximately equal to 2.71828183.

# Prism switches to scientific notation when the values are very large or very small. For example:
  
 # 2.3e-5, means 2.3 times ten to the minus five power, or 0.000023
 # 4.5e6 means 4.5 times ten to the sixth power, or 4500000 which is the same as 4,500,000
##############

############# P VALUE AND ALPHA #############

#Alpha, the significance level, is the probability that you will make the mistake of rejecting the null hypothesis when in fact it is true.  
#The p-value measures the probability of getting a more extreme value than the one you got from the experiment.  
#If the p-value is greater than alpha, you accept the null hypothesis.  
#If it is less than alpha, you reject the null hypothesis.

# alpha = .1
# my P = 1.893205e-05 > less than alpha = reject null = class and bribe are not independent
# comp p = 0.1502 > slightly more than alpha = accept null = class and bribe are independent

# (d) #

# They are useful in helping to interpret chi-square tables by providing information about 
# which cells contribute to a significant chi-square.
# If the standardized residual is beyond the range of ± 2, then that cell can be considered to
# be a major contributor, if it is > +2, or a very weak contributor, if it is beyond -2, to the
# overall chi-square value.
# A large standardized residual provides evidence against independence in that cell. = against H0
# When H0 is true, there is only about a 5% chance that any particular 
#standardized residual exceeds 2 in absolute value.

#####################
# Problem 2
#####################

women <- read.csv ("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", fill= TRUE)

# (a) # 

# You need to estimate the efect of the reservation policy 
#on the number of new or repaired drinking water facilities in the villages.

# H0: There is no effect of reservation policy on the number of new or repaired drinking water facilities.
# H1: There is an effect of reservation policy on the number of new or repaired drinking water facilities.

# The null hypothesis says nothing is happening. 

# (b) #

class(women)
typeof (women)

influence <- lm(women$water ~ women$reserved, data = women)
summary(influence)
print(influence)

# OR TO GO BY HAND #

# BETA 1 #
beta <- sum((women$water - mean(women$water)) * (women$reserved - mean(women$reserved)))/
  sum((women$reserved - mean(women$reserved))^2)
beta

# Alpha # 
alpha <- mean(women$water)-beta*mean(women$reserved)
alpha

reg_DF <- as.data.frame(cbind(women$reserved,women$water))
reg_DF

# STANDARD EROR # 
sig <- sigma(influence)
sig

# SE FOR BETA 1 #

beta_se <- sig/sqrt(sum((reg_DF$V1)-mean(reg_DF$V1))^2)
beta_se

# SE FOR ALPHA #


alpha_se <- sig * sqrt((1/dim(reg_DF) [1]) + (mean(reg_DF$V1)^2)/sum((reg_DF$V1-mean(reg_DF$V1))^2))
alpha_se

#beta
betaC <- 2*pt((beta-0)/beta_se, dim(reg_DF) [1]-2, lower.tail = F)
betaC

#alpha
alphaC <- 2*pt((alpha-0)/alpha_se, dim(reg_DF) [1]-2, lower.tail = F)
alphaC

###############
# PROBLEM 3
###############

# (a) #
fruitfly <-read.csv ("http://mldata.org/repository/data/download/csv/uci-20070111-fruitfly/")
typeof(fruitfly)
summary(fruitfly)

fruitfly

# (b) #

plot(fruitfly$Thorax, fruitfly$Longevity,
     main = "Thorax vs Plot life span",
     xlab = "Thorax",
     ylab = "Life span")

correlation.results <- cor(fruitfly$Longevity,fruitfly$Thorax)
correlation.results

# (c) #


reg.results <- lm(fruitfly$Longevity ~ fruitfly$Thorax)
summary(reg.results)

abline(lm(fruitfly$Longevity ~ fruitfly$Thorax), col = "purple")


# (d) #

fruitfly_chisq <- chisq.test(table(fruitfly$Longevity, fruitfly$Thorax))
fruitfly_chisq
summary(fruitfly_chisq)
# (e) #

?confint

# calculating by hand:

tvalue <- (0.0028068-0)/0.0003067
tvalue
# same sa that from the table

# confidence interval for beta 1

#beta 1+t score*se
#beta 1-tscore*se

upperCI <- 0.0028068 + (9.151614*0.0003067)
lowerCI <- 0.0028068 - (9.151614*0.0003067)  

upperCI
lowerCI

#BY FORMULA

confint(reg.results, 'fruitfly$Longevity', level=0.90)

# (f) #

class(reg.results)
newDF <- reg.results
newDF <- as.data.frame(cbind(newDF))
newDF$Thorax <- 0.8
typeof(newDF)

prediction_res <- predict(lm(fruitfly$Longevity ~ fruitfly$Thorax), 
                   newdata=newDF$Thorax, se.fit=T) 
