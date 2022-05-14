#This is the code for exercise 4 on page 52
#Biscuits are made with 4 different levels of Baking powder
#There are four repeats per level.
#The height or the rise is measured

#The table on page 52 with experimental data is written to a CSV file "BiscuitsE

rm(list = ls())# removes all objects from the environment
setwd('C:/Users/bobby/Desktop/My Career/Data Science Material/Design and Analysis of Experiements')# The location of csv file
Biscuit<- read.csv('BiscuitsExercise.csv')# read data
attach(Biscuit)# Bring it to environment

#Question 1:"what is the experimental unit"?
#Answer 1: Experimental unit is "Biscuits" 

#Question 2: Perform the analysis of variance to test the hypothesis
#of no treatment unit
#Answer 2: We use aov function in R to do so

mod1<-lm(Rise~BakingPowder, data = Biscuit) #Fit a linear model
anovaOut<-anova(mod1)
Fstats <-anovaOut$'F value'
Pvalue<-anovaOut$`Pr(>F)` 
print(c(Fstats, Pvalue))
# The Pvalue is very small and < 0.05. Thus we reject the null hypothesis
# that level of BakingPowder has no effect on Rise. i.e. the Rise with
# or without treatment is not different which is the null hypothesis.


# The Pvalue is very small and < 0.05. Thus we reject the null hypothesis
# that level of BakingPowder has no effect on Rise. i.e. the Rise with
# or without treatment is not different which is the null hypothesis.

# How to do anova by hand? We ascertain the 
muRise<-mean(Rise) #mean Rise
Risehat<- predict(mod1, Biscuit)# Predicted Rise
errRise<-Rise-Risehat# error (Observed-predicted)
SSE<-sum(errRise^2) # Sum of squares of error

SSR<-sum((muRise-Risehat)^2)# sum of squares of regression
SST <-sum((Rise-muRise)^2) # summ of squares total

dfErr<-dim(Biscuit)[1]-2 # 2 degress of freedom lost. Since y = mx+c, and we are esitmating m and c.
dfT<-dim(Biscuit)[1] - 1 # one degree of freedom lost as we estimate muRise to calculate SST
dfR <- dfT-dfErr; # degress of freedom for regression. 

#The formula for F statistics =  (SSR/dfR)/(SSE/dfErr)
Fstats_hand <- (SSR/dfR)/(SSE/dfErr) # This should match Fstats above
Pvalue_hand <-pf(1/Fstats_hand, dfErr,dfR)# This should match Pvalue above
print(c(Fstats_hand,Pvalue_hand))
# since Pvalue or Pvalue_hand <0.05, we reject the null hypothesis that
# level of BakingPowder has no effect on Rise.














