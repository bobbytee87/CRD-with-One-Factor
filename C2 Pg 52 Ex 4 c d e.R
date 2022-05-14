#This is the code for exercise 4, question c on page 52
#Biscuits are made with 4 different levels of Baking powder
#There are four repeats per level.
#The height or the rise is measured

#The table on page 52 with experimental data is written to a CSV file "BiscuitsE

rm(list = ls())# removes all objects from the environment
setwd('C:/Users/bobby/Desktop/My Career/Data Science Material/Design and Analysis of Experiements')# The location of csv file
Biscuit<- read.csv('BiscuitsExercise.csv')# read data
attach(Biscuit)# Bring it to environment

#Question c:"Formulate a contrast to test the hypothesis that increase in rise height
#is a linear function of the increase in baking powder in the dough, and
#test this hypothesis."?

#First we build a linear model with BakingPowder as a factor
Biscuit$BakingPowder<-as.factor(BakingPowder)
class(BakingPowder)# It will be numeric!

detach(Biscuit)
attach(Biscuit) 
class(BakingPowder)#now BakingPowder is a factor

#now we make a linear model of Rise~BakingPowder
mod1<-lm(Rise~BakingPowder, data = Biscuit)
summary(mod1)
# If you inspect the model coefficients, they already provide answer to question 4c
print(mod1$coefficients)
#Rise increase approximately by 16 for an every increase of BakingPowder by 0.25 
#Note that the Intercept is the Rise for BakingPowder= 0.25
# The coefficient for Baking powder = 0.5 is the difference in Rise for BakingPowder = 0.5 relative to 0.25
# We can prove it if required
mu025 <-mean(Rise[BakingPowder==0.25])# Notice square brackets
mu050 <- mean(Rise[BakingPowder==0.5])

print(mu050-mu025)
print(mod1$coefficients[2])
#The above two are the identical. So it proves that The coefficient for Baking powder = 0.5 is the difference in Rise for BakingPowder = 0.5 relative to 0.25

#Now to answer the question 4b, we fit contrast as follows
contrast050_025 <-fit.contrast(mod1, 'BakingPowder', c(-1, 1, 0, 0))
print(contrast050_025) #The contrast estimate is 16.65 and P value is <0.05. So statistically significant

contrast075_050 <-fit.contrast(mod1, 'BakingPowder', c(0, -1, 1, 0))
print(contrast075_050) #The contrast estimate is 19.4 and P value is <0.05. So statistically significant

contrast100_075 <-fit.contrast(mod1, 'BakingPowder', c(0, 0, -1, 1))
print(contrast100_075) #The contrast estimate is 15.875 and P value is <0.05. So statistically significant

#So we see that the contrast for each step in increasing by approx ~16
#Implying the effect of BakingPowder level is linear

# Question d; Estimate the variance of the experimental error σ2.

Risehat = predict(mod1, Biscuit)
SSE = sum((Rise-Risehat)^2)

#Question e: Make a plot of residuals versus predicted values and normal plot of
#residuals and comment on whether the assumptions of the linear model
#are justified
resid<-residuals(mod1)
plot(Risehat, resid, xlab='Fitted Rise', ylab = 'Residuals')

#Question f: The question is not clear
