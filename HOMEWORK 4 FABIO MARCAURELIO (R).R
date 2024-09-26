rm(list=ls())


#########################
###    HOMEWORK 3     ###
### FABIO MARCAURELIO ###
###     599027        ###
#########################


##### LIBRARY #####

library(AER) 
library(lmtest)
library(stargazer)
library(lmtest)
library(sandwich)
library(wooldridge)
library(car)
library(bucky)
library(dplyr)
library(plyr) 
library(ggplot2)
attach(homework)
homework <- read.csv(( "hw4_marcaurelio.csv"))

####################
#### QUESTION 1 ####
####################

#################################
### FOR COMMENTS SEE PDF FILE ###
#################################

#################################### A #########################################################

##### build the data.set #####

homework$dambexp <- ifelse(homework$ambexp>0, 1, 0) #if exp>0 then 1, 0 otherwise 
table(homework$dambexp) #share with positive exp
hist(dambexp) #positive exp is triple 

# 526 Ambulatory do not spend (excluding dental and outpatient mental) anything, 1367 ambulatory do #

#################################### B #########################################################

##### start using an LPM for estimate the probability of positive expenditure #####

lpmexp <- lm(dambexp ~ totchr+age+educ)
robust.summary(lpmexp, vcov = vcovHC) #always robust standard errors

##### evaluating statistically significance #####

linearHypothesis(lpmexp, c("totchr=0")) #statistically different from zero F>10
linearHypothesis(lpmexp, c("age=0")) #statistically different from zero F>10
linearHypothesis(lpmexp, c("educ=0")) #statistically different from zero F>10

linearHypothesis(lpmexp, c("totchr=0", "age=0", "educ=0")) # jointly statistically different from zero F>10

#################################### C #########################################################

##### interpreation linear probability model #####

summary(fitted(lpmexp))
# interpretation as predicted probabilities?? #
exp_hat <- predict(lpmexp)
summary(exp_hat) #min is above zero and this is ok but max is above 1. then it is not a probability measure
plot(exp_hat)

# this linear probability model is not a probability measure since is not in between 0 and 1 #

#################################### D #########################################################

##### Probit model #####
probitexp <- glm(dambexp ~ totchr+age+educ, 
              family = binomial(link = "probit")) #family=binomial is for probit

summary(probitexp) #bias if there is heteroskedasticity, we can not fix it 
lht(probitexp, matchCoefs(probitexp,"totchr"))
confint(probitexp)

probit_hat <- probitexp$fitted.values
summary(probit_hat)
# predicted linear combination
probitlinearcomb <- probitexp$linear.predictors
summary(probitlinearcomb)

stargazer(lpmexp,probitexp,type="text") #compare 

#################################### E #########################################################

# 1) Average marginal effect 
AME_totchr <- dnorm(probitlinearcomb)*coefficients(probitexp)["totchr"]
summary(AME_totchr) # from 0 to 0.35
# 2) Marginal effect at the mean
MEM_totchr <- dnorm(mean(probitlinearcomb))*coefficients(probitexp)["totchr"]
MEM_totchr #0.26

#################################### F #########################################################



#################################### G #########################################################

##### Logit model #####
logitexp <- glm(dambexp ~ totchr+age+educ, 
             family = binomial)
summary(logitexp)

logit_hat <- logitexp$fitted.values
summary(logit_hat)
# predicted linear combination
logitlinearcomb <- logitexp$linear.predictors
summary(logitlinearcomb)

# 1) Average marginal effect 
AME_totchr1 <- dnorm(logitlinearcomb)*coefficients(logitexp)["totchr"]
summary(AME_totchr1) 
# 2) Marginal effect at the mean
MEM_totchr1 <- dnorm(mean(logitlinearcomb))*coefficients(logitexp)["totchr"]
MEM_totchr1 #0.27 similiar to probit

ggplot(homework, aes(x=totchr, y=dambexp)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) # plot 
