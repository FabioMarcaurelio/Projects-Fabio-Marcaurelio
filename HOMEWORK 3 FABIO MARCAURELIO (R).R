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
attach(marcaurelio_xlsx)

####################
#### QUESTION 1 ####
####################

#################################
### FOR COMMENTS SEE PDF FILE ###
#################################

#################################### 1.A #########################################################

##### start from a simple model with a non linear viariable #####

QchildrenOLS<-lm(children  ~ educ + age + I(age^2))
summary(QchildrenOLS, vcov=vcovHC()) # p-value equal to zero in all variables
summary(QchildrenOLS) # no difference under homoskedasticity assumptions
coefci(QchildrenOLS) # for educ C.I coefficients is in between (-13,9%;-9,8%)
hist(educ) #very few women with high years of education

# all variable are statistically significant, p-value=0#

##### we check for non linearity in age #####

OLSage<-lm(children  ~ age)
summary(OLSage)
summary(OLSage, vcov=vcovHC)

OLSagesq<- lm(children ~ poly(age, 2, raw=TRUE))
summary(OLSagesq,vcov=vcovHC)
linearHypothesis(OLSagesq, matchCoefs(OLSagesq,"age"))

plot(age, children, 
     xlab="age",ylab="children")
abline(OLSage, col="red") #linear relation
lines(x=age, y=fitted(OLSagesq), col="blue") # quadratic relationship

# age have a non linear component #

##### average effect #####

b <- coef(OLSagesq)
mean(age)
b["poly(age, 2, raw = TRUE)1"]+2*b["poly(age, 2, raw = TRUE)2"]*mean(age)

##### prediction #####

X<- data.frame(age = seq(from=18,to=60,by=1))
pred <- predict(OLSagesq, X, interval = "confidence")
predr <- predict(robustify(OLSagesq), X, interval = "confidence")
cbind(X,pred,predr)[1:60,]
matplot(X$age, predr, type = "l", lty=c(1,2,2))

beta_age <- coefficients(OLSagesq)["poly(age, 2, raw = TRUE)1"] 
beta_age2 <- coefficients(OLSagesq)["poly(age, 2, raw = TRUE)2"] 
turning_point <- -beta_age / (2*beta_age2)
print(turning_point) # turning point of number of children per women 50 years old

#################################### 1.B #########################################################

##### test for individual significance #####

linearHypothesis(QchildrenOLS, matchCoefs(QchildrenOLS,"educ")) #F>10 for educ=0 
linearHypothesis(QchildrenOLS, matchCoefs(QchildrenOLS,"age")) #F>10 for age=0

#################################### 1.C #########################################################

##### test for significance in age and age^2 #####

#H0: age+age^2=0
R <- matrix(rbind(c(0,0, 1,0), c(0,0,0,1)), nrow=2) 
r <- c(0,0)
t <- qf(1 - 0.05, 2, 1589-4) 
test=linearHypothesis(QchildrenOLS, hypothesis.matrix=R, rhs=r, vcov = vcovHC(QchildrenOLS, type="HC3"))

if(test$F<-t||test$F>t) {print("we reject the null")} else {print("we accept the null")}

# age and age^2 are joitnly statistically significant #

#################################### 1.D #########################################################

##### add urban in the model #####

QchildrenOLSurban<-lm(children  ~ educ + age + I(age^2) + urban)
summary(QchildrenOLSurban,vcov=vcovHC) #urban statistically significant
summary(QchildrenOLSurban) # no change under homoskedasticity assumption

#################################### 1.E #########################################################

linearHypothesis(QchildrenOLSurban, matchCoefs(QchildrenOLSurban,"urban")) #F>10 (12.825)

#################################### 1.F #########################################################

### compare the two models ###

stargazer(QchildrenOLS,QchildrenOLSurban,type="text") 

# small changes in the parameter of educ, from -0.119 to -0.108 (probably omitted variable)

##### omitted variable? #####

# a regression that is omitting an explanatory variable is suffering from omitted variable bias if
# 1- urban and educ are correlated #
# 2- urban have an effect on number of children #

educurban<-lm(educ ~ urban)
summary(educurban) #i know that it is biased 
cor.test(educ,urban)# 1- seems satisfied

urbanOLS<-lm(children  ~ educ + age + I(age^2) + urban)
summary(urbanOLS,vcov=vcovHC) #2- seems staisfied

# possible omitted variable, this is not a true and consistent statistical test but it could give us an idea #
# we have to remember that in this model we can have collinearity between educ and urban #


####################
#### QUESTION 2 ####
####################



##### starting considering a model with two insturumental variables as a regressors (probably collinearity) #####

OLSfrstbic<-lm(children  ~ age + I(age^2) + urban + frsthalf + bicycle)
summary(OLSfrstbic, vcov=vcovHC()) #without educ low relation

stargazer(QchildrenOLSurban, OLSfrstbic, type="text") 

#################################### 2.B #########################################################

##### first stage for firsthalf + bicycle instruments #####
##### fitted(st1educ) = educhat = part of educ that is explained by the exogenous frsthalf + bicycle #####

st1educ <- lm(educ ~  age + I(age^2) + urban+ frsthalf + bicycle)
summary(st1educ) # both significant (p-value=0), instruments are relevant 
educhat <- fitted(st1educ)
coefci(st1educ)
educres<-resid(st1educ)

lht(st1educ,c("frsthalf","bicycle"),vcov=vcovHC, type="HC3") #F>10 

cor.test(educ,frsthalf) #  magnitude of “correlation” (-0.15, as we suspects in the PDF)
cor.test(educ,bicycle) #  magnitude of “correlation” (0.08 not so correlated)

# in the constraint frsthalf=0=bicycle instruments are relevant #

##### manually test for relevance of pi-> Ho: pi=0 with pi=frsthalf #####

testfrst= coef(st1educ)["frsthalf"]/sqrt(vcovHC(st1educ)[4,4])
if (!(testfrst<1.96&&testfrst>testfrst)) {
  print("We reject H0 (relevant instrument)")
} else {print("We do not reject H0 (istrument can be relevant)")}

##### manually test for relevance of pi-> Ho: pi=0 with pi=bicycle #####

testbic= coef(st1educ)["bicycle"]/sqrt(vcovHC(st1educ)[5,5])
if (!(testbic<1.96&&testbic>-1.96)) {
  print("We reject H0 (instrument is relevant)")
} else {print("We do not reject H0 (instrument can be relevant)")}

# we reject the null in both cases, instruments are RELEVANT #

##### second stage for firsthalf + bicycle instruments #####

st2educ <- lm(children ~  age + I(age^2) + urban + educhat)
summary(st2educ) # equal to IVreg but wrong Standard Error 

# for validity we have to check for exogenous of the instrument #
# Given that educhat is the part of the endogenous explanatory variable not correlated with the error term
#of the structural equation of interest, this stage considers COV(u, W) = 0

##### IVreg under overidentify #####

IVfrstbic <- ivreg(children ~ educ + age + I(age^2) + urban | age + I(age^2) + urban + frsthalf + bicycle + bicycle )
summary(IVfrstbic, diagnostics = TRUE) 

# instruments are jointly significant, we reject the null of weak instruments (strong) #
# we reject the null, it is possible that instruments are not valid #

##### compare coefficients in IVreg and OLS #####

compareCoefs(QchildrenOLSurban, IVfrstbic) # educ change a lot

##### compare second stage and IVreg in firsthalf + bicyle case #####

stargazer(IVfrstbic,st2educ,type="text") 

# As we can see, the estimates and s.e. for educ are same for 2SLS and “two-stage OLS” #

#################################### 2.C #########################################################

##### consider only firsthalf as an instruments and assume cor(frsthalf'error)= 0 (exogeneity) #####
##### first stage for frsthalf #####

st1frst <- lm(educ ~ age + I(age^2) + urban + frsthalf)
summary(st1frst) # frsthalf significant

frsthat <- fitted(st1frst)
frstres<- resid(st1frst)

lht(st1frst,c("frsthalf"),vcov=vcovHC, type="HC3") #F>10(37) RELEVANT instrument

# we can not test for exogeneity of an instruments (cor[z,u]=0) in the exactly identify case, #
# in fact we assume it. since instruments is relevant we can say that by construction that is Valid #

#################################### 2.D #########################################################

##### second stage for firsthalf #####

st2frst <- lm(children ~ frsthat + age + I(age^2) + urban)
summary(st2frst) #equal coefficients of IVreg but wrong s.e.

# IVreg for firsthalf #

IVfrst <- ivreg(children  ~ educ + age + I(age^2) + urban | age + I(age^2) + urban + frsthalf)
summary(IVfrst, diagnostics = TRUE) #sargan does not works in this framework (L=K)

# we do not reject the null, educ is exogenous in this framework #
# then OLS is better then IV both under Gauss-Markov and heteroskedasticiy #

##### compare coefficients in IVreg and OLS #####

compareCoefs(QchildrenOLSurban, IVfrst) # educ change a lot

##### compare second stage and IVreg in firsthalf case #####

stargazer(IVfrst,st2frst,type="text") 

# As we can see, the estimates and s.e. for educ are same for 2SLS and “two-stage OLS” #

#################################### 2.E #########################################################

##### Hausman test for endogeneity of educ given frsthalf instrument #####

OLShausman<-lm(educ ~ age + I(age^2) + urban + frsthalf)
summary(OLShausman)
lht(OLShausman,c("frsthalf"),vcov=vcovHC, type="HC3") #F>10
residhausman<-resid(OLShausman)
hist(residhausman)

hausman.test<-lm(children ~  educ + age + I(age^2)  + urban + residhausman )
summary(hausman.test) #no significant, educ exogenous IN THIS FRAMEWORK

lht(hausman.test,c("residhausman"),vcov=vcovHC, type="HC3") #F<10

# we do not reject the null, educ is exogenous in this framework #
# then OLS is better then IV both under Gauss-Markov and heteroskedasticiy #
# since we assume that instrument is valid this test is correct in statistically sense #
# we can do nothing more without another data for certify the exogeneity of educ #

