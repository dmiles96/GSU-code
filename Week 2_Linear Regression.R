######################################################################
###################### Linear Regression Review ######################
######################################################################

setwd("C:/Users/evanl/Desktop/Fall 2021 8830 TA/R Tutorials") ## Working directory
set.seed(112460) ## Seed for replicability

require(datasets) ## Where the data comes from
require(dplyr) ## Utility tools
require(lmtest) ## Supplemental and postestimation tests
require(sandwich) ## Specific for the sandwich calculation of robust SE calculations
require(ggplot2) ## Graphical presentation
require(stargazer) ## Tables

state<-data.frame(state.x77) ## Adjusting class of data
attach(state) ## Attaching variables for quicker calls

## OLS Model

mRate<-glm(Murder ~ Population + Income + Illiteracy, family = gaussian, data = state)
summary(mRate)

## Looking for heteroskedasticity

state$residuals_lm<-mRate$residuals

ggplot(data=state, aes(y=residuals_lm, x=Population))  + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data=state, aes(y=residuals_lm, x=Income))  + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data=state, aes(y=residuals_lm, x=Illiteracy))  + geom_point(col = 'blue') + geom_abline(slope = 0)

## Testing for heteroskedasticity

bptest(mRate)
coeftest(mRate, vcov = vcovHC(mRate, "HC1"))

## Calculating the 'robust' standard errors

cov_m<-vcovHC(mRate, method = "HC1")
rob_m<-sqrt(diag(cov_m))

## Presentation of Results

stargazer(mRate, type = "latex", 
          se=list(rob_m),
          ci=TRUE, df=FALSE, keep.stat = c("n"),
          out = "OLS mRate Outputs.tex")

## Alternatively

alt_mRate<-lm(Murder ~ Population + Income + Illiteracy, data = state)
summary(mRate)
summary(alt_mRate)

######################################################################
##################### Post-estimation strategies #####################
######################################################################

## Linearity of Relationship Under Study

qqnorm(residuals(mRate), ylab = "Residuals")
qqline(residuals(mRate))

## Normality of Residuals 

#Distribution of Residuals

hist(mRate$residuals)
sd(mRate$residuals)

## Testing for Multicollinearity 

# Correlation

cor.test(Population, Income, method = c("pearson"), use = "complete.obs")
cor.test(Population, Illiteracy, method = c("pearson"), use = "complete.obs")
cor.test(Income, Illiteracy, method = c("pearson"), use = "complete.obs")

#Tolerance (Below 0.1 is an issue)

tolerance=(1-(mRate$deviance/mRate$null.deviance))

# Variable inflation factor (Greater than 5 is an issue, greater than 10 is proof of multicollinearity)

vif(mRate)

## Testing for heteroskedasticity

bptest(mRate)
coeftest(mRate, vcov = vcovHC(mRate, "HC1"))

# Serial/autocorrelation

dwtest(Murder ~ Population + Income + Illiteracy)
