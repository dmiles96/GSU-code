# Import Data and Run Packages
library(haven)
ANES_2000 <- read_dta("ANES 2000.dta")
View(ANES_2000)

set.seed(112460) ## Seed for replicability

require(plyr) ## Utility tools
require(dplyr)## Utility tools
require(lmtest) ## Supplemental and postestimation tests
require(sandwich) ## Specific for the sandwich version of robust SE calculations
require(ggplot2) ## Graphical presentation
require(stargazer) ## Tables
require(car) # Companion to Applied Regression
require(carData) # Supplemental Data
require(gmodels) # Crosstabs
require(ggplot2) # Primary Graphs
require(MASS) # For polr
require(brant) # For brant test
require(margins) 
require(jtools) 
require(ggstance) 
require(broom.mixed) 
require(Rcpp) 
require(sjPlot)
require(sjmisc) 
require(ggeffects)
require(plm)
require(lmtest)
require(ordinal) # CLM
require(reshape2)
require(effects)
require(aod) # For wald.test
require(stats4) # For BIC
require(tidyverse)

#VCF0303 - Party ID
#VCF0104 - Gender
#VCF0105a - Race
#VCF0128a - Religious Affiliation
#VCF0110 - Education


binarydata <- ANES_2000

#Religion Cleaning
binarydata <- binarydata %>%
  filter(VCF0128 %in% c("1","2","3","4"))
names(binarydata) [names(binarydata)== 'VCF0128'] <- 'Religion'

#Education Cleaning
binarydata <- binarydata %>%
  filter(VCF0110 %in% c("1", "2", "3", "4"))
names(binarydata) [names(binarydata)== 'VCF0110'] <- 'Education'

#Gender Cleaning
binarydata <- binarydata %>%
  filter(VCF0104 %in% c("1", "2", "3"))
names(binarydata) [names(binarydata)== 'VCF0104'] <- 'Gender'

#Race Cleaning
binarydata <- binarydata %>%
  filter(VCF0105a %in% c("1", "2", "3", "4","5","6","7"))
names(binarydata) [names(binarydata)== 'VCF0105a'] <- 'Race'

#Party ID
binarydata <- binarydata %>%
  filter(VCF0303 %in% c("1", "2", "3"))
names(binarydata) [names(binarydata)== 'VCF0303'] <- 'PartyID'

#VCF0604 is Govt.Trust: Do What Is Right
binarydata <- binarydata %>%
  filter(VCF0604 %in% c("1", "2", "3"))
names(binarydata) [names(binarydata)== 'VCF0604'] <- 'Ideology'

#------------------------------------------------------------------------------#

require(gmodels)
model1<-glm(as.factor(Ideology) ~ PartyID + Race + Religion + Education + Gender, family = binomial(link = "logit"), data=binarydata)
summary(model1)
model2<-glm(as.factor(Ideology) ~ Race + Religion + Education + Gender, family = binomial(link = "logit"), data=binarydata)
summary(model2)
model3<-glm(as.factor(Ideology) ~ Race + Education + Gender, family = binomial(link = "logit"), data=binarydata)
summary(model3)
model4<-glm(as.factor(Ideology) ~ Religion + Education + Gender, family = binomial(link = "logit"), data=binarydata)
summary(model4)
model5<-glm(as.factor(PartyID) ~ Ideology + Race + Religion + Education + Gender, family = binomial(link = "logit"), data=binarydata)
summary(model5)

#First Model
ANES_fit_Table1<-polr(as.factor(Ideology) ~ PartyID + Race + Gender + Religion + Education, data=binarydata, Hess = TRUE)
summary(ANES_fit_Table1) # summarizing the results (to include SES)
brant(ANES_fit_Table1) # PLA Test
poTest(ANES_fit_Table1) # PLA Test
stargazer(ANES_fit_Table1)


ANES_fit_Table1_Pred <- predict(ANES_fit_Table1, type = "probs")

ANES_fit_Table1_Pred

stargazer(ANES_fit_Table1, type = "latex", keep.stat = c("n", "ll", "bic"), 
          add.lines=list(c("poTest", 
                           table.layout = "mc-b-t-sa-n")))

pred_data_8 <- data.frame(Ideology=rep(median(as.numeric(binarydata$Ideology)),4),
                          Religion=c(1,2,3,4),
                          Race=rep(median(as.numeric(binarydata$Race)),4),
                          PartyID=rep(median(as.numeric(binarydata$PartyID)),4),
                          Gender=rep(median(as.numeric(binarydata$Gender)),4),
                          Education=rep(median(as.numeric(binarydata$Education)),4))


view(pred_data_8)


pred_data_9 <- data.frame(Ideology=rep(median(as.numeric(binarydata$Ideology)),2),
                          Race=rep(median(as.numeric(binarydata$Race)),2),
                          Religion=rep(median(as.numeric(binarydata$Religion)),2),
                          PartyID=rep(median(as.numeric(binarydata$PartyID)),2),
                          Gender=c(1,2),
                          Education=rep(median(as.numeric(binarydata$Education)),2))

ggdata1<-cbind(pred_data_8, predict(ANES_fit_Table1, pred_data_8, type = "probs"))
ggdata2<-cbind(pred_data_9, predict(ANES_fit_Table1, pred_data_9, type = "probs"))

view(ggdata1)


ggdata12 <- melt(ggdata1, id.vars = c("Ideology", "PartyID", "Race", "Education", "Gender", "Religion"),
                 variable.name = "Level", value.name="Probability")

ggdata12 #Molten Effect (not class) for use in ggplot

ggdata22 <- melt(ggdata2, id.vars = c("PartyID", "Religion", "Ideology", "Gender", "Race", "Education"),
                 variable.name = "Level", value.name="Probability")

ggplot(ggdata12, aes(x = PartyID, y = Probability, colour = Level)) +
  geom_line(size=1.5) + ggtitle("Predicted Probability of Ideology, by Party ID") + 
  ylab("Probability") + xlab("Ideology") + scale_x_continuous(breaks=c(1,2,3),
                                                                 labels=c("1","2","3"))

ggplot(ggdata22, aes(x = Ideology, y = Probability, colour = Level)) +
  geom_line(size=1.5) + ggtitle("Predicted Probability of Ideology, by Education Level") + 
  ylab("Probability") + xlab("Education") + scale_x_continuous(breaks=c(1,2),
                                                            labels=c("1","2"))

ggplot(ggdata22, aes(x = Ideology, y = Probability, colour = Level)) +
  geom_line(size=1.5) + ggtitle("Predicted Probability of Religion, by Education Level") + 
  ylab("Probability") + xlab("Religion") + scale_x_continuous(breaks=c(1,2),
                                                               labels=c("1","2"))



#Cross Tables
CrossTable(binarydata$Ideology)
CrossTable(binarydata$PartyID)
CrossTable(binarydata$Religion)
CrossTable(binarydata$Gender)
CrossTable(binarydata$Race)
CrossTable(binarydata$Education)