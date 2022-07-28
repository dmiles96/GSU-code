library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(haven)
library(reshape2)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
install.packages("gapminder")
install.packages("janitor")
library(janitor)
library(gapminder)
library(tidyverse)
require(datasets) ## Where the data comes from
require(dplyr) ## Utility tools
require(lmtest) ## Supplemental and postestimation tests
require(sandwich) ## Specific for the sandwich calculation of robust SE calculations
require(ggplot2) ## Graphical presentation
require(stargazer) ## Tables

set.seed(112460) ## Seed for replicability

## Educational Attainment ##
correlation_EDU_internet_usage <- correlation_between_internet_usage_and_mean_years_of_schooling
names(correlation_EDU_internet_usage)[names(correlation_EDU_internet_usage) == 'Entity'] <- "Country"
names(correlation_EDU_internet_usage)[names(correlation_EDU_internet_usage) == 'Educational Attainment (average years of total education) (Barro Lee Education Dataset (2010))'] <- "Edu_Attain"
names(correlation_EDU_internet_usage)[names(correlation_EDU_internet_usage) == 'Percentage of Individuals using the Internet (ICT) (2015)'] <- "Percent_of_ppl_using_Internet"
view(correlation_EDU_internet_usage)
correlation_EDU_internet_usage <- subset(correlation_EDU_internet_usage, select = -c(Code, Continent))
correlation_EDU_internet_usage <- correlation_EDU_internet_usage %>%
  filter(grepl('United States', Country))
correlation_EDU_internet_usage <- correlation_EDU_internet_usage[1:13,]
view(correlation_EDU_internet_usage)

## Hours on SMP ##
hrs_on_SMP <- daily_hours_spent_with_digital_media_per_adult_user_3
names(hrs_on_SMP)[names(hrs_on_SMP) == 'Entity'] <- "Country"
hrs_on_SMP <- subset(hrs_on_SMP, select = -(Code))
hrs_on_SMP <- hrs_on_SMP
names(hrs_on_SMP)[names(hrs_on_SMP) == 'Year'] <- "Year_Hrs_on_SMP"
names(hrs_on_SMP)[names(hrs_on_SMP) == 'Mobile (BOND Internet Trends (2019))'] <- "Mobile_Internet_Use"
names(hrs_on_SMP)[names(hrs_on_SMP) == 'Desktop/Laptop (BOND Internet Trends (2019))'] <- "Desktop_Laptop_Internet_Use"
names(hrs_on_SMP)[names(hrs_on_SMP) == 'Other Connected Devices (BOND Internet Trends (2019))'] <- "Other_Device_Internet_Use"
view(hrs_on_SMP)


## Adults on SMP ##
adults_on_SMP <- statistic_id246230_share_of_us_adults_who_use_selected_social_networks_2020
adults_on_SMP <- adults_on_SMP[-c(1,2),]
names(adults_on_SMP)[names(adults_on_SMP) == 'X2'] <- "Percent"
adults_on_SMP <- subset(adults_on_SMP, select = -c(X3))

## News Source ##
SMP_as_news_source <- statistic_id718019_social_media_as_a_news_source_worldwide_2021
SMP_as_news_source <- SMP_as_news_source[-c(1,2),]
names(SMP_as_news_source)[names(SMP_as_news_source) == 'X2'] <- "Percent"
SMP_as_news_source <- subset(SMP_as_news_source, select = -c(X3))


 ## Joined Data --- EDU & Hrs ##
joined_data <- inner_join(hrs_on_SMP, correlation_EDU_internet_usage, by= "Country")
view(joined_data)

joined_data_2 <- na.omit(joined_data)
view(joined_data_2)

## Users on SMP ##
users_on_SMP <- users_by_social_media_platform
names(users_on_SMP)[names(users_on_SMP) == 'Entity'] <- "Platform"
users_on_SMP <- subset(users_on_SMP, select = -(Code))


#2. Model & Equation
HRSOL_MOBILE_EDU <- glm(Mobile_Internet_Use ~ Edu_Attain + Year_Hrs_on_SMP, family = "gaussian", data = joined_data_2)
summary(HRSOL_MOBILE_EDU)

HRSOL_DL_EDU <- glm(Desktop_Laptop_Internet_Use ~ Year_Hrs_on_SMP + Edu_Attain, family = "gaussian", data = joined_data_2)
summary(HRSOL_DL_EDU)

combined <- glm(Percent_of_ppl_using_Internet ~ Year, family = "gaussian", data = joined_data_2)
summary(combined)


#3. Presentation of Results
stargazer(HRSOL_MOBILE_EDU, type = "latex", 
          out = "OLS Mobile Internet Use + EDU Attainment + % Online.tex")

stargazer(HRSOL_DL_EDU, type = "latex", 
          out = "OLS Desk/Laptop Internet Use Outputs + Percent of People Using Internet by Year.tex")

stargazer(combined, type = "latex", 
          out = "OLS Percent of People Using Internet by Year.tex")

#4. Heteroskedasticity & Linearity of Relationship Under Study
bptest(HRSOL_DL_EDU)
bptest(combined)
bptest(HRSOL_MOBILE_EDU)

qqnorm(residuals(HRSOL_MOBILE_EDU), main = "Mobile Internet Use",
       ylab = "Residuals")
qqline(residuals(EDU_HRS))

qqnorm(residuals(combined),main = "Percent of People Using Internet by Year",
       ylab = "Residuals")
qqline(residuals(combined))

qqnorm(residuals(HRSOL_DL_EDU), main = "Desktop/Laptop Internet Use",
       ylab = "Residuals")
qqline(residuals(HRSOL_DL_EDU))