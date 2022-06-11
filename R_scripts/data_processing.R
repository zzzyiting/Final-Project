library(tidyverse)
library(tidymodels)
library(sqldf)
library(psych)
library(Hmisc)
library(VIM)

# read data
telcom <- read.csv("ml/data/unprocessed/WA_Fn-UseC_-Telco-Customer-Churn.csv")
head(telcom)

# display stats 
describe(telcom)

# draw distribution graph
churn_count <- sqldf("select churn, count(1) as count from telcom group by churn")
p1 <- ggplot(data=churn_count,aes(x=Churn, y=count, fill=Churn)) + geom_col() + ggtitle ('Customer Attrition in Data')
p1


gender_count <- sqldf("select gender, count(1) as count from telcom group by gender")
p2 <- ggplot(data=gender_count, aes(x=gender, y=count, fill=gender)) + geom_col() + ggtitle ('Distribution of Gender')
p2


seniorcitizen_count <- sqldf("select seniorcitizen, count(1) as count from telcom group by seniorcitizen")
p3 <- ggplot(data=seniorcitizen_count, aes(x=SeniorCitizen, y=count, fill=SeniorCitizen)) + geom_col() + ggtitle ('Distribution of SeniorCitizen')
p3

partner_count <- sqldf("select Partner, count(1) as count from telcom group by Partner")
p4 <- ggplot(data=partner_count, aes(x=Partner, y=count, fill=Partner)) + geom_col() + ggtitle ('Distribution of Partner')
p4

dependents_count <- sqldf("select Dependents, count(1) as count from telcom group by Dependents")
p5 <- ggplot(data=dependents_count, aes(x=Dependents, y=count, fill=Dependents)) + geom_col() + ggtitle ('Distribution of Dependents')
p5

tenure_count <- sqldf("select tenure, count(1) as count from telcom group by tenure")
p6 <- ggplot(data=tenure_count, aes(x=tenure, y=count, fill=tenure)) + geom_col() + ggtitle ('Distribution of tenure')
p6

monthly_charges_count <- as.data.frame(table(cut(telcom$MonthlyCharges, breaks=10)))
p7 <- ggplot(data=monthly_charges_count, aes(x=Var1, y=Freq, fill=Var1)) + geom_col() + ggtitle ('Distribution of MonthlyCharges') + theme(axis.text.x = element_blank()) + xlab("MonthlyCharges")
p7

total_charges_count <- as.data.frame(table(cut(telcom$TotalCharges, breaks=10)))
p8 <- ggplot(data=total_charges_count, aes(x=Var1, y=Freq, fill=Var1)) + geom_col() + ggtitle ('Distribution of TotalCharges') + theme(axis.text.x = element_blank()) + xlab("TotalCharges")
p8

# median imputation
aggr(telcom$TotalCharges, prop=FALSE, numbers=TRUE)
telcom$TotalCharges=impute(telcom$TotalCharges, median)

aggr(telcom$MonthlyCharges, prop=FALSE, numbers=TRUE)
aggr(telcom$SeniorCitizen, prop=FALSE, numbers=TRUE)
aggr(telcom$tenure, prop=FALSE, numbers=TRUE)


save(telcom, file = "ml/data/processed/telcom.Rdata")
