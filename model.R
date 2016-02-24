
# Starting R 

library(igraph)
library(dplyr)
library(ggplot2)
library(randomForest)
library(data.table)

options(stringsAsFactors = FALSE)

##### Opening and basic formating of datasets

setwd("D:/Google Drive/Mikike/BusinessAnalytics/Tananyag/Data Science for Business/Final project")

train.adult.df <- read.csv("adult.data", header = FALSE, sep = ",")
colnames(train.adult.df) <- c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","earning")
setDT(train.adult.df)
train.adult.df[train.adult.df==" ?",]=NA

test.adult.df <- read.csv("adult.test", header = FALSE, sep = ",")
colnames(test.adult.df) <- c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","earning")
setDT(test.adult.df)
test.adult.df[test.adult.df==" ?",]=NA
test.adult.df <- test.adult.df[-1,]


dt <- train.adult.df


##### Checking features

## age

plot(dt$age, main = "Age distribution", ylab = "Age", ylim = c(0, 110))
ggplot(dt, aes(x=age))+geom_histogram(binwidth = 1)
# strange 90-year-old population
a <- NULL
for(i in 17:90){
  a <- c(a,dt[age==i, .N])
}
summary(dt$age)
sum(is.na(dt$age))

dt[, logage:= log(age)]
ggplot(dt, aes(x=logage))+geom_histogram(binwidth = .1)

## workclass

dt$workclass = as.factor(dt$workclass)
ggplot(dt, aes(x=workclass))+geom_bar()
unique(dt$workclass)
sum(is.na(dt$workclass))
sum(is.na(dt$workclass))/length(dt$workclass)
dt$workclass = as.character(dt$workclass)

## fnlwgt

plot(dt$fnlwgt, main = "fnlwgt distribution", ylab = "fnlwgt")
ggplot(dt, aes(x=fnlwgt))+geom_histogram(binwidth = 30000)
summary(dt$fnlwgt)
sum(is.na(dt$fnlwgt))

dt[, logfnlwgt:= log(fnlwgt)]
ggplot(dt, aes(x=logfnlwgt))+geom_histogram()

## education

dt$education = as.factor(dt$education)
ggplot(dt, aes(x=education))+geom_bar()
unique(dt$education)
sum(is.na(dt$education))
dt$education = as.character(dt$education)

## education_num

#plot(dt$education_num, main = "education_num distribution", ylab = "education_num")
ggplot(dt, aes(x=education_num))+geom_histogram(binwidth = 1)
summary(dt$education_num)
sum(is.na(dt$education_num))

## marital status

dt$marital_status = as.factor(dt$marital_status)
ggplot(dt, aes(x=marital_status))+geom_bar()
unique(dt$marital_status)
sum(is.na(dt$marital_status))
dt$marital_status = as.character(dt$marital_status)  

## occupation

dt$occupation = as.factor(dt$occupation)
ggplot(dt, aes(x=occupation))+geom_bar()
unique(dt$occupation)
sum(is.na(dt$occupation))
sum(is.na(dt$occupation))/length(dt$occupation)
dt$occupation = as.character(dt$occupation) 

## relationship

dt$relationship = as.factor(dt$relationship)
ggplot(dt, aes(x=relationship))+geom_bar()
unique(dt$relationship)
sum(is.na(dt$relationship))
dt$relationship = as.character(dt$relationship) 

## race

dt$race = as.factor(dt$race)
ggplot(dt, aes(x=race))+geom_bar()
unique(dt$race)
sum(is.na(dt$race))
dt$race = as.character(dt$race)

## sex

dt$sex = as.factor(dt$sex)
ggplot(dt, aes(x=sex))+geom_bar()
unique(dt$sex)
sum(is.na(dt$sex))
dt$sex = as.character(dt$sex)

## capital_gain

plot(dt$capital_gain, main = "capital_gain distribution", ylab = "capital_gain")
ggplot(dt, aes(x=capital_gain))+geom_histogram(binwidth = 5)
summary(dt$capital_gain)
sum(is.na(dt$capital_gain))


## capital_loss

plot(dt$capital_loss, main = "capital_loss distribution", ylab = "capital_loss")
ggplot(dt, aes(x=capital_loss))+geom_histogram(binwidth = 5)
summary(dt$capital_loss)
sum(is.na(dt$capital_loss))

## hours_per_week

plot(dt$hours_per_week, main = "hours_per_week distribution", ylab = "hours_per_week")
ggplot(dt, aes(x=hours_per_week))+geom_histogram(binwidth = 5)
summary(dt$hours_per_week)
sum(is.na(dt$hours_per_week))

## Native country

dt$native_country = as.factor(dt$native_country)
ggplot(dt, aes(x=native_country))+geom_bar()
unique(dt$native_country)
sum(is.na(dt$native_country))
sum(is.na(dt$native_country))/length(dt$native_country)
country <- data.table(table(dt$native_country, useNA = "always"))
colnames(country) <- c("Country", "Nr")
a <-  sum(country$Nr)
country[, Percent :=  round(Nr/sum(a)*100,4)]
dt$native_country = as.character(dt$native_country)
# is that count? USA: 89.6, MX: 2, NA: 1.8, Phil: 0.6


##### chk $workclass vs $occupation because of the similarity in nr of NAs

sum(is.na(dt$occupation)==is.na(dt$workclass))/length(dt$workclass)
# NAs at the same records





############### sandbox
factorx <- factor(cut(dt$native_country, breaks=unique(dt$native_country)))
str(factorx)
factor(dt$native_country)
data.frame(table(dt$native_country))

