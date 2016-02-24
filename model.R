
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

##### Target variable: earning

summary (dt$earning)
table(dt$earning)

##### Checking features

## age

plot(dt$age, main = "Age distribution", ylab = "Age", ylim = c(0, 110))
ggplot(dt, aes(x=age))+geom_histogram(binwidth = 1)+facet_grid(earning~., scales = "free")
# strange 90-year-old population
a <- NULL
for(i in 17:90){
  a <- c(a,dt[age==i, .N])
}
summary(dt$age)
sum(is.na(dt$age))

dt[, logage:= log(age)]
ggplot(dt, aes(x=logage))+geom_histogram(binwidth = .05)+facet_grid(earning~., scales = "free")

## workclass

dt$workclass = as.factor(dt$workclass)
ggplot(dt, aes(x=workclass))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$workclass)
sum(is.na(dt$workclass))
sum(is.na(dt$workclass))/length(dt$workclass)
dt$workclass = as.character(dt$workclass)

## fnlwgt
# outlier???
plot(dt$fnlwgt, main = "fnlwgt distribution", ylab = "fnlwgt")
ggplot(dt, aes(x=fnlwgt))+geom_histogram(binwidth = 30000)+facet_grid(earning~., scales = "free")
summary(dt$fnlwgt)
sum(is.na(dt$fnlwgt))

dt[, logfnlwgt:= log(fnlwgt)]
ggplot(dt, aes(x=logfnlwgt))+geom_histogram()

## education

dt$education = as.factor(dt$education)
ggplot(dt, aes(x=education))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$education)
sum(is.na(dt$education))
dt$education = as.character(dt$education)

## education_num

#plot(dt$education_num, main = "education_num distribution", ylab = "education_num")
ggplot(dt, aes(x=education_num))+geom_histogram(binwidth = 1)+facet_grid(earning~., scales = "free")
summary(dt$education_num)
sum(is.na(dt$education_num))

## marital status

dt$marital_status = as.factor(dt$marital_status)
ggplot(dt, aes(x=marital_status))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$marital_status)
sum(is.na(dt$marital_status))
dt$marital_status = as.character(dt$marital_status)  

## occupation

dt$occupation = as.factor(dt$occupation)
ggplot(dt, aes(x=occupation))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$occupation)
sum(is.na(dt$occupation))
sum(is.na(dt$occupation))/length(dt$occupation)
dt$occupation = as.character(dt$occupation) 

## relationship

dt$relationship = as.factor(dt$relationship)
ggplot(dt, aes(x=relationship))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$relationship)
sum(is.na(dt$relationship))
dt$relationship = as.character(dt$relationship) 

## race

dt$race = as.factor(dt$race)
ggplot(dt, aes(x=race))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$race)
sum(is.na(dt$race))
dt$race = as.character(dt$race)

## sex

dt$sex = as.factor(dt$sex)
ggplot(dt, aes(x=sex))+geom_bar()+facet_grid(earning~., scales = "free")
unique(dt$sex)
sum(is.na(dt$sex))
dt$sex = as.character(dt$sex)

## capital_gain
# ez mi a szar egyáltalán
plot(dt$capital_gain, main = "capital_gain distribution", ylab = "capital_gain")
ggplot(dt, aes(x=capital_gain))+geom_histogram()+facet_grid(earning~., scales = "free")
summary(dt$capital_gain)
sum(is.na(dt$capital_gain))


## capital_loss
# ez mi a szar egyáltalán
plot(dt$capital_loss, main = "capital_loss distribution", ylab = "capital_loss")
ggplot(dt, aes(x=capital_loss))+geom_histogram()+facet_grid(earning~., scales = "free")
summary(dt$capital_loss)
sum(is.na(dt$capital_loss))

## hours_per_week

plot(dt$hours_per_week, main = "hours_per_week distribution", ylab = "hours_per_week")
ggplot(dt, aes(x=hours_per_week))+geom_histogram(binwidth = 5)+facet_grid(earning~., scales = "free")
summary(dt$hours_per_week)
sum(is.na(dt$hours_per_week))

## Native country

dt$native_country = as.factor(dt$native_country)
ggplot(dt, aes(x=native_country))+geom_bar()+facet_grid(earning~., scales = "free")
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

training_base <- dt[,-16, with = FALSE]

########### ####### ### features


########### ####### ### model building


### training and validation sets

N <- nrow(training_base)
id_tr <- sample(1:N,2*N/3)
id_va <- sample(base::setdiff(1:N, id_tr))

# chk sum(id_tr)+sum(id_va)-(32561+1)*32561/2

trainingSet <- training_base[id_tr,]
validationSet <- training_base[id_va,]

### H2O # http://localhost:54321

library(h2o)
h2o.init(max_mem_size = "1g", nthreads = -1)

#paste(names(trainingSet), collapse = ",")
for (i in c(2,4,6,7,8,9,10, 14,15)) {
  trainingSet[[i]]<-as.factor(trainingSet[[i]])
  validationSet[[i]]<-as.factor(validationSet[[i]])
  test.adult.df[[i]]<-as.factor(test.adult.df[[i]])
}
test.adult.df[[1]]<-as.numeric(test.adult.df[[1]])

str(trainingSet)
str(test.adult.df)
  


### models
# random forest

dtTr <- as.h2o(trainingSet)
dtTr$earning <- as.factor(dtTr$earning)
dtVa <- as.h2o(validationSet)
dtVa$earning <- as.factor(dtVa$earning)
dtTe <- as.h2o(test.adult.df)
dtTe$earning <- as.factor(dtTe$earning)

system.time({
  md <- h2o.randomForest(x = seq(ncol(dtTr) - 1), y = ncol(dtTr), 
                         training_frame = dtTr, 
                         mtries = -1, ntrees = 500, max_depth = 20, nbins = 200)
})
md

h2o.auc(md) 
h2o.auc(h2o.performance(md, dtTe))

# gmb
#for (i in c(2,4,6,7,8,9,10, 14,15)) {
#  dtVa[[i]]<-as.factor(dtVa[[i]])
#  dtTe[[i]]<-as.factor(dtTe[[i]])
#}

system.time({
  md <- h2o.gbm(x = seq(ncol(dtTr) - 1), y = ncol(dtTr), 
                training_frame = dtTr, validation_frame = dtVa,
                max_depth = 15, ntrees = 500, learn_rate = 0.01, nbins = 200,
                stopping_rounds = 3, stopping_tolerance = 1e-3)
})
md

h2o.auc(md)
h2o.auc(h2o.performance(md, dtTe))






############### sandbox
factorx <- factor(cut(dt$native_country, breaks=unique(dt$native_country)))
str(factorx)
factor(dt$native_country)
data.frame(table(dt$native_country))


ggplot(dt) + geom_histogram(aes(x = education_num)) +
  facet_grid(earning~., scales = "free") + scale_x_log10()

sa<-sample(1:100, 50)
