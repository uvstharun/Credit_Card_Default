rm( list=ls() ) # remove all existing objects in the environment
gc() # garbage collection


install.packages("dplyr")
install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.2.2.tar.gz", repo=NULL, type="source")
install.packages("tidyverse")
install.packages("conflicted")
install.packages("caret")
install.packages("lattice")
install.packages("ggvis")
install.packages("MLmetrics")
install.packages("scales")
install.packages("precrec")
install.packages("patchwork")
install.packages("MASS")
install.packages("dplyr")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(ggvis)
library(MLmetrics) 
library(precrec) 
library(rpart)  
library(rpart.plot)
library(scales)
library(patchwork)
library(MASS)
library(dplyr)

# Obtain the data
set.seed(1234)
#setwd('/Users/queeny.mathayas/Downloads/UCI_Credit_Card')
data = read.csv('credit_card_default.csv', header=TRUE)
head(data)

#deleting the first row 
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]
head(data)
view(data)
ncol(data)

#converting data into numeric 
data[, 1:25] <- sapply(data[, 1:25], as.numeric)
str(data) #to view the strcture of the data 

#changing the name of the default payment column to avoid confusion 
colnames(data)[colnames(data)=="default payment next month"] <- "default.payment.next.month"

creditcard = data 
cat('The dimention is: ', dim(creditcard)) #checking dimention 
cat('Is there any NA data: ', any(is.na(creditcard))) #checking the if there are any NA values 

head(creditcard)
#checking the length and see which variables can be converted to factors
apply(creditcard,2, function(x) length(unique(x)))


ncol(data)

#Data Analysis 
#In the analysis will address the following question 
#probability of default payment affected by different categories of demographic variables
#is there a relationship between a person's payment behavior in the past six months and their subsequent payments
#What are the most influential variables in predicting default payment

factor_vars <- c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
creditcard_f = creditcard
creditcard_f[factor_vars] <- lapply(creditcard_f[factor_vars], function(x) as.factor(x)) #converting variables into factors 



ncol(data)

#demographic features 

# Step 2. Check the distribution of continuous variables by histogram and/or boxplot.
# Delete outliers if there are points very isolated from the others.

# Get some plots of the data set
CreditCard_plot = creditcard

CreditCard_plot$LIM_cut = cut(as.numeric(as.character(CreditCard_plot$LIMIT_BAL)), 
                              c((0:8)*130000), right = FALSE, 
                              labels = c("0-130K", "130K-260K", "260K-390K", 
                                         "390K-520K", "520K-650K", "650K-780K",
                                         "780K-910K", "910K-1040K")) # Categorize LIMIT_BAL

CreditCard_plot$Age_cut = cut(as.numeric(as.character(CreditCard_plot$AGE)), 
                              c(seq(20,80,10)), right = FALSE) # Categorize Defualt Rate

# Convert format
CreditCard_plot$default.payment.next.month = as.character(CreditCard_plot$default.payment.next.month)
CreditCard_plot$EDUCATION = as.character(CreditCard_plot$EDUCATION)


# Plot 1 --------------------------------------------------------------------------
ggplot(data=CreditCard_plot, aes(LIM_cut, Age_cut)) + 
  geom_bar(stat = "identity", aes(fill = Age_cut), position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs( title = "Age and Limit Balance Catagories", 
        x = "Limit Balance Catagories (NTD)", 
        y = "Age Catagories (Years)")

## Comment: Middle ages have higher amount of given credit in NT dollars

# Plot 2 --------------------------------------------------------------------------
# The original dataset in default.payment.next.month column shows 0 and 1, 
# which mean credit card does not default and do default respectively. 
# It is not so clear for someone who does not know this dataset, 
# so I changed the labels of default.payment.next.month on the plot
# First, given a list for converting the labels 

default_names = list('0' ="No Default", '1'= "Default")

# Then, define a labeller to convert labels of default.payment.next.month
default_labeller = function(variable,value) {
  return(default_names[value])
}

ggplot(data=CreditCard_plot, aes(x=AGE)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(default.payment.next.month ~., labeller=default_labeller) +
  geom_vline(data=CreditCard_plot, aes(xintercept=mean(AGE, na.rm=T)), 
             linetype="dashed", size=1, colour="red") +
  labs(title = "Histogram of Age and Credit Card Default",
       x = "Age (Years)",
       y = "Count", tag = "B")

## Comment: The bar charts shows it is lower percentage of credit card default 
## For people between age 25 and age 40. Also, most of the clients are 
## between age 25 and age 35.

# Plot 3 --------------------------------------------------------------------------
CreditCard_plot2 = CreditCard_plot
CC_Default = CreditCard_plot$default.payment.next.month
CreditCard_plot2$default.payment.next.month[CC_Default=="1"] = "Default"
CreditCard_plot2$default.payment.next.month[CC_Default=="0"] = "No Default"
CreditCard_plot2$SEX[CreditCard_plot$SEX=="1"] = "Male"
CreditCard_plot2$SEX[CreditCard_plot$SEX=="2"] = "Female"

ggplot(data=CreditCard_plot2, aes(x=AGE)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(default.payment.next.month ~SEX) +
  geom_vline(data=CreditCard_plot2,
             aes(xintercept=mean(AGE, na.rm=T)), 
             linetype="dashed", size=1, colour="red") +
  labs(title = "Histogram of Age, Gender, and Credit Card Default", 
       x = "Age (Years)",
       y = "Count", 
       tag = "C")

# Plot 4 --------------------------------------------------------------------------
ggplot(data=CreditCard_plot, aes(x=LIMIT_BAL, colour=default.payment.next.month)) +
  stat_density(geom="line",position="identity") + 
  stat_density(geom="line", aes(color = "default.payment.next.month")) +
  labs(title = "Density of Limit Balance and Credit Card Default", 
       x = "Limit Balance (NTD)",
       y = "Density", 
       tag = "D") + 
  scale_colour_discrete(name="Default", 
                        breaks=c("0", "1", "default.payment.next.month"),
                        labels=c("No", "Yes", "All (Yes and No)"))

## Comment: Light blue line, which represents the density of credit card default, 
## has a high peak at limit balance about 10000 NTD. It might tell us 
## that credit card might be too easy to approve without careful 
## considerations of applicants' credit score.

# Plot 5 --------------------------------------------------------------------------
ggplot(data=CreditCard_plot, aes(MARRIAGE, fill = default.payment.next.month)) +
  labs(title = "Stacked Bar Chart of Marital Status and Credit Card Default",
       subtitle = (" 0:Missing;1: Married; 2:Single; 3: Others"),
       x = "Marital status", y = "Density", tag = "E") + 
  geom_histogram(bins = 7) + 
  scale_fill_discrete(name="Default", breaks=c("0", "1"), labels=c("No", "Yes"))

## Comment: Most of cardholders were married or single. 
## Single had less probabilities of credit card default by percentage.

# Plot 6 --------------------------------------------------------------------------
edu_count_table = as.data.frame(table(CreditCard_plot$EDUCATION))
edu_count_table$Prob = edu_count_table$Freq / sum(edu_count_table$Freq)

colnames(edu_count_table) = c("Edu", "Freq", "Prob" )

ggplot(data=edu_count_table, aes(x="", y=Prob, fill=Edu)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  labs(title = "Pie Chart of Eduction Level", 
       y = "Probability", 
       tag = "F") + 
  scale_fill_discrete(name="Education Level", 
                      breaks=seq(0,6), 
                      labels=c("Missing Data", "Graduate", "University", "High school", "Others", "Unknown", "Unknown"))
## Comment: More than two-third cardholders have Bachelor's degree.

# Get default data
default = creditcard[creditcard$default.payment.next.month == 1,]
summary(default)

# Define function to calculate default rate based on one factor
DefaultRate = function(tab){
  
  names = colnames(tab) # Get column names of table
  N = 2:6 # Get LIMIT_BAL, SEX, EDUCATION, MARRIAGE, AGE
  DefaultRateList = list() # Initialize
  
  for ( i in 1:length(N)){
    
    factor = names[N[i]]
    fre_cc = as.data.frame(table(creditcard[factor]))
    fre_de = as.data.frame(table(default[factor]))
    
    # Left join
    fre_table = merge(fre_cc, fre_de, by.x=factor, by.y=factor, all.x=TRUE) 
    
    fre_table[is.na(fre_table)] = 0 # Replace NA as 0
    colnames(fre_table) = c(factor, 'AllData', 'Default')
    
    # Get the default rate and count of no default
    fre_table$NoDefault = fre_table$AllData - fre_table$Default 
    fre_table$Rate = fre_table$Default / fre_table$AllData 
    DefaultRateList[[i]] = as.matrix(fre_table)
    
  }
  
  return(DefaultRateList)
}

DefaultRateMat = DefaultRate(creditcard)

# Plot 7 --------------------------------------------------------------------------
# Extract LIMIT_D_R default rate
LimitBal_D_R = as.data.frame(DefaultRateMat[[1]])

LimitBal_D_R$LIM_cut = cut(as.numeric(as.character(LimitBal_D_R$LIMIT_BAL)), 
                           c((0:8)*130000), right = FALSE, 
                           labels = c("0-130K", "130K-260K", "260K-390K",
                                      "390K-520K", "520K-650K", "650K-780K",
                                      "780K-910K", "910K-1040K")) # Categorize LIMIT_BAL

LimitBal_D_R$Rate_cut = cut(as.numeric(as.character(LimitBal_D_R$Rate)), 
                            c(seq(0,1.1,0.1)), right = FALSE) # Categorize Defualt Rate

# Visualize the default of LIMIT_BAL
LimitBal_D_R$Rate = as.numeric(as.character(LimitBal_D_R$Rate))
LimitBal_D_R$LIMIT_BAL = as.numeric(as.character(LimitBal_D_R$LIMIT_BAL))

ggplot(data=LimitBal_D_R, aes(x=LIMIT_BAL, y=Rate, color=Rate_cut)) + 
  geom_point() + 
  labs(title = "Probability of Default Rate Based on Limit Balance", 
       x = "Limit Balance (NTD)", 
       y = "Probability", 
       tag = "G") + 
  scale_colour_discrete(name="Categorize Default Rate", 
                        labels=c("Less than 10%", "10%-20%", "20%-30%", 
                                 "30%-40%", "40%-50%", "50%-60%","More than 60%"))

## Comment: The average of default rate is relatively low 
## When the balance limit is between 250,000 and 500,000. 
## The volatility of default rate is relatively high 
## when the balance limit is between 500,000 and 750,000.

# Plot 8 --------------------------------------------------------------------------
Edu_D_R = as.data.frame(DefaultRateMat[[3]])
Edu_D_R$Rate = as.numeric(as.character(Edu_D_R$Rate))

Edu_D_R$EDUCATION = c("MissingData", "Graduate", "University", 
                      "HighSchool", "Others", "Unknown1", "Unknown2")

Edu_D_R_new = Edu_D_R[,c(1,3,4,5)] %>% gather(DefaultYN, Count, Default:NoDefault)

Edu_D_R_new$Count = as.numeric(as.character(Edu_D_R_new$Count))

ggplot(data=Edu_D_R_new, aes(x=EDUCATION, y=Count, fill=DefaultYN)) +
  geom_bar(colour="black", 
           stat="identity", 
           position=position_dodge()) +
  geom_text(aes(label=Count),
            position=position_dodge(width=0.9), 
            vjust=-0.25, 
            size=3) +
  labs(title = "Count of Default and Non-Default Based on Education Level", 
       x = "Education Level", 
       y = "Count ", 
       tag = "H") + 
  theme_minimal() + 
  scale_fill_brewer(palette="Paired", name="Default Yes or No", labels=c("Yes", "No")) 

## Comment: Most of the cardholders have bachelor's degree or master's degree.

ggplot(data=Edu_D_R, 
       aes(x=EDUCATION, y=Rate)) + 
  geom_jitter(aes(x=EDUCATION,
                  y=Rate)) +
  labs(title = "Scatter Plot of Default Rate and Education Level", 
       x = "Education Level", 
       y = "Count ", 
       tag = "I")

## Comment: Clients with higher education level has lower default rate.

# Plot 9 --------------------------------------------------------------------------
Age_D_R = as.data.frame(DefaultRateMat[[5]])
# Convert format to numeric
for (i in 1:length(Age_D_R)){
  Age_D_R[,i] = as.numeric(as.character(Age_D_R[,i]))
}

ggplot(Age_D_R, aes(x=AGE, y=AllData, color=Rate)) + 
  geom_point(aes(size=Rate)) +
  labs(title = "Level of Default Rate Baesd on Age", 
       x = "Age (Years)", 
       y = "Count of All Data", 
       tag = "J") + 
  theme_minimal()

## Comment: More people have credit card between age 22 and age 40. 
##The variance is higher for clients older than 60.

#There are 24 factors against amount of given credit. In order to aviod overfitting, I selected the 

# Delete outliers if there are points very isolated from the others.
# Remove rows with Education=0,5,6 and MARRIAGE=0,3 and LIMIT_BAL,SEX,AGE=0
#without0 = apply(creditcard,1, function(x) all(x[2:6]!=0) && x[4]!=5 && x[4]!=6 && x[5]!=3)
#creditcard = creditcard[without0,]


#based on the assumptions we created new features.
#age group
creditcard =creditcard%>%
  mutate(age_group = case_when(AGE<=25~1,   
                               AGE<=45~0,
                               AGE>45~1))


ncol(creditcard)
str(creditcard)


#frequency of delays
creditcard = creditcard%>%
  mutate(fre_delay = if_else(PAY_0>0,1,0)+
           if_else(PAY_2>0,1,0)+
           if_else(PAY_3>0,1,0)+
           if_else(PAY_4>0,1,0)+
           if_else(PAY_6>0,1,0)+
           if_else(PAY_5>0,1,0))




ncol(creditcard)
#is_ever_delay
creditcard =creditcard%>%
  mutate(is_ever_delay = if_else(fre_delay>0,1,0) )


ncol(creditcard)
str(creditcard)


#BILL_AMT1_0
creditcard =creditcard%>%
  mutate(BILL_AMT1_0 = if_else(BILL_AMT1<=0,0,1))

ncol(creditcard)
str(creditcard)


#new features
new_vars <- c('age_group','fre_delay','is_ever_delay','BILL_AMT1_0')

creditcard_f =cbind(creditcard_f,creditcard[new_vars])
creditcard_f[new_vars] <- lapply(creditcard_f[new_vars], function(x) as.factor(x))

#New update
#change code: Remove default.payment.next.month from corr check

#correlation with default_payment_next_month sort by absolute value.
corr1 = cor(data)[,25]%>%abs()%>%sort(decreasing = TRUE) 
corr2 = cor(creditcard)[,25]%>%abs()%>%sort(decreasing = TRUE)

#printing a list of the top 10 variables with the highest correlation values with 'default_payment_next_month'.
print(corr1[2:25]) #old variables

dim(data)

print(corr2[2:29]) #new variables

# add code: To check if correlation >0.8

vars_name = names(corr2[2:29]) 

creditcard <- creditcard[, c("default.payment.next.month", vars_name)]

#Training and Testing 

ncol(creditcard)
head(creditcard)
n= nrow(creditcard)


trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = creditcard[trainIndex ,]  
dim(train)
test = creditcard[-trainIndex ,]
dim(test)
ncol(train)
ncol(test)

#Prediction performance metrics 
predict_perform = function(data,pD){
  cat("Accuracy is ",Accuracy(y_pred = pD, y_true = data$default.payment.next.month),'\n') 
  cat("Recall is ", Recall(y_pred = pD, y_true = data$default.payment.next.month,positive = '1'),'\n' )
  cat("Precision is",Precision(y_pred = pD, y_true = data$default.payment.next.month,positive = '1'),'\n')
}

#Prediction performance plots
predict_perform_p = function(data,pD){
  precrec_obj <- evalmod(labels = data$default.payment.next.month, scores = as.numeric(pD))
  autoplot(precrec_obj)
}

#plot variable importance 
var_imp =function(model){
  v=varImp(model)
  print(v)
  v$var = rownames(v)
  v %>% ggvis(~var, ~Overall) %>% layer_bars()
}

#Logistic Regression: full model
lrModel0 = glm(default.payment.next.month ~ ., data = train, family = binomial)
print(lrModel0)
summary(lrModel0)
predictDefault0 = ifelse(predict(lrModel0, newdata = train, 
                                 type = "response")>=0.5,1,0)
summary(predictDefault0)
var_imp(lrModel0)

#Logistic Regression: a simple model
#lrModel1 = glm(default.payment.next.month ~ PAY_0+PAY_2+PAY_3+PAY_4+PAY_6, data = train, family = binomial)
#predictDefault1 = ifelse(predict(lrModel1, newdata = train, 
                                 #type = "response")>=0.5,1,0)

#Logistic Regression: a muti-variable model based on varImp 
#lrModel2 = glm(default.payment.next.month ~ PAY_0+fre_delay, data = train, family = binomial)
#predictDefault2 = ifelse(predict(lrModel2, newdata = train, 
                                 #type = "response")>=0.5,1,0)
#var_imp(lrModel2)

#compare log regression models when threshold is 0.5,0.4 & 0.3
predict_perform(train,predictDefault0)

#predict_perform(train,predictDefault1)

#predict_perform(train,predictDefault2)

#predictDefault1_1 = ifelse(predict(lrModel0, newdata = train, 
                                   #type = "response")>=0.5,1,0)
#predict_perform(train,predictDefault1_1)
#predict_perform_p(train,predictDefault1_1)

#predictDefault1_2 = ifelse(predict(lrModel1, newdata = train, 
                                   #type = "response")>=0.4,1,0)
#predict_perform(train,predictDefault1_2)
#predict_perform_p(train,predictDefault1_2)

#predictDefault1_3 = ifelse(predict(lrModel1, newdata = train, 
                                   #type = "response")>=0.3,1,0)
#predict_perform(train,predictDefault1_3)
#predict_perform_p(train,predictDefault1_3)

#---------backward selection----------
backward_model <- stepAIC(lrModel0, direction = "backward")

yhat = predict(backward_model, newdata = train, type='response')
hist(yhat)
sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

dichotomize1 = function(yhat, cutoff=.3) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}
dichotomize2 = function(yhat, cutoff=.4) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}
dichotomize3 = function(yhat, cutoff=.3) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}
yhat.class1 = dichotomize1(yhat, .5)
err1 = mean(yhat.class1 != train$default.payment.next.month) 
err1
table(yhat.class1, train$default.payment.next.month)
sen(train$default.payment.next.month, yhat.class1)
spe(train$default.payment.next.month, yhat.class1)

yhat.class2 = dichotomize2(yhat, .5)
err2 = mean(yhat.class2 != train$default.payment.next.month) 
err2
table(yhat.class2, train$default.payment.next.month)

yhat.class2 = dichotomize2(yhat, .1)
sen(train$default.payment.next.month, yhat.class2)
spe(train$default.payment.next.month, yhat.class2)
table(train$default.payment.next.month, yhat.class2)




yhat.class3 = dichotomize3(yhat, .5)
err3 = mean(yhat.class3 != train$default.payment.next.month) 
err3
table(yhat.class3, train$default.payment.next.month)
sen(train$default.payment.next.month, yhat.class3)
spe(train$default.payment.next.month, yhat.class3)

#----------forward selection-------

forward_model <- stepAIC(lrModel0, direction = "forward")

yhatf = predict(forward_model, newdata = train, type='response')
hist(yhatf)
senf = function(ytrue, yhatf) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhatf[ind.true1] )
}

spef = function(ytrue, yhatf) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhatf[ind.true0] )
}

dichotomizef1 = function(yhatf, cutoff=.5) {
  out = rep(0, length(yhatf))
  out[yhatf > cutoff] = 1
  out
}
dichotomizef2 = function(yhatf, cutoff=.4) {
  out = rep(0, length(yhatf))
  out[yhatf > cutoff] = 1
  out
}
dichotomizef3 = function(yhatf, cutoff=.3) {
  out = rep(0, length(yhatf))
  out[yhatf > cutoff] = 1
  out
}
yhat.classf1 = dichotomizef1(yhatf, .5)
errf1 = mean(yhat.classf1 != train$default.payment.next.month) 
errf1
table(yhat.classf1, train$default.payment.next.month)
senf(train$default.payment.next.month, yhat.classf1)
spef(train$default.payment.next.month, yhat.classf1)

yhat.classf2 = dichotomizef2(yhat, .5)
errf2 = mean(yhat.classf2 != train$default.payment.next.month) 
errf2
table(yhat.classf2, train$default.payment.next.month)
senf(train$default.payment.next.month, yhat.classf2)
spef(train$default.payment.next.month, yhat.classf2)


yhat.classf3 = dichotomizef3(yhat, .5)
errf3 = mean(yhat.classf3 != train$default.payment.next.month) 
errf3
table(yhat.classf3, train$default.payment.next.month)
senf(train$default.payment.next.month, yhat.class3)
spef(train$default.payment.next.month, yhat.class3)


#--------stepwise selection----------
step_model <- stepAIC(glm(default.payment.next.month ~ ., data = train, family = binomial), 
                      direction = "both", 
                      trace = FALSE)

yhats = predict(step_model, newdata = train, type='response')

hist(yhats)
sens = function(ytrue, yhats) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhats[ind.true1] )
}

spes = function(ytrue, yhats) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhats[ind.true0] )
}

dichotomize1s = function(yhats, cutoff=.5) {
  out = rep(0, length(yhats))
  out[yhats > cutoff] = 1
  out
}
dichotomize2s = function(yhats, cutoff=.4) {
  out = rep(0, length(yhats))
  out[yhats > cutoff] = 1
  out
}
dichotomize3s = function(yhats, cutoff=.3) {
  out = rep(0, length(yhats))
  out[yhats > cutoff] = 1
  out
}
yhat.class1s = dichotomize1s(yhats, .5)
err1s = mean(yhat.class1s != train$default.payment.next.month) 
err1s
table(yhat.class1s, train$default.payment.next.month)
sens(train$default.payment.next.month, yhat.class1s)
spes(train$default.payment.next.month, yhat.class1s)


yhat.class2s = dichotomize2(yhats, .5)
err2s = mean(yhat.class2s != train$default.payment.next.month) 
err2s
table(yhat.class2s, train$default.payment.next.month)
sens(train$default.payment.next.month, yhat.class2s)
spes(train$default.payment.next.month, yhat.class2s)


yhat.class3s = dichotomize3s(yhat, .5)
err3s = mean(yhat.class3s != train$default.payment.next.month) 
err3s
table(yhat.class3s, train$default.payment.next.month)
sens(train$default.payment.next.month, yhat.class3s)
spes(train$default.payment.next.month, yhat.class3s)



creditcard <- creditcard[, c("default.payment.next.month", vars_name)]

# Training and Testing 
n = nrow(creditcard)

trainIndex = sample(1:n, size = round(0.8 * n), replace = FALSE)
train = creditcard[trainIndex, ]  
test = creditcard[-trainIndex, ]

### KNN ###
## Set1.select all variables as predictors
require(class)

# Separate response variable and predictor variables in train and test datasets
# Separate response variable and predictor variables in train and test datasets
ytrain0 = train[, "default.payment.next.month"]
xtrain0 = train
xtrain0$default.payment.next.month = NULL
ytest0 = test[, "default.payment.next.month"]
xtest0 = test
xtest0$default.payment.next.month = NULL

# Run the knn() function
ypred0 = knn(xtrain0, xtest0, ytrain0, k = 3, prob = TRUE)
# Run the knn() function
ypred0 = knn(train, test, ytrain0, k = 3, prob = TRUE)

get.prob = function(x) {
  prob = attr(x, 'prob')
  cl = as.numeric(x)
  ind = which(cl == 1)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:sqrt(30000), ct = .5) {
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct ) + 1
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}


## cutoff = 0.5
#obj0_0 = knn.bestK(xtrain0, xtest0, ytrain0, ytest0, seq(1,sqrt(30000),2), .5)
#obj0_0

# Run knn.bestK() function with a larger step size
obj0_0 = knn.bestK(xtrain0, xtest0, ytrain0, ytest0, seq(1, sqrt(30000), 20), .5)
obj0_0

## rerun with the best k
ypred0_0 = knn(xtrain0, xtest0, ytrain0, k=obj0_0$k.optimal, prob=T)
table(ytest0, ypred0_0)
errknn0_0 = mean(ypred0_0!= ytest0) 
errknn0_0
senf(ytest0, ypred0_0)
spef(ytest0, ypred0_0)

##cutoff = 0.4

obj0_1 = knn.bestK(xtrain0, xtest0, ytrain0, ytest0, seq(1, sqrt(30000), 20), .4)
obj0_1

## rerun with the best k
ypred0_1 = knn(xtrain0, xtest0, ytrain0, k=obj0_1$k.optimal, prob=T)
table(ytest0, ypred0_1)
errknn0_1 = mean(ypred0_1!= ytest0) 
errknn0_1
senf(ytest0, ypred0_1)
spef(ytest0, ypred0_1)

##cutoff = 0.3

obj0_2 = knn.bestK(xtrain0, xtest0, ytrain0, ytest0, seq(1, sqrt(30000), 20), .3)
obj0_2

## rerun with the best k
ypred0_2 = knn(xtrain0, xtest0, ytrain0, k=obj0_2$k.optimal, prob=T)
table(ytest0, ypred0_2)
errknn0_2 = mean(ypred0_2!= ytest0) 
errknn0_2
senf(ytest0, ypred0_2)
spef(ytest0, ypred0_2)

# CART
# Choose parameters for the decision tree
#control_params <- rpart.control(minsplit = 20, cp = 0.001, maxdepth = 10)
#cart_model <- rpart(default.payment.next.month ~ ., data = train, method = "class", control = control_params)

# Calculate minimum error and best pruned tree
#optimal_tree <- prune(cart_model, cp = cart_model$cptable[which.min(cart_model$cptable[,"xerror"]),"CP"])
#min_error <- optimal_tree$cptable[which.min(optimal_tree$cptable[,"xerror"]),"xerror"]

# Visualize the best pruned tree
#rpart.plot(optimal_tree)

# Create a list to store the results

#knn_results <- list()
#cart_results <- list()

names(data)

# Load your dataset here
# data <- read.csv("your_data_file.csv")

# Classification Tree with rpart
fit = rpart(default.payment.next.month ~ ., method="class", data=data, minsplit=5)

# Minimum Error Tree
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')

# Best Pruned Tree
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(2) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')

## How to predict? I am taking best pruned tree as an example.
yhat = predict(pfit.bp, data, type = "class") # replace "data" by validation data if you have it
err.bp = mean(yhat != data$default.payment.next.month)

# if you want to use a cutoff not equal to 0.5
prob1 = predict(pfit.bp, data, type = "prob")[,2]
pred.class = as.numeric(prob1 > .7)
ytest = as.numeric(data$default.payment.next.month) - 1 # Be careful! Check the variable type of your outcome
err.bp.newCut = mean(pred.class != ytest)
err.bp.newCut


################################################
# Regression Tree Example
K = 10 # number of cross-validations
fit = rpart(Mileage ~ Price + Country + Reliability + Type, 
            method="anova", data=cu.summary, xval=K)

# Minimum Error Tree
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')

# Best Pruned Tree
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(K) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')

## How to predict? I am taking best pruned tree as an example.
yhat = predict(pfit.bp, cu.summary) # replace "dat" by validation data if you have it

rmse = function(x, y) sqrt( mean((x-y)^2) ) # I wrote a function to calculate RMSE
rmse(yhat, cu.summary$Price)
plot(cu.summary$Price, yhat) # check the scatter plot, not so good




