#Load packages
library(ggplot2)
library(gridExtra)
library(DMwR)
library(DataCombine)
library(corrgram)
library(C50)
library(caret)
library(randomForest)
library(inTrees)
library(class)
library(e1071)

#Set Working directory
setwd("C:/Users/Kiran/Desktop/New")

#Read Datasets
TestData = read.csv("Test_data.csv")
TrainData = read.csv("Train_data.csv")

#Look at data structure
str(TestData)
str(TrainData)

########## Exploratory Data Analysis ##########

###Univariate Analysis

##Churn distribution w.r.t state in TrainData & TestData
bar_state1 = ggplot(TrainData, aes(state, fill = Churn)) + geom_bar() + ggtitle("TrainData - Churn w.r.t state")
bar_state2 = ggplot(TestData, aes(state, fill = Churn)) + geom_bar() + ggtitle("TestData - Churn w.r.t state")
grid.arrange(bar_state1, bar_state2, top = "Churn w.r.t state in TrainData & TestData")

##Churn distribution w.r.t area code in TrainData & TestData
bar_ac1 = ggplot(TrainData, aes(area.code, fill = Churn)) + geom_bar() + ggtitle("TrainData - Churn w.r.t state")
bar_ac2 = ggplot(TestData, aes(area.code, fill = Churn)) + geom_bar() + ggtitle("TestData - Churn w.r.t state")
grid.arrange(bar_ac1, bar_ac2, ncol = 2, nrow = 1, top = "Churn w.r.t area code in TrainData & TestData")

##Churn distribution w.r.t international plan in TrainData & TestData
bar_ip1 = ggplot(TrainData, aes(international.plan, fill = Churn)) + geom_bar(alpha = 1) + ggtitle("TrainData - Churn w.r.t international.plan")
bar_ip2 = ggplot(TestData, aes(international.plan, fill = Churn)) + geom_bar(alpha = 1) + theme(legend.position = "left") + ggtitle("TestData - Churn w.r.t international.plan")
grid.arrange(bar_ip1, bar_ip2, ncol = 2, nrow = 1, top = "Churn w.r.t international.plan in TrainData & TestData")

##Churn distribution w.r.t voice mail plan in TrainData & TestData
bar_vmp1 = ggplot(TrainData, aes(voice.mail.plan, fill = Churn)) + geom_bar(alpha = 1) + ggtitle("TrainData - Churn w.r.t voice.mail.plan")
bar_vmp2 = ggplot(TestData, aes(voice.mail.plan, fill = Churn)) + geom_bar(alpha = 1) + theme(legend.position = "left") + ggtitle("TestData - Churn w.r.t voice.mail.plan")
grid.arrange(bar_vmp1, bar_vmp2, ncol = 2, nrow = 1, top = "Churn w.r.t voice.mail.plan in TrainData & TestData")


#Churn distribution across TrainData & TestData
dens1 = ggplot(TrainData, aes(Churn, fill = Churn)) + geom_density(alpha = 1) + ggtitle("TrainData Churn")
dens2 = ggplot(TestData, aes(Churn, fill = Churn)) + geom_density(alpha = 1) + theme(legend.position = "left") + ggtitle("TestData Churn")
grid.arrange(dens1, dens2, ncol = 2, nrow = 1, top = "Churn Distribution across TrainData & TestData")
#Churn is equally distributed across TrainData & TestData

#Churn distribution w.r.t account.length in TrainData & TestData
hist_al1 = ggplot(TrainData, aes(account.length, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t accont.length")
hist_al2 = ggplot(TestData, aes(account.length, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t account.length")
grid.arrange(hist_al1, hist_al2, ncol = 2, nrow = 1, top = "Churn w.r.t account.length in TrainData & TestData")

#Churn distribution w.r.t number.vmail.messages in TrainData & TestData
hist_nvm1 = ggplot(TrainData, aes(number.vmail.messages, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t number.vmail.messages")
hist_nvm2 = ggplot(TestData, aes(number.vmail.messages, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t number.vmail.messages")
grid.arrange(hist_nvm1, hist_nvm2, ncol = 2, nrow = 1, top = "Churn w.r.t number.vmail.messages in TrainData & TestData")

#Churn distribution w.r.t total.day.minutes in TrainData & TestData
hist_tdm1 = ggplot(TrainData, aes(total.day.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.day.minutes")
hist_tdm2 = ggplot(TestData, aes(total.day.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.day.minutes")
grid.arrange(hist_tdm1, hist_tdm2, ncol = 2, nrow = 1, top = "Churn w.r.t total.day.minutes in TrainData & TestData")

#Churn distribution w.r.t total.day.calls in TrainData & TestData
hist_tdc1 = ggplot(TrainData, aes(total.day.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.day.calls")
hist_tdc2 = ggplot(TestData, aes(total.day.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.day.calls")
grid.arrange(hist_tdc1, hist_tdc2, ncol = 2, nrow = 1, top = "Churn w.r.t total.day.calls in TrainData & TestData")

#Churn distribution w.r.t total.day.charge in TrainData & TestData
hist_tdch1 = ggplot(TrainData, aes(total.day.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.day.charge")
hist_tdch2 = ggplot(TestData, aes(total.day.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.day.charge")
grid.arrange(hist_tdch1, hist_tdch2, ncol = 2, nrow = 1, top = "Churn w.r.t total.day.charge in TrainData & TestData")

#Churn distribution w.r.t total.eve.minutes in TrainData & TestData
hist_tem1 = ggplot(TrainData, aes(total.eve.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.eve.minutes")
hist_tem2 = ggplot(TestData, aes(total.eve.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.eve.minutes")
grid.arrange(hist_tem1, hist_tem2, ncol = 2, nrow = 1, top = "Churn w.r.t total.eve.minutes in TrainData & TestData")

#Churn distribution w.r.t total.eve.calls in TrainData & TestData
hist_tec1 = ggplot(TrainData, aes(total.eve.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.eve.calls")
hist_tec2 = ggplot(TestData, aes(total.eve.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.eve.calls")
grid.arrange(hist_tec1, hist_tec2, ncol = 2, nrow = 1, top = "Churn w.r.t total.eve.calls in TrainData & TestData")

#Churn distribution w.r.t total.eve.charge in TrainData & TestData
hist_tech1 = ggplot(TrainData, aes(total.eve.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.eve.charge")
hist_tech2 = ggplot(TestData, aes(total.eve.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.eve.charge")
grid.arrange(hist_tech1, hist_tech2, ncol = 2, nrow = 1, top = "Churn w.r.t total.eve.charge in TrainData & TestData")

#Churn distribution w.r.t total.night.minutes in TrainData & TestData
hist_tnm1 = ggplot(TrainData, aes(total.night.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.night.minutes")
hist_tnm2 = ggplot(TestData, aes(total.night.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.night.minutes")
grid.arrange(hist_tnm1, hist_tnm2, ncol = 2, nrow = 1, top = "Churn w.r.t total.night.minutes in TrainData & TestData")

#Churn distribution w.r.t total.night.calls in TrainData & TestData
hist_tnc1 = ggplot(TrainData, aes(total.night.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.night.calls")
hist_tnc2 = ggplot(TestData, aes(total.night.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.night.calls")
grid.arrange(hist_tnc1, hist_tnc2, ncol = 2, nrow = 1, top = "Churn w.r.t total.night.calls in TrainData & TestData")

#Churn distribution w.r.t total.night.charge in TrainData & TestData
hist_tnch1 = ggplot(TrainData, aes(total.night.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.night.charge")
hist_tnch2 = ggplot(TestData, aes(total.night.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.night.charge")
grid.arrange(hist_tnch1, hist_tnch2, ncol = 2, nrow = 1, top = "Churn w.r.t total.night.charge in TrainData & TestData")

#Churn distribution w.r.t total.intl.minutes in TrainData & TestData
hist_tim1 = ggplot(TrainData, aes(total.intl.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.intl.minutes")
hist_tim2 = ggplot(TestData, aes(total.intl.minutes, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.intl.minutes")
grid.arrange(hist_tim1, hist_tim2, ncol = 2, nrow = 1, top = "Churn w.r.t total.intl.minutes in TrainData & TestData")

#Churn distribution w.r.t total.intl.calls in TrainData & TestData
hist_tic1 = ggplot(TrainData, aes(total.intl.calls, fill = Churn)) + geom_histogram(bins = 30) + ggtitle("TrainData - Churn w.r.t total.intl.calls")
hist_tic2 = ggplot(TestData, aes(total.intl.calls, fill = Churn)) + geom_histogram(bins = 30) + ggtitle("TestData - Churn w.r.t total.intl.calls")
grid.arrange(hist_tic1, hist_tic2, ncol = 2, nrow = 1, top = "Churn w.r.t total.intl.calls in TrainData & TestData")

#Churn distribution w.r.t total.intl.charge in TrainData & TestData
hist_tich1 = ggplot(TrainData, aes(total.intl.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t total.intl.charge")
hist_tich2 = ggplot(TestData, aes(total.intl.charge, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t total.intl.charge")
grid.arrange(hist_tich1, hist_tich2, ncol = 2, nrow = 1, top = "Churn w.r.t total.intl.charge in TrainData & TestData")

#Churn distribution w.r.t number.customer.service.calls in TrainData & TestData
hist_ncsc1 = ggplot(TrainData, aes(number.customer.service.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TrainData - Churn w.r.t number.customer.service.calls")
hist_ncsc2 = ggplot(TestData, aes(number.customer.service.calls, fill = Churn)) + geom_histogram(bins = 10) + ggtitle("TestData - Churn w.r.t number.customer.service.calls")
grid.arrange(hist_ncsc1, hist_ncsc2, ncol = 2, nrow = 1, top = "Churn w.r.t number.customer.service.calls in TrainData & TestData")

#keep only TrainData & TestData for further processing
rmExcept(c("TrainData", "TestData"))

#Save all categorical variables into Cat_Var
Cat_Var = c('state', 'area.code', 'phone.number',
            'international.plan', 'voice.mail.plan', 'Churn')

#Assign levels to all factor variables
for (i in Cat_Var) {
  TestData[,i] = factor(TestData[,i], labels = (1:length(levels(factor(TestData[,i])))))
  TrainData[,i] = factor(TrainData[,i], labels = (1:length(levels(factor(TrainData[,i])))))
  
}

#Save all the continuous variables in Con_Var
Con_Var = c('account.length', 'number.vmail.messages',
            'total.day.minutes', 'total.day.calls', 'total.day.charge',
            'total.eve.minutes', 'total.eve.calls', 'total.eve.charge',
            'total.night.minutes', 'total.night.calls', 'total.night.charge',
            'total.intl.minutes', 'total.intl.calls', 'total.intl.charge',
            'number.customer.service.calls')

#Loop to convert into numeric
for (i in Con_Var) {
  TestData[,i] = as.numeric(TestData[,i])
  TrainData[,i] = as.numeric(TrainData[,i])
  
}



#### Missing Value Analysis ####

sum(is.na(TrainData))
sum(is.na(TestData))
#No Missing values

#### Outlier Analysis ####
#Keep a copy of datasets
TrainData1 = TrainData
TestData1 = TestData
# TrainData = TrainData1
# TestData = TestData1

#Loop to get grids of boxplots of "Churn" vs. rest of Con_Var
for (i in 1:length(Con_Var)) {
  assign(paste0("gn",i), ggplot(aes_string(x = "Churn", y = (Con_Var[i])), data = TrainData)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=20,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x="Churn", y=Con_Var[i])+
           ggtitle(paste("Box plot of Churn in TrainData for",Con_Var[i])))
}

###Plotting grids of boxplots together
grid.arrange(gn1,gn2,gn3,gn4,ncol=2)
grid.arrange(gn5,gn6,gn7,gn8,ncol=2)
grid.arrange(gn9,gn10,gn11,gn12,ncol=2)
grid.arrange(gn13,gn14,gn15,ncol=3)
 
# #Loop to remove outliers
# for (i in Con_Var) {
#   print(i)
#   val = TrainData[,i][TrainData[,i] %in% boxplot.stats(TrainData[,i])$out]
#   print(length(val))
#   val1 = TestData[,i][TestData[,i] %in% boxplot.stats(TestData[,i])$out]
#   print(length(val1))
#   ChurnData = ChurnData[which(!ChurnData[,i] %in% val),]
# 
# }

# #1080 outliers in total
 
#Loop to impute outliers with NA 
#and replace them with respective mean & median values
#  
# for (i in Con_Var) {
#   val = TrainData[,i][TrainData[,i] %in% boxplot.stats(TrainData[,i])$out]
#   TrainData[,i][TrainData[,i] %in% val] = NA
#   val1 = TestData[,i][TestData[,i] %in% boxplot.stats(TestData[,i])$out]
#   TestData[,i][TestData[,i] %in% val1] = NA

  #By Mean
  # TrainData[,i][is.na(TrainData[,i])] = mean(TrainData[,i], na.rm = T)
  # TestData[,i][is.na(TestData[,i])] = mean(TestData[,i], na.rm = T)

  #By Median
  # TrainData[,i][is.na(TrainData[,i])] = median(TrainData[,i], na.rm = T)
  # TestData[,i][is.na(TestData[,i])] = median(TestData[,i], na.rm = T)
# }
# 
# #knnImputation method
# TrainData = knnImputation(TrainData, k = 3)
# TestData = knnImputation(TestData, k = 3)


#Capping the outliers
#Function that calculates quantiles and caps
# Cap_Outlier = function(x) {
#   qnt = quantile(x, probs = c(0.25, 0.75), na.rm = T)
#   caps = quantile(x, probs = c(0.05, 0.95), na.rm = T)
#   R = 1.5 * IQR(x, na.rm = T)
#   x[x < (qnt[1] - R)] = caps[1]
#   x[x > (qnt[2] + R)] = caps[2]
#   return (x)
# }
#Loop to apply Cap_Outlier function
# for (i in Con_Var){
#   TrainData[,i] = Cap_Outlier(TrainData[,i])
#   TestData[,i] = Cap_Outlier(TestData[,i])
# }


#Check IQR of Con_Var
for (i in Con_Var) {
  print(paste("IQR of", i, "in TrainData", IQR(TrainData[,i])))
  print(paste("IQR of", i, "in TestData", IQR(TestData[,i])))
}
#number.vmail.messages in both the datasets is zero-inflated
#ways to deal with it, introducing new outliers
#so Outlier analysis is skipped
########## Feature Selection ##########

rmExcept(c("TrainData", "TestData", "Cat_Var", "Con_Var"))

# Correlation Analysis
corrgram(TrainData[,Con_Var], order = F, upper.panel = panel.pie,
         text.panel = panel.txt, main = "Correlation Plot for TrainData")


#Chi-Squared test of independence
Cat_Var_Chi = c('state', 'area.code', 'phone.number', 'international.plan', 'voice.mail.plan')
for (i in Cat_Var_Chi) {
  print(i)
  print(chisq.test(table(TrainData$Churn,TrainData[,i])))
}

# Dimensionality Reduction
TrainData = subset(TrainData, select =  -c(area.code, phone.number,
                                           total.day.charge, total.eve.charge,
                                           total.night.charge, total.intl.charge))

TestData = subset(TestData, select =  -c(area.code, phone.number,
                                         total.day.charge, total.eve.charge,
                                         total.night.charge, total.intl.charge))

########## Feature Scaling ##########
cnames = c("account.length", "number.vmail.messages", "total.day.minutes",
           "total.day.calls","total.eve.minutes", "total.eve.calls",
           "total.night.minutes","total.night.calls","total.intl.minutes",
           "total.intl.calls","number.customer.service.calls")

#Normalisation
for (i in cnames) {
  TrainData[,i] = (TrainData[,i] - min(TrainData[,i]))/(max(TrainData[,i]) - min(TrainData[,i]))
  TestData[,i] = (TestData[,i] - min(TestData[,i]))/(max(TestData[,i]) - min(TestData[,i]))
}

#Normality check for a few variables
hist(TrainData$account.length)
hist(TrainData$number.vmail.messages)
hist(TrainData$total.day.minutes)
hist(TrainData$total.day.calls)

########## Modelling ##########

##Define EvaluationMatrix to calculate error metrics
EvaluationMatrix <- function(conf_Mat){
  TN = conf_Mat[1,1]
  FN = conf_Mat[2,1]
  FP = conf_Mat[1,2]
  TP = conf_Mat[2,2]
  
  # Accuracy
  Accuracy = ((TN + TP) * 100) / (TN + TP + FN + FP)
  
  # False Negative Rate
  FNR = (FN / (FN + TP)) * 100
  return(c(Accuracy, FNR))
}


## Decision Tree Classifier
#Developing model using TrainData
DT_Cls = C5.0(Churn ~., TrainData, trials = 25, rules = T)
#Check summary of the model
summary(DT_Cls)
#Predict test cases
DT_Cls_Predict = predict(DT_Cls, TestData[,-15], type = "class")
#Build Confusion Matrix
Conf_Mat_DT = table(observed = TestData$Churn, predicted = DT_Cls_Predict)
#Error metrics
EvaluationMatrix(Conf_Mat_DT)

## Random Forest Classifier 
#Developing model on TrainData
RF_Cls = randomForest(Churn ~., TrainData, importance = T, ntree = 500)

##Extract rules fromn RF
#transform RF object to an "inTrees" format
treeList = RF2List(RF_Cls)
 
#Extract rules
Ext_Rules = extractRules(treeList, TrainData[,-15])
 
#Visualize some rules
Ext_Rules[1:2,]
 
#Make rules more readable
ReadRules = presentRules(Ext_Rules,colnames(TrainData))
ReadRules[1:2,]
 
# #Get rule metrics
ruleMetric = getRuleMetric(Ext_Rules, TrainData[,-15], TrainData$Churn)

# #Evaulate few rules
ruleMetric[1:2,]

#Predict test cases
RF_Cls_Predict = predict(RF_Cls, TestData[,-15])
#Build Confusion Matrix
Conf_Mat_RF = table(observed = TestData$Churn, predicted = RF_Cls_Predict)
#Error metrics
EvaluationMatrix(Conf_Mat_RF)

## KNN - Classifier 
#Classification cum Prediction
KNN_Cls_Predict = knn(TrainData[,1:14], TestData[,1:14], TrainData$Churn, k = 3)
#Build Confusion Matrix
Conf_Mat_KNN = table(observed = TestData$Churn, predicted = KNN_Cls_Predict)
#Error metrics
EvaluationMatrix(Conf_Mat_KNN)

## Naive Bayes Classifier
#Developing model on TrainData
NB_Cls = naiveBayes(Churn ~ ., TrainData)
#Predict test cases
NB_Cls_Predict = predict(NB_Cls, TestData[,-15], type = "class")
#Build Confusion Matrix
Conf_Mat_NB = table(observed = TestData$Churn, predicted = NB_Cls_Predict)
#Error metrics
EvaluationMatrix(Conf_Mat_NB)



## Logistic Regression
#Developing model on TrainData
Log_Cls = glm(Churn ~., TrainData, family = "binomial")
#Summary of the model
summary(Log_Cls)
#Predict test cases
Log_Cls_Predict = predict(Log_Cls, newdata =  TestData[,-15], type = "response")
#Categorise probabilities
Log_Cls_Predict = ifelse(Log_Cls_Predict>0.5, 1, 0)
#Build Confusion Matrix
Conf_Mat_Log = table(observed = TestData$Churn, predicted = Log_Cls_Predict)
#Error metrics
EvaluationMatrix(Conf_Mat_Log)

