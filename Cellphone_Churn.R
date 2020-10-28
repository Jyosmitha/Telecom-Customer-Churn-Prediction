setwd("C:/Users/ammu/Desktop/Great Lakes/5. Predictive Modelling/project")
getwd()
library(readxl)
library(e1071)
library(GGally)
library(mice)
library(ROCR)
library(ineq)
library(car)
library(lmtest)
library(pan)
library(corrplot)
library(ggplot2)
library(DataExplorer)
library(reshape)
library(RColorBrewer)
library(class)
library(caTools)
library(caret)
CellphoneRawData=read_excel("Cellphone.xlsx",2)
MetaData=read_excel("Cellphone.xlsx",1)
Cellphone=read_excel("Cellphone.xlsx",2)
names(Cellphone)
dim(Cellphone)
#View(Cellphone)
#View(MetaData)
head(Cellphone)
tail(Cellphone)

#null values check
anyNA(Cellphone)
sum(is.na(Cellphone))
sum(rowSums(is.na(Cellphone)))
sum(colSums(is.na(Cellphone)))

#structure
str(Cellphone)

#summary
summary(Cellphone)

attach(Cellphone)

#Changing few columns into categorical variables
Cellphone$Churn=as.factor(Churn)
Cellphone$ContractRenewal=as.factor(ContractRenewal)
Cellphone$DataPlan=as.factor(DataPlan)

#datatypes of variables
split(names(Cellphone), sapply(Cellphone,function(x) paste(class(x), collapse="" )))

#separating Continous and categorical variables
CellphoneContinous=subset(Cellphone,select = -c(Churn,ContractRenewal,DataPlan))
CellphoneCategorical=subset(Cellphone,select = c(Churn,ContractRenewal,DataPlan))
names(CellphoneContinous)
names(CellphoneCategorical)

#creating a dataframe with continous and response variable
CellphoneContChurn=cbind(Churn,CellphoneContinous)
names(CellphoneContChurn)
#View(CellphoneContChurn)

#Univariate Analysis

#dataset overview
plot_intro(Cellphone)

#barplot for categorical variables
plot_bar(Cellphone)

#melting for ggplots
contChurnMelt=melt(CellphoneContChurn,id=c("Churn"))
contChurnMelt$Churn=as.factor(Churn)

#histogram for continous variables
gg <- ggplot(contChurnMelt, aes(value, fill=variable))
gg+geom_histogram(aes(color = value) ) +
  facet_wrap(~variable,scales = "free")

#density plot for continous
gg <- ggplot(contChurnMelt, aes(value, fill=variable))
gg+geom_density(aes(color = value) ) +
  facet_wrap(~variable,scales = "free")

#contingency table for categorical 
variables=colnames(Cellphone)
for(i in c(1,3,4))
{
  print(variables[i])
  print(table(Cellphone[i]))
  print(round(prop.table(table(Cellphone[i])),3))
}
#boxplot-Univariate
dev.off()
variables=names(Cellphone)
par(mfrow=c(1,2,4))
for (i in c(2,5:11))
  {
     boxplot(Cellphone[i]
    ,main=variables[i]
    ,xlab=variables[i])
}
#Bivariate analysis
#corrplot
dev.off()
corrplot(cor(Cellphone[-c(1,3,4)]),method="number")

#barplot for Churn vs Categoricals
dev.off()
#melting for categorical variables
CellphoneCategorical=as.data.frame(CellphoneCategorical)
catChurnMelt=melt(CellphoneCategorical, id=("Churn"))

gg<- ggplot(data=catChurnMelt,aes(x=value ))
gg+geom_bar(aes(fill=Churn,stat="count"))+
  facet_wrap(~variable,scales = "free")


qplot(DataPlan,DataUsage,data =Cellphone,geom = "boxplot" )

#boxplot for Churn vs Numerical
dev.off()
gg <- ggplot(contChurnMelt, aes(x=Churn, y=value))
gg+geom_boxplot(aes(color = Churn), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 4)+
  coord_flip()

#densityplot for Churn vs Numerical
dev.off()
gg<- ggplot(data=contChurnMelt,aes(x=value ))
gg+geom_density(aes(fill=Churn,stat="count",alpha=I(.4)))+
  facet_wrap(~variable,scales = "free")

# histogram for Churn vs Numericals
contChurnMelt$variable=as.character(contChurnMelt$variable)
dev.off()
gg<- ggplot(data=contChurnMelt,aes(x=value ))
gg+geom_histogram(aes(fill=Churn,stat="count"))+
  facet_wrap(~variable,scales = "free")

#ggpair plots for continous variables along with correlation
dev.off()
ggpairs(CellphoneContinous, ggplot2::aes(colour = as.factor(Churn)))

#multi-variate analysis

#Dataplan vs relevant Categorical variables with Churn
qplot(DataPlan,DataUsage,data =Cellphone,geom = "boxplot",
      main="Data Plan vs Data Usage", colour=Churn )
qplot(DataPlan ,DayMins,  data =Cellphone,geom = "boxplot",
      main="Data Plan vs DayMins", fill=Churn)
qplot(DataPlan ,MonthlyCharge,  data =Cellphone,geom = "boxplot",
      main="Data Plan vs MonthlyCharge", fill=Churn)

qplot(Churn,DataUsage,data =Cellphone,geom = "boxplot",
      main="Data Plan vs Data Usage ",colour=DataPlan)


#contractrenewal vs relevant Categorical variables with Churn
qplot(ContractRenewal ,DataUsage,  data =Cellphone,geom = "boxplot",
      main="ContractRenewal vs DataUsage", fill=Churn)
qplot(ContractRenewal ,DayMins,  data =Cellphone,geom = "boxplot",
      main="ContractRenewal vs DayMins", fill=Churn)
qplot(ContractRenewal ,MonthlyCharge,  data =Cellphone,geom = "boxplot",
      main="ContractRenewal vs MonthlyCharge", fill=Churn)
qplot(ContractRenewal ,RoamMins,  data =Cellphone,geom = "boxplot",
      main="ContractRenewal vs RoamMins", fill=Churn)


#checking for multicollinearity
dev.off()
corrplot(cor(CellphoneContChurn[-1]),method="number")

temp=CellphoneContChurn
names(temp)
str(temp)

#checking VIF without removing any variables
vif(lm(Churn~.,data=temp))
#removing "AccountWeeks"
vif(glm(Churn~.,data=temp[,-2]))
#removing "DataUsage"
vif(glm(Churn~.,data=temp[,-3]))
#removing "CustServeCalls"
vif(glm(Churn~.,data=temp[,-4]))
#removing "DayMins"
vif(glm(Churn~.,data=temp[,-5]))
#removing "DayCalls"
vif(glm(Churn~.,data=temp[,-6]))
#removing "MonthlyCharge"
vif(glm(Churn~.,data=temp[,-7]))
#removing "OverageFee"
vif(glm(Churn~.,data=temp[,-8]))
#removing "RoamMins"
vif(glm(Churn~.,data=temp[,-9]))

# checking significance for categorical variables
ChiSqStat=NULL
for ( i in 2 :(ncol(CellphoneCategorical))){
  Statistic <- data.frame(
    "Row" = colnames(CellphoneCategorical[1]),
    "Column" = colnames(CellphoneCategorical[i]),
    "Chi SQuare" = chisq.test(CellphoneCategorical[[1]], CellphoneCategorical[[i]])$statistic,
    "df"= chisq.test(CellphoneCategorical[[1]], CellphoneCategorical[[i]])$parameter,
    "p.value" = chisq.test(CellphoneCategorical[[1]], CellphoneCategorical[[i]])$p.value)
  ChiSqStat <- rbind(ChiSqStat, Statistic)
}
ChiSqStat <- data.table::data.table(ChiSqStat)
ChiSqStat

# checking significance for continous variables

model <- glm(Churn~AccountWeeks ,
               data = CellphoneContChurn, family = binomial)
summary(model)
#AccountWeeks not Significant

model <- glm(Churn~DataUsage ,
             data = CellphoneContChurn, family = binomial)
summary(model)

model <- glm(Churn~CustServCalls ,
             data = CellphoneContChurn, family = binomial)
summary(model)

model <- glm(Churn~DayMins ,
             data = CellphoneContChurn, family = binomial)
summary(model)

model <- glm(Churn~DayCalls ,
             data = CellphoneContChurn, family = binomial)
summary(model)
#daily calls not significant

model <- glm(Churn~MonthlyCharge ,
             data = CellphoneContChurn, family = binomial)
summary(model)

model <- glm(Churn~OverageFee ,
             data = CellphoneContChurn, family = binomial)
summary(model)

model <- glm(Churn~RoamMins ,
             data = CellphoneContChurn, family = binomial)
summary(model)

#building Logistic regression model

names(Cellphone)
str(Cellphone)
#datasplit for logistic regression
set.seed(1234)
dataSplitLG=sample.split(Cellphone$Churn,SplitRatio = 0.70)
LRTrainData=subset(Cellphone,dataSplitLG=="TRUE" )
LRTestData=subset(Cellphone,dataSplitLG=="FALSE")
names(LRTrainData)
#building LR model
LRTrainData=LRTrainData
LRTrainData=LRTrainData[ ,-c(2,4,8,9)]
LRTestData=LRTestData
LRTestData=LRTestData[ ,-c(2,4,8,9)]
model=glm(Churn~.,data=LRTrainData,family=  "binomial" )
vif(model)
summary(model)
confint(model)
anova(model,test="Chisq")

#preidicting LR model on test Dataset
predLR=predict(model,LRTestData,type = "response")
tabLR=(table(LRTestData$Churn,predLR>0.5 ))
tabLR
TP = tabLR[2,2]
FN = tabLR[2,1]
FP = tabLR[1,2]
TN = tabLR[1,1]

Accuracy = (TP+TN)/nrow(LRTestData)
Accuracy
sensitivity = TP/(TP+FN)  #Recall
sensitivity
Specificity = TN/(TN+FP)
Specificity 
Precision = TP/(TP+FP)
Precision
F1 = 2*(Precision*sensitivity)/(Precision + sensitivity) #Harmonic Mean
F1

#checking normality for residulas-shapiro test-Normal Q-Q plot
#Null Hypothesis: Residuals are normally distributed.
#Alternate Hypothesis: Residuals are not normally distributed
result=shapiro.test(model$residuals)
result
dev.off()
par(mfrow=c(2,2))
plot(model)
#pvalue is 2.2e-16 which is less than alpha =0.05,
#hence we reject the null hypothesis

#errors are homoscedacity- bptest (bruesh-Pagantest)-Residuals vs Levarage
#null hypothesis:residuals are having constant variance
#Alternate:residulas are not having constant variance
result=bptest(model)
result
#pvalue is 2.2e-16 which is less than alpha =0.05,
#hence we reject the null hypothesis

#errors should not be auto-correlated-dwtest
#null hypothesis:auto-correlation in residuals doesnot exists
#Alternate:rauto-correlation in residuals exists
result=dwtest(model)
result


#ROC Curve
ROCRpred = prediction(predLR, LRTestData$Churn)
auctest=as.numeric(performance(ROCRpred, "auc")@y.values)
auctest
perf = performance(ROCRpred, "tpr","fpr")
dev.off()
plot(perf,col="black",lty=4, lwd=2)
plot(perf,lwd=3,colorize = TRUE,
     main="ROC Curve for Logistic regression")


k <- blr_gains_table(model)
plot(k)

blr_ks_chart(k, title = "KS Chart",
             yaxis_title = " ",xaxis_title = "Cumulative Population %",
             ks_line_color = "black")

blr_gini_index(model, data = LRTrainData)
blr_gini_index(model, data = LRTestData)

blr_roc_curve(k, title = "ROC Curve",
              xaxis_title = "1 - Specificity",
              yaxis_title = "Sensitivity",roc_curve_col = "blue",
              diag_line_col = "red", point_shape = 18,
              point_fill = "blue", point_color = "blue",
              plot_title_justify = 0.5)  

blr_rsq_mcfadden(model)
blr_rsq_mcfadden_adj(model)


#Gini Coefficient train
Ginitest= (2*auctest) - 1
Ginitest

Ginitest = ineq(predLR , "gini")
Ginitest

#KS 
KSTest=max(perf@y.values[[1]]- perf@x.values[[1]])
KSTest


#KNN model:
#taking a copy of original dataset and scaling it for KNN
CellphoneKNN=CellphoneRawData
names(CellphoneKNN)
CellphoneKNNScaled=scale(CellphoneKNN[-1])
CellphoneKNNData=cbind(CellphoneKNNScaled,CellphoneKNN$Churn)
CellphoneKNNData=as.data.frame(CellphoneKNNData)
attach(CellphoneKNNData)
names(CellphoneKNNData)[11]="Churn"
#View(CellphoneKNNData)


#splitting data set for train and test
set.seed(1234)
dataSplitKNN=sample.split(CellphoneKNNData$Churn,SplitRatio = 0.70)            
KNNTrainData=subset(CellphoneKNNData,dataSplitKNN=="TRUE")
KNNTestData=subset(CellphoneKNNData,dataSplitKNN=="FALSE")
round(prop.table(table(KNNTrainData$Churn)),3)
round(prop.table(table(KNNTestData$Churn)),3)

#building KNN Model with possibly ideal K-values and respective confusion matrices
kvalue=as.integer(sqrt(nrow(KNNTrainData)))+1
kvalue
Kprediction=knn(KNNTrainData[,-11],
                KNNTestData[,-11],KNNTrainData[,11],
                47)
tabKnn=with(KNNTrainData,table(KNNTestData$Churn,Kprediction))
confusionMatrix(tabKnn)
Kprediction=knn(KNNTrainData[,-11],
                KNNTestData[,-11],KNNTrainData[,11],
                49)
tabKnn=with(KNNTrainData,table(KNNTestData$Churn,Kprediction))
confusionMatrix(tabKnn)
Kprediction=knn(KNNTrainData[,-11],
                KNNTestData[,-11],KNNTrainData[,11],
                51)
confusionMatrix(table(KNNTestData$Churn,Kprediction))
Kprediction=knn(KNNTrainData[,-11],
                KNNTestData[,-11],KNNTrainData[,11],
                53)
tabKnn=with(KNNTrainData,table(KNNTestData$Churn,Kprediction))
confusionMatrix(table(KNNTestData$Churn,Kprediction))

#splitting data for NaiveBayes Model
set.seed(1234)
CellphoneNB=CellphoneRawData
dataSplitNB=sample.split(CellphoneNB,SplitRatio = 0.70)
NBTrainData=subset(CellphoneNB,dataSplitNB=="TRUE")
NBTestData=subset(CellphoneNB,dataSplitNB=="FALSE")
attach(NBTrainData)

#Naive Bayes model prediction with "class" type
NBClass=naiveBayes(Churn~.,data=NBTrainData)
NBClass
NBPredClass=predict(NBClass,NBTestData[,-1],type="class")
NBPredClass
#Naive Bayes confusionMatrix with "class"
tabNBClass=with(NBTestData,table(NBTestData$Churn,NBPredClass))
confusionMatrix(tabNBClass)

#Naive Bayes model prediction with "raw" type
NBProb = naiveBayes(Churn ~., data=NBTrainData)
NBProb
NBPredProb = predict(NBProb, NBTestData[,-1], type = 'raw')

#Naive Bayes confusionMatrix with "raw"
tabNBProb = table(NBTestData$Churn, NBPredProb[,2]>0.5)

TP = tabNBProb[2,2]
FN = tabNBProb[2,1]
FP = tabNBProb[1,2]
TN = tabNBProb[1,1]

Accuracy = (TP+TN)/nrow(NBTestData)
Accuracy
sensitivity = TP/(TP+FN)  #Recall
sensitivity
Specificity = TN/(TN+FP)
Specificity 
Precision = TP/(TP+FP)
Precision














