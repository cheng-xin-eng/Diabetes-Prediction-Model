setwd("/Users/chengxineng/Desktop/Y1 S2/DSA1101/Data")
dat=read.csv("diabetes-dataset.csv", header = TRUE)
names(dat)
head(dat)
dim(dat)
attach(dat)
set.seed(1101)

#############################################################################################

#Part I
#1
#response variable: blood_glucose_level, diabetes
#input variables: gender, age, hypertension, heart_disease, smoking_history, bmi, HbA1c_level, blood_glucose_level

#2
M1=lm(diabetes ~ age + gender + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level)
summary(M1)
#(i) gender and diabetes
tab_gen=table(gender, diabetes);tab_gen

#rate(diabetes|F)
rate_dF=tab_gen[1,2]/sum(tab_gen[1,]);rate_dF
#rate(diabetes|M)
rate_dM=tab_gen[2,2]/sum(tab_gen[2,]);rate_dM

#rate(diabetes|M)>rate(diabetes|F)
#Positive association between diabetes and females

#(ii) hypertension and diabetes
tab_hyp=table(hypertension, diabetes);tab_hyp

#rate(diabetes|H)
rate_dH=tab_hyp[2,2]/sum(tab_hyp[2,]);rate_dH
#rate(diabetes|NH)
rate_dNH=tab_hyp[1,2]/sum(tab_hyp[1,]);rate_dNH

#rate(diabetes|H)>rate(diabetes|NH)
#Positive association between diabetes and hypertension

#(iii) heart disease and diabetes
tab_heart=table(heart_disease,diabetes);tab_heart

#rate(diabetes|HD)
rate_dHD=tab_heart[2,2]/sum(tab_heart[2,]);rate_dHD
#rate(diabetes|NHD)
rate_dNHD=tab_heart[1,2]/sum(tab_heart[1,]);rate_dNHD

#rate(diabetes|HD)>rate(diabetes|NHD)
#Positive association between diabetes and heart diseases

#(iv) smoking_history and diabetes
tab_smoke=table(smoking_history, diabetes);tab_smoke

for (i in 1:6) {
  rate_smoke=tab_smoke[i,2]/sum(tab_smoke[i,])
  print(round(rate_smoke, digit=3))
}

#rate(diabetes|current)=0.102
#rate(diabetes|ever)=0.118
#rate(diabetes|former)=0.170
#rate(diabetes|never)=0.095
#rate(diabetes|No Info)=0.041
#rate(diabetes|not current)=0.107
#rates are different, association between diabetes and smoking history is shown.

#(v) bmi and diabetes
box_dbmi=boxplot(bmi~diabetes)
cor(bmi,diabetes)
#0.2143574
#boxplots are mostly overlapping, association between diabetes and bmi is relatively weak.

#(vi) age and diabetes
box_dage=boxplot(age~diabetes)
cor(age, diabetes)
#0.258008
#boxplots are mostly overlapping, association between diabetes and age is relatively strong.

#(vii) HbA1c_level and diabetes
box_dHbA1c=boxplot(HbA1c_level~diabetes)
cor(HbA1c_level,diabetes)
#0.4006603
#moderate overlap between the boxplots, association between diabetes and HbA1c_level is moderately strong.


#(viii)blood_glucose_level and diabetes
box_dbgl=boxplot(blood_glucose_level~diabetes)
cor(blood_glucose_level,diabetes)
#0.419558
#moderate overlap between the boxplots, association between diabetes and HbA1c_level is moderately strong.

#3
train=sample(1:100000,80000)
train.data=dat[train,]
test.data=dat[-train,]

#############################################################################################
#Part II
#4
library(rpart)
library(rpart.plot)
library(ROCR)

#decision trees
model.DT=rpart(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi +HbA1c_level + blood_glucose_level,
           method ="class",
           data = train.data,
           control = rpart.control(minsplit = 1),
           parms = list(split ='information'))

rpart.plot(model.DT , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)


pred.mDT = as.data.frame(predict(model.DT, newdata=test.data[,1:8], type='prob'))
pred.yes.DT=pred.mDT[,2]

pred.DT = prediction(pred.yes.DT, test.data$diabetes)
roc.DT = performance(pred.DT, measure="tpr", x.measure="fpr")
plot(roc.DT)

legend("bottomright", c("Naive Bayes","Decision Trees","Logistic Regression"),col=c("red","black","blue"), lty=1)
auc.DT = performance(pred.DT , measure ="auc")
auc.DT@y.values[[1]]
#0.8355068

#accuracy 
DT.pred = predict(model.DT, test.data[,1:8], type='class')
DT.confusion.matrix=table(DT.pred,test.data$diabetes);DT.confusion.matrix
DT.accuracy=sum(diag(DT.confusion.matrix))/sum(DT.confusion.matrix);DT.accuracy
#accuracy is 0.97225

#Naive Bayes
library(e1071)
model.NB=naiveBayes(diabetes ~ age + gender + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level, train.data)
results=predict(model.NB,newdata=test.data[1:8],"class")
confusion.matrix=table(results, test.data$diabetes);confusion.matrix

p.NB=as.data.frame(predict(model.NB, test.data[,1:8],"raw"))
p.yes.NB=p.NB[,2]
pred.NB=prediction(p.yes.NB,test.data$diabetes)
roc.NB = performance(pred.NB, measure="tpr", x.measure="fpr")
plot(roc.NB,  col = "red", add=TRUE) 

auc.NB=performance(pred.NB , "auc")
auc.NB@y.values[[1]]
#0.9241489

#accuracy 
NB.pred = predict(model.NB, test.data[,1:8], type='class')
NB.confusion.matrix=table(NB.pred,test.data$diabetes);NB.confusion.matrix
NB.accuracy=sum(diag(NB.confusion.matrix))/sum(NB.confusion.matrix);NB.accuracy
#accuracy is 0.9052


#Logistic Regression
model.Log=glm(diabetes ~ age + gender + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level, data=train.data, family=binomial(link= "logit"))
summary(model.Log)

pred.Log= as.data.frame(predict(model.Log, newdata=test.data[1:8],type="response"))
pred.Log = prediction(pred.Log, test.data$diabetes)
roc.Log = performance(pred.Log, measure="tpr", x.measure="fpr")
plot(roc.Log, col = "blue", add=TRUE)

auc.Log = performance(pred.Log , measure ="auc")
auc.Log@y.values[[1]]
#0.962497

#accuracy 
Log.pred = predict(model.Log, test.data[,1:8], type='response')
Log.pred=round(Log.pred);Log.pred
Log.confusion.matrix=table(Log.pred,test.data$diabetes)
Log.accuracy=sum(diag(Log.confusion.matrix))/sum(Log.confusion.matrix);Log.accuracy
#accuracy is 0.9603









