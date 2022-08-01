library(ggplot2)
library(dplyr)


# Heart Disease Analysis

# Import dataset
df = indata = read.csv("heart.csv", sep = ",")

# Quick look at the loaded data
sprintf("Number of columns: %s and the number of observations: %s", ncol(indata), nrow(indata))

# Columns 
colnames(indata)

# Structure of data
str(indata)

# Data distribution
ggplot(data = indata) +
  geom_bar(aes(x = target,fill = as.factor(target))) + 
  ggtitle("Target distribution") + 
  labs(fill = "Target") + 
  xlab("Target: 0 = No, 1 = Yes") + 
  ylab("Density")


# Data Summary
summary(indata)

### Data pre processing

# Checking missing values
apply(indata,2,function(x){
  sum(is.na(x))
})

### This shows it has no missing values in any of the column
# We can now introduce  NULL values randomly in each column

# Inserting 10 NULLS in each column
randomRow =  sample(c(1:1025),10, replace = T)

# For cp 
indata[randomRow,"cp"] = NA

randomRow =  sample(c(1:1025),2, replace = T)

# For chol
indata[randomRow,"chol"] = NA


randomRow =  sample(c(1:1025),6, replace = T)

# For chol
indata[randomRow,"oldpeak"] = NA


# Checking null values again
mis = apply(indata,2,function(x){
  sum(is.na(x))
})

mis_df = data.frame(Features = colnames(indata), MissingVals = mis)

ggplot(mis_df) + 
  geom_bar(aes(x = Features, y = MissingVals), stat = "identity") + 
  ggtitle("Missing Values") + 
  ylab("Missing Values")


# Replacing missing data again - Replaced by mean

indata[which(is.na(indata$cp)),"cp"] = round(mean(indata[which(!is.na(indata$cp)),"cp"])) 
indata[which(is.na(indata$chol)),"chol"] = round(mean(indata[which(!is.na(indata$chol)),"chol"])) 
indata[which(is.na(indata$oldpeak)),"oldpeak"] = round(mean(indata[which(!is.na(indata$oldpeak)),"oldpeak"])) 

df = indata

# Converting target variable into factor as it is a categorical variable
indata$target = ifelse(indata$target == 0,"No","Yes")
indata$target = as.factor(indata$target)


# checking structure again
str(indata)

# Splitting data in training and test set
library(caTools)

splt =  sample.split(indata$target, SplitRatio = 0.8) 
train = subset(indata,splt == TRUE)
test = subset(indata,splt == FALSE)


# Adding age group
i = 1
for(i in 1:nrow(indata)){
  if(indata[i,"age"] >= 0 && indata[i,"age"] < 25){
    indata[i,"group"] = "0-25"
  }
  
  if(indata[i,"age"] >= 25 && indata[i,"age"] < 50){
    indata[i,"group"] = "25-50"
  }
  
  if(indata[i,"age"] >= 50 && indata[i,"age"] < 75){
    indata[i,"group"] = "50-75"
  }
  
  if(indata[i,"age"] >= 75){
    indata[i,"group"] = "75-100"
  }
}

# Converting group into factor
indata$group  = as.factor(indata$group)

# Exploratory model
# Effect of age on health

ggplot(indata) + 
  geom_bar(aes(x = target, fill = target), stat = "count")  + 
  facet_wrap(group~.) + 
  xlab("Health Status") + 
  ylab("Count") + 
  labs(fill = "Healthy/Unhealthy") + 
  ggtitle("Frequency of Healthy/Unhealth patients", subtitle = "By Age Group")



# Most common chest pain type

ggplot(indata) + 
  geom_bar(aes(x = as.factor(cp)), stat = "count", )  + 
  xlab("Chest Pain Type") + 
  ylab("Count") + 
  labs(fill = "Healthy/Unhealthy") + 
  ggtitle("Chest pain type count")


# Most severe chest pain type

ggplot(indata) + 
  geom_bar(aes(x = as.factor(cp), fill = as.factor(cp)), stat = "count", )  + 
  facet_wrap(target~.)  +
  xlab("Chest Pain Type") + 
  ylab("Count") + 
  labs(fill = "Healthy/Unhealthy") + 
  ggtitle("Chest pain type count")


# Effect of chest pain on health by age group

ggplot(indata) + 
  geom_bar(aes(x = as.factor(cp),fill = target), stat = "count", position = "dodge")  + 
  facet_wrap(group~.) + 
  xlab("Chest Pain Type") + 
  ylab("Count") + 
  labs(fill = "Healthy/Unhealthy") + 
  ggtitle("Effect of chest pain on Healthy/Unhealth patients", subtitle = "By Age Group")


# Relation between Blood pressure and health
ggplot(indata) + 
  geom_density(aes(x = trestbps, fill = target), stat = "density", alpha = 0.7) + 
  xlab("Rest Blood Pressure") + 
  labs(fill = "Healthy/Unhealthy") + 
  ylab("Density") + 
  ggtitle("Density of Rest Blood Pressure")


# How fasting blood sugar (> 120 mg/dl) relates to health
ggplot(indata) + 
  geom_density(aes(x = fbs, fill = target), stat = "density", alpha = 0.5) + 
  xlab("Blood Sugar ") + 
  labs(fill = "Healthy/Unhealthy") + 
  ylab("Density") + 
  ggtitle("Fasting blood sugar (> 120 mg/dl)")


# checking correlation between variables
corr = cor(indata[,c(1:13)])
library(corrplot)
corrplot(corr)


# Modelling

# Using SVM
library(e1071)
set.seed(235)
svmfit = svm(target~., data = train, scale = T, type = "C-classification")
summary(svmfit)

# Making pred using SVM
pred = predict(svmfit,newdata = test[,-14])

# Accuracy using confusion matrix
cm = table(pred,test$target)
cm

# Accuracy
svm_acc=((cm[1,1] + cm[2,2])/sum(cm))
svm_acc

#Error Rate
error_svm=((cm[1,2]+cm[2,1])/sum(cm))


#Precision value
svm_yesp=(cm[1,2]+cm[2,2])
pres_svm=(cm[2,2]/svm_yesp)


#Recall Value
svm_yesa<-(cm[2,1]+cm[2,2])
rec_svm<-(cm[2,2]/svm_yesa)


#F1 Score
f1_svm= ((2*pres_svm*rec_svm)/(pres_svm+rec_svm))
f1_svm

# Applying cross validation to svm
library(caret)
library(pROC)

svm_ctrl = trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                        classProbs = T,
                        summaryFunction = twoClassSummary,
                        savePredictions = TRUE
                        )
svm_model =  train(target ~., train,
                   method = "svmRadial",
                   tuneLength = 5,
                   trControl = svm_ctrl,
                   preProc = c("center","scale"),
                   metric = "ROC",
                   verbose = TRUE)
max(svm_model$results[,"ROC"])

#Confusion Matrix after tuning
p2 <- predict(svm_model, test[,-14])
cm2 <- table(p2, test$target)


# Accuracy
svm_acc1=sum(diag(cm2)/sum(cm2))

#Error Rate
error_svm1=((cm2[1,2]+cm[2,1])/sum(cm2))

#Precision value
svm_yesp1=(cm2[1,2]+cm2[2,2])
pres_svm1=(cm2[2,2]/svm_yesp1)

#Recall Value
svm_yesa1<-(cm2[2,1]+cm2[2,2])
rec_svm1<-(cm2[2,2]/svm_yesa1)

#F1 Score
f1_svm1= ((2*pres_svm1*rec_svm1)/(pres_svm1+rec_svm1))

#ROC Curve and Auc score
pred = predict(svm_model, test[,-14], type = "prob")
gc_pROC <- roc(response = test$target, predictor = pred[, "Yes"])
plot(gc_pROC, main = "ROC using Cross Validated SVM")
cm2
svm_acc1
f1_svm1
gc_pROC$auc




# Using randomforst
library(randomForest)

set.seed(123)

rf_ctrl = trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 5,
                       classProbs = TRUE,
                        search = "random")

rf_model =  train(target ~., train,
                   method = "rf",
                   tuneLength = 10,
                   trControl = rf_ctrl,
                   metric = "ROC",
                   verbose = TRUE)
rf_model

# Confusion matrix
pred_rf <- predict(rf_model, test[,-14])
cm_rf<- table(pred_rf, test$target)

# Accuracy
rf_acc=sum(diag(cm_rf)/sum(cm_rf))

#Precision value
rf_yesp=(cm_rf[1,2]+cm_rf[2,2])
pres_rf=(cm_rf[2,2]/rf_yesp)

#Recall Value
rf_yesa<-(cm_rf[2,1]+cm_rf[2,2])
rec_rf<-(cm_rf[2,2]/rf_yesa)

#F1 Score
f1_rf= ((2*pres_rf*rec_rf)/(pres_rf+rec_rf))

# ROC for rf
pred_rf = predict(rf_model, test[,-14], type = "prob")
gc_pROC_rf <- roc(response = test$target, predictor = pred_rf[, "Yes"])
plot(gc_pROC_rf, main = "ROC using Random Forest")

cm_rf
rf_acc
f1_rf
gc_pROC_rf$auc

# Using search grid
set.seed(123)

rf_ctrl_grd = trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       classProbs = TRUE,
                       search = "grid")
mtry = sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=c(1:15))
rf_model_grd =  train(target ~., train,
                  method = "rf",
                  tuneLength = 15,
                  trControl = rf_ctrl,
                  metric = "ROC",
                  tuneGrid=tunegrid,
                  verbose = TRUE)
rf_model_grd

# Confusion matrix
pred_rf1 <- predict(rf_model_grd, test[,-14])
cm_rf1<- table(pred_rf1, test$target)

# Accuracy
rf_acc1=sum(diag(cm_rf1)/sum(cm_rf1))

#Precision value
rf_yesp1=(cm_rf1[1,2]+cm_rf1[2,2])
pres_rf1=(cm_rf1[2,2]/rf_yesp1)

#Recall Value
rf_yesa1<-(cm_rf1[2,1]+cm_rf1[2,2])
rec_rf1<-(cm_rf1[2,2]/rf_yesa1)

#F1 Score
f1_rf1= ((2*pres_rf1*rec_rf1)/(pres_rf1+rec_rf1))

# Plotting roc for rf with grid search
pred_rf_grd = predict(rf_model_grd, test[,-14], type = "prob")
gc_pROC_rf_grd <- roc(response = test$target, predictor = pred_rf_grd[, "Yes"])
plot(gc_pROC_rf_grd, main = "ROC using Random Forest with Grid Search")

#Display Results
cm_rf1
#Accuracy
rf_acc1
#F1 score
f1_rf1
#AUC score
gc_pROC_rf_grd$auc

#  Using  Naive Bayes
library(e1071)
library(klaR)
nv_ctrl = trainControl(method = "cv", number = 10, classProbs = TRUE)
nv_model = train(x = train[,-14],y = train$target,
                 method = "nb",
                 tuneLength = 15,
                 trControl = nv_ctrl)
plot(nv_model)
# Confusion matrix
pred_nv <- predict(nv_model, test[,-14])
cm_nv<- table(pred_nv, test$target)
cm_nv

# Accuracy
nv_acc=sum(diag(cm_nv)/sum(cm_nv))
nv_acc

#Precision value
nv_yesp=(cm_nv[1,2]+cm_nv[2,2])
pres_nv=(cm_nv[2,2]/nv_yesp)

#Recall Value
nv_yesa<-(cm_nv[2,1]+cm_nv[2,2])
rec_nv<-(cm_nv[2,2]/nv_yesa)

#F1 Score
f1_nv= ((2*pres_nv*rec_nv)/(pres_nv+rec_nv))
f1_nv

#ROC Curve
pred_nv = predict(nv_model,test[,-14], type = "prob")
gc_pROC_nv <- roc(response = test$target, predictor = pred_nv[, "Yes"])
plot(gc_pROC_nv, main = "ROC using Naive Bayes")
gc_pROC_nv$auc



