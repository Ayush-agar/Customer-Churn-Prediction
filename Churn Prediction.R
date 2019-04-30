library(tidyverse) 
library(MASS)
#install.packages("car")
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
#install.packages("pROC")
library(pROC)
#install.packages("ggcorrplot")
library(ggcorrplot)
telco<- read.csv("Telco Data.csv")
options(repr.plot.width=6, repr.plot.height=4)
missing_data<-telco%>% summarise_all(funs(sum(is.na(.))/n()))
missing_data<-gather(missing_data, key = "variables",value = "percent_missing")
ggplot(missing_data,aes(x=reorder(variables, percent_missing), y=percent_missing))+
  geom_bar(stat = "identity", fill="red", aes(color=I('white')), size=.3)+
  xlab('variables')+
  coord_flip()+
  theme_bw()
telco<- telco[complete.cases(telco),]
complete.cases(Telco_Data)
telco$SeniorCitizen<-as.factor(ifelse(telco$SeniorCitizen==1,'YES', 'NO'))
theme1<-theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = .5), legend.position = "none")
theme2<-theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), legend.position = "none")
glimpse(telco)
options(repr.plot.width = 6, repr.plot.height = 4)
telco%>%
  group_by(Churn)%>%
  summarise(Count=n())%>%
  mutate(percent=prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill= Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")

options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+ theme1,
#            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1,
#            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme1,
#            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+theme1,
#            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+theme1,
#            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

plot_grid(ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

options(repr.plot.height = 2, repr.plot.width = 6)
ggplot(telco, aes(y= tenure, x="", fill=Churn))+
  geom_boxplot()+
  theme_bw()+
  xlab(" ")

options(repr.plot.height = 2, repr.plot.width = 6)
ggplot(telco, aes(y= MonthlyCharges, x="", fill=Churn))+
  geom_boxplot()+
  theme_bw()+
  xlab(" ")

options(repr.plot.height = 2, repr.plot.width = 6)
ggplot(telco, aes(y= TotalCharges, x="", fill=Churn))+
  geom_boxplot()+
  theme_bw()+
  xlab(" ")

options(repr.plot.height = 4, repr.plot.width = 6)
telco_cor<- round(cor(telco[,c("tenure","MonthlyCharges","TotalCharges")]), 1)
ggcorrplot(telco_cor, title = "correlation")+theme(plot.title = element_text(hjust = .5))

options(repr.plot.height = 4, repr.plot.width = 4)
boxplot(telco$tenure)$out

options(repr.plot.height = 4, repr.plot.width = 4)
boxplot(telco$MonthlyCharges)$out
boxplot(telco$TotalCharges)$out

# DATA PREPARATION:
#   
# Cleaning the Categorical features
# Standardising Continuous features
# Creating derived features
# Creating dummy variables for factor variables
# Creating the final dataset
# Splitting the data into train and validation set

telco<-data.frame(lapply(telco,function(x){
  gsub("No internet service", "No", x)}))
telco<-data.frame(lapply(telco,function(x){
  gsub("No phone service", "No", x)}))

num_columns<- c("tenure","MonthlyCharges","TotalCharges")
telco[num_columns]<- sapply(telco[num_columns], as.numeric)
telco_int<- telco[,c("tenure","MonthlyCharges","TotalCharges")]
telco_int<-data.frame(scale(telco_int))

#max(telco$tenure)
#min(telco$tenure)

telco<- mutate(telco, tenure_bin=tenure)

telco$tenure_bin[telco$tenure_bin>=0 & telco$tenure_bin<=12]<-'0-1 year'
telco$tenure_bin[telco$tenure_bin>12 & telco$tenure_bin<=24]<-'1-2 year'
telco$tenure_bin[telco$tenure_bin>24 & telco$tenure_bin<=36]<-'2-3 year'
telco$tenure_bin[telco$tenure_bin>36 & telco$tenure_bin<=48]<-'3-4 year'
telco$tenure_bin[telco$tenure_bin>48 & telco$tenure_bin<=60]<-'4-5 year'
telco$tenure_bin[telco$tenure_bin>60 & telco$tenure_bin<=72]<-'5-6 year'

telco$tenure_bin<- as.factor(telco$tenure_bin)

options(repr.plot.width =6, repr.plot.height = 3)
ggplot(telco, aes(tenure_bin, fill=tenure_bin))+geom_bar()+theme1

# Creating Dummy Variables
telco_cat<-telco[,-c(1,6,19,20)]
dummy<-data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1, data = telco_cat))[,-1]))

head(dummy)

# Creating the final dataset by combining the numeric and dummy data frames.

telco_final<-cbind(telco_int,dummy)
head(telco_final)

# Splitting the data into train and validation data.

set.seed(123)
indices= sample.split(telco_final$Churn, SplitRatio = .7)
train=telco_final[indices,]
test= telco_final[!(indices),]

# MODEL BUILDING 1
# 
# Starting with Logistic Regression

model_1<-glm(Churn~., data = train, family = "binomial")
summary(model_1)

model_2<-stepAIC(model_1, direction = "both")
summary(model_2)

# We can use variance inflation factor (vif) to get rid of redundant predictors or the variables 
# that have high multicollinearity between them. Multicollinearity exists when two or more 
# predictor variables are highly related to each other and then it becomes difficult to 
# understand the impact of an independent variable on the dependent variable.

vif(model_2)

#Removing DeviceProtection due to high p-value 
model_3 <-glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                Partner + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + TechSupport + 
                StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.year + 
                tenure_bin.x5.6.year, family = "binomial", data = train)
summary(model_3)
vif(model_3)

#Removing StreamingTV  as it has high p-value 
model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                 Partner + InternetService.xFiber.optic + InternetService.xNo + 
                 OnlineSecurity + OnlineBackup + TechSupport +  
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + tenure_bin.x1.2.year + 
                 tenure_bin.x5.6.year, family = "binomial", data = train)
summary(model_4)
vif(model_4)

final_model<-model_3

# Model Evaluation using the validation data:

pred<- predict(final_model, type="response", newdata = test[,-24])
summary(pred)
test$prob<- pred

# Using probability cutoff of 50%.
pred_churn<-factor(ifelse(pred>=.50,"Yes","No"))
actual_churn<-factor(ifelse(test$Churn==1,"Yes","No"))
table(pred_churn, actual_churn)

# Let's find the Accuracy, Sensitivity, Specificity using 50% cutoff

cutoff_churn<-factor(ifelse(pred>=.50,"Yes","No"))
conf_final<-confusionMatrix(cutoff_churn,actual_churn,positive = "Yes")
accuracy<-conf_final$overall[1]
sensitivity<-conf_final$byClass[1]
specificity<-conf_final$byClass[2]
accuracy
sensitivity
specificity

# As we can see above, when we are using a cutoff of 0.50, we are getting a good accuracy and 
# specificity, but the sensitivity is very less. Hence, we need to find the optimal probalility 
# cutoff which will give maximum accuracy, sensitivity and specificity

perform_fn<-function(cutoff){
    predict_churn<-factor(ifelse(pred>=cutoff,"Yes","No"))
    conf<-confusionMatrix(predict_churn, actual_churn, positive = "Yes")
    accuracy<-conf$overall[1]
    sensitivity<-conf$byClass[1]
    specificity<-conf$byClass[2]
    out<-t(as.matrix(c(sensitivity, specificity, accuracy)))
    colnames(out)<-c("sensitivity","specificity","accuracy")
    return(out)
}

options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))

cutoff_churn<- factor(ifelse(pred>=.32,"Yes","No"))
conf_final<- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy<- conf_final$overall[1]
sensitivity<-conf_final$byClass[1]
specificity<- conf_final$byClass[2]
accuracy
sensitivity
specificity

# Logistic Regression with a cutoff probability value of 0.32 gives us better values of accuracy, 
# sensitivity and specificity in the validation data.

# MODEL BUILDING 2

set.seed(123)
telco_final$Churn <- as.factor(telco_final$Churn)

indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

#Training the Decision Tree model using all variables & Predicting in the validation data
options(repr.plot.width = 10, repr.plot.height = 8)
library(rpart)
# library(rpart.plot)

#Training
Dtree= rpart(Churn~.,data = train, method = "class")
summary(Dtree)

#predicting
DTpred<-predict(Dtree,type = "class", newdata = validation[,-24])

#checking the confusion matrix
confusionMatrix(validation$Churn,DTpred)

#Modelbuilding 3 Random Forest

library(randomForest)
set.seed(123)
telco_final$Churn<-as.factor(telco_final$Churn)
indices=sample.split(telco_final$Churn, SplitRatio = .7)
train=telco_final[indices,]
validation=telco_final[-indices,]

#Training the RandomForest Model
model.rf<-randomForest(Churn~.,data = train,proximity=FALSE, importance=FALSE, ntree=500, mtry=4,
                       do.trace=FALSE)
model.rf

#Predicting on the validation set and checking the Confusion Matrix.
testPred<- predict(model.rf,newdata = validation[,-24])
table(testPred,validation$Churn)

confusionMatrix(validation$Churn, testPred)
