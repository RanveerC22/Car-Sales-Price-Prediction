data<-read.csv("Car_details_v3 - Copy.csv")
mdata<-data[,!(names(data) %in% c("name","year","fuel","seller_type","transmission","owner","torque","torque2","rpm"))]
mdata<-mdata[1:25,]
View(mdata)

#Applying Multiple Linear Regression To The Above Dataset
cat("By applying the Multiple Linear Regression the summary we get is")
l1<-lm(selling_price~km_driven+max_power+mileage+engine+seats,mdata)
print(summary(l1))

cat("************************************************************************************************************************\n")
cat("According to our observation the independent variables(engine and seats) has no significance so we exclude the variables\n")
cat("************************************************************************************************************************\n")
cat("\n So our new summary is\n")

#Modifying the data and then applying the multiple linear regression
mlrdata<-data[,!(names(data) %in% c("name","year","fuel","seller_type","transmission","owner","torque","torque2","rpm","seats","engine"))]
mlrdata<-mlrdata[1:25,]
View(mlrdata)
l2<-lm(selling_price~.,mlrdata)
print(summary(l2))


cat("*****************************************************************************************************************************************************")
cat("\nSo we can tell that our\n")
cat("1)Multiple regression provides 74.9% of variance, which is explained by the independent variables\n")

#Removing the percentage of error on the prediction done by MLR
pred_mlr=sum(predict(l2))
pred_ml=sum(mdata$selling_price)
cat("*****************************************************************************************************************************************************\n")
cat(" By Applying Random Forest Algorithm\n")
cat("*************************************\n")

library(randomForest)
rfdata<-mlrdata
View(rfdata)
set.seed(123)

ind<-sample(2,nrow(rfdata),replace = TRUE,prob = c(0.7,0.3))
train<-rfdata[ind==1,]  #70% to training data
test<-rfdata[ind==2,]   #30% to testing data

View(test)

rf<-randomForest(selling_price~.,data = train)
print(rf)

predo<-predict(rf,test) #Predicted Values for testing dataset done by Random Forest Algorithm
test$predo=predo
View(test)

#Calculating the % of error for RM
pred_rf<-sum(predict(rf,test)) #Sum of predicted values done by RM for 10% testing data set 
actu_rf<-sum(test$selling_price) #Sum of actual vales = selling_price
actu_rf
pred_rf
diff_rm=pred_rf-actu_rf
a<-(diff_rm/actu_rf)*100
a
cat("***************************************************************************************************************************\n")
cat("So we can say that\n")
cat("1)Random Forest Algorithm provides 62.42% of variance for 30% of data-set, which is explained by the independent variables\n")
cat("2)The % of error explained by Random Forest Algorithm is 0.65\n")
cat("***************************************************************************************************************************\n")
cat("By applying SVM\n")

library(e1071)
library(ggplot2)

svmdata<-mlrdata
sv<-svm(formula=selling_price~.,
               data=svmdata,
               type='eps-regression',
               kernel='radial')
print(summary(sv))



#Calculating the error for SVM
pred_svm<-sum(predict(sv,svmdata))
actu_svm<-sum(svmdata$selling_price)


diff_svm=actu_svm-pred_svm
diff_svm<-(diff_svm/actu_svm)*100
diff_svm

svmdata$predicted_values<-predict(sv,svmdata)
View(svmdata)

cat("***************************************************************************************************************************\n")
cat("So we can say that\n")
cat("1)Support Vector Machine provides 0.73% of error\n")
cat("***************************************************************************************************************************\n")

p<-ggplot()+geom_point(aes(x=svmdata$mileage,y=svmdata$selling_price),colour='red')
my<-print(p)+geom_line(aes(x=svmdata$mileage,y=predict(sv,newdata = svmdata)),colour='blue')
print(my)


