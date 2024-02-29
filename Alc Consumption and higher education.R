d1=read.csv("student-por.csv")
d2=read.csv("student-mat.csv")
nrow(d1)
nrow(d2)
names(d1)
d3<-rbind(d1,d2)
# Above I combined2  dataframes into one
nrow(d3)
names(d3)
sapply(d3,class)

d3$Alc <- (d3$Dalc*5+d3$Walc*2)/7
# Above is the calculation of average Alcohol Consumption per day
# the Alc variable is then added to d3 data.frame

library(caret)
index<-createDataPartition(d3$higher, p = 0.8, list = F)
train<-d3[index,]
test<-d3[-index,]
# Here I have devides d3 dataframe into training (80%) and testing (20%) sets

library(ggplot2)
ggplot(test)

cor(d3[,-c(1:2,4:6,9:12,16:23)])

mod1<-lm(Alc~.,data = train)
summary(mod1)
predictAlcCons<-predict(mod1,newdata=test)
length(predictAlcCons)
RMSE<-sqrt(mean((predictAlcCons-test$Alc)^2))
RMSE 
#Root Mean Square of Errors is quite small, which shows the good quality of predictive model

actuals_preds <- data.frame(cbind(actuals=test$Alc, predicteds=predictAlcCons))
head(actuals_preds)
# above is head part of actuals_predicteds dataframe.

correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
# here is corralation accuracy of actuals and predicteds, it is 100% ;)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
# hear is the measure of minimal and maximal accuracy

plot(mod1)
coefficients(mod1) # model coefficients
confint(mod1, level=0.95) # CIs for model parameters 
fitted(mod1) # predicted values
residuals(mod1) # residuals
anova(mod1) # anova table 
vcov(mod1) # covariance matrix for model parameters 
influence(mod1) # regression diagnostics

library(rpart)
library(rpart.plot)
set.seed(1)
model_c<-rpart(higher~.,data=train)
prp(model_c,type = 2,extra=4,main="Probability of classes")
pred_class<-predict(model_c,test,type = "class")
pred_class[1:20]
confusionMatrix(pred_class, test$higher, positive='yes')
pred_prob<-predict(model_c,test)
pred_prob
library(ROCR)
P_Test<-prediction(pred_prob[,2],test$higher)
perf<-performance(P_Test,"tpr","fpr")
plot(perf,colorize = T)
performance(P_Test,"auc")@y.values

#KNN using CV method to determine best k value for higher accuracy
set.seed(1)
ctrl<-trainControl(method = "cv", number = 10) #making 10 defferent training and testing sets
knn_c<-train(higher~.,data = d3, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength=10)
knn_c$results
plot(knn_c)
grid<-expand.grid(k=9:15)
set.seed(1)
knn_c1<-train(higher~.,data = d3, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneGrid=grid)
knn_c1$results
plot(knn_c1)
library(class)
knn_p<-knn(train[,-c(1:2,4:6,9:12,16:23)],test[,-c(1:2,4:6,9:12,16:23)],cl = train$higher, k = 13,prob = T)
classes<-knn(train[,-c(1:2,4:6,9:12,16:23)],test[,-c(1:2,4:6,9:12,16:23)],cl = train$higher, k = 13)
confusionMatrix(classes, test$higher, positive='yes')
probs<-attr(knn_p,"prob")
probs
P_Test5<-prediction(probs,test$higher)
perf1<- performance(P_Test5,"tpr","fpr")
plot(perf1,colorize=T,main="KNN Model AUC (using CV)")
performance(P_Test5,"auc")@y.values
# Finally got the Area Under The Curve 80 percent, which tells about the model to be average (good) 
# So we have got model which predicts people wishing to get higher education with 80% accuracy