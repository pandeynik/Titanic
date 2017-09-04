library(e1071)
nb_default= naiveBayes(Survived~., data=train_knn[,-4])
default_pred= predict(nb_default, test_knn, type="class")
table(default_pred, test_knn,dnn=c("Prediction", "Actual"))
anyNA(train_knn)
anyNA(test_knn1)
install.packages("C50")
install.packages("klar")
set.seed(2)
train_naive= read.csv("train.csv")
test_naive=read.csv("test.csv")
extractFeatures= function(data){
  features=c("Pclass",
             "Age",
             "Sex",
             "Parch",
             "SibSp",
             "Fare",
             "Embarked")
fea=data[,features]

fea$Age[is.na(fea$Age)] <- -1
fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
fea$Embarked[fea$Embarked==""] = "S"
fea$Sex      <- as.factor(fea$Sex)
fea$Embarked <- as.factor(fea$Embarked)
return(fea)
}

extract_train <-extractFeatures(train_naive)
extract_test <-extractFeatures(test_naive)

library(C50)
naive_bayes <- naiveBayes(extract_train,as.factor(train_naive$Survived))
submission <- data.frame(PassengerId = test_naive$PassengerId)
submission$Survived<- predict(naive_bayes,as.data.frame(extract_test))
write.csv(submission, file = "1_naive_bayes_r_submission.csv", row.names=FALSE)

print(table(predict(naive_bayes,as.data.frame(extract_test))))
summary(naive_bayes)

#_______________________________________________________________________________
model=naiveBayes(train_naive$Survived., data=train_naive)


#_________________________________
data(train)
train=read.csv("train.csv")
test=read.csv("test.csv")
train
 model_1= naiveBayes(train$Survived~., data=train)
predict(model_1, train[1:10,-1])
predict(model_1, train[1:10,-1], type="raw")
pred_naive=predict(model_1, train[,-1])


table(predict(model_1, train[,-1]))
attach(train)
Survived
train$Survived

#______________________________________
m_1=naiveBayes(Survived~., data=train)
m_2=naiveBayes(train[,-5], train[,2])
m_2
table(predict(m_2,train[,-5], train[,2]))
#Error in match.arg(type) : 'arg' must be NULL or a character vector
head(train[,3], n=4)
head(train[,5], n=10)

#_______________________________________
library(caret)
library(lattice)
library(ggplot2)
library(klaR)
head(train)
names(train)
x=train[,-12]
x
y=train$Survived
y
train$Survived=as.factor(y)
model_caret=train(x,y,'nb',trControl=trainControl(method='cv',number=10))
#Error: wrong model type for regression
dput(train)
#____________________________________________________
library(e1071)
m_3=naiveBayes(Survived~., data=train)
class(m_3)
summary(m_3)

?dput
print((m_3))
tbl_list=sapply(train[-12], table, train[,12])



levels(train$Survived)
train$Survived[train$Survived==1]="yes"
train$Survived[train$Survied==0]="No"
leve