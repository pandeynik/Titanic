## call caret package
#caret package provides us direct access to various functions for training 
#our model with various machine learning algorithms like Knn, SVM, 
#decision tree, linear regression, etc
library(caret)

#import data set 
train_knn= read.csv("train_knn.csv")

#For checking the structure of data frame we can call the function str over 
str(train_knn)

#data slicing: Dividing dataset into train and test data set
# to make this slicing  replicable, use set.seet()
sample_size= floor(0.75*nrow(train_knn))
set.seed(111)
train_data= sample(seq_len(nrow(train_knn)), size = sample_size)
data_train= train_knn[train_data, ]
data_test= train_knn[-train_data, ]

##one more method by which wecan split our data into test and train
#caret packageb have createDataPatition() function to split data
set.seed(555)
intrain <- createDataPartition(y = train_knn$Survived, p= 0.7, list = FALSE)
training <- train_knn[intrain,]
testing <- train_knn[-intrain,]
#here y takes the value of variable according to whcih data needs to be partitioned
#target variable is at Survived, so we are passing train_knn$Survived
#p parameter varry from 0-1 , shows the percentage of split
#list for whether to return a list or matrix


#checking dimension of train an test data set
dim(data_train)
dim(data_test)

#checking for NA vlaue if present
anyNA(train_knn_1)

#Our target variable consists of 3 values 1, 2, 3. These should considered as categorical variables. 
#To convert these to categorical variables, we can convert them to factors.
data_train[["Survived"]]=factor(data_train[["Survived"]])

#traing the KNN model
#Caret package provides train() method for training our data for various algorithms. 
#We just need to pass different parameter values for different algorithms. 
#Before train() method, we will first use trainControl() method. 
#It controls the computational nuances of the train() method.

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#We are setting 3 parameters of trainControl() method. 
#The "method" parameter holds the details about resampling method. 
#We can set "method" with many values like  "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" etc. 
#For this , let's try to use repeatedcv i.e, repeated cross-validation.

#fitting KNN model
set.seed(3333)
knn_fit_123 <- train(Survived ~., data = data_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
#summary
#Its showing Accuracy and Kappa metrics result for different k value.
#From the results, it automatically selects best k-value.
knn_fit_123

#predict Survived value for test data set
test_pre_knn=predict(knn_fit_123,newdata = data_test)
test_pre_knn

#confusion Matrix
confusionMatrix(test_pre_knn, data_test$Survived)
#accuracy
181/223
#81 %

#predict Survived value for actual test data set
test_pre_knn_test=predict(knn_fit_123,newdata = test_knn1)
test_pre_knn_test

#saving resut at directory in csv file
write.csv(test_pre_knn_test, file="Survived_knn.csv",  row.names = FALSE)
