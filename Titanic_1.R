# setting working directory
setwd("C:/Users/Nikhil/Desktop/data analyst/R/Titanic problem")

# For data manipulation and tidying
library(dplyr)

# For data visualizations
library(ggplot2)

# For modeling and predictions
library(caret)
install.packages("glmnet")
library(glmnet)
install.packages("ranger")
library(ranger)
install.packages("e1071")
library(e1071)
install.packages("ggthemr")
install.packages("ggthemr")

e1071
# Importing data 
train=read.csv("train.csv")
test=read.csv("test.csv")
full= bind_rows(train, test)
str(full)

### Exploring Titanic data set
# Age vs Survived
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  xlab("Age") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Survived")


# Sex vs Survived
ggplot(full[1:891,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")

tapply(full[1:891,]$Survived,full[1:891,]$Sex,mean)

#Sex vs Survived vs Age 
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  xlab("Age") +
  ylab("Count") +
  facet_grid(.~Sex)+
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Sex vs Survived")

##pclass vs sex vs survived
ggplot(full[1:891,], aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81))

#Fare
ggplot(full[1:891,], aes(x = Fare, y = Pclass)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))

##saving all graph in pdf
pdf("Titanic_Graph.pdf",width=7,height=5)
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + geom_histogram(bins=30) + 
  xlab("Age") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Survived")


# Sex vs Survived
ggplot(full[1:891,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")

tapply(full[1:891,]$Survived,full[1:891,]$Sex,mean)

#Sex vs Survived vs Age 
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  xlab("Age") +
  ylab("Count") +
  facet_grid(.~Sex)+
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Sex vs Survived")

##pclass vs sex vs survived
ggplot(full[1:891,], aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81))

#Fare
ggplot(full[1:891,], aes(x = Fare, y = Pclass)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))
dev.off()

# It appears that several of these variables should be represented as factors 
# and thus should be reclassified.
factor_variables <- c('PassengerId', 'Survived', 'Pclass', 'Sex', 'Embarked')
full[factor_variables] <- lapply(full[factor_variables], function(x) as.factor(x))

# names and tittles
# as names are so many and from looking at data it seems that name can be divided on 
#the
# basis of titles


names <- full$Name
## used gsub function to extract titles.
titles <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

#adding column "Titles" in full data set.
full$Titles <- titles

##finding all unique titles
unique(full$Titles)
# 18 titles , it is managable.

#divide these titles on the basis of Gender
# created a table having gender and titles
table(full$Sex, full$Title)
# some of the titles (Captain, Don, Dona, Jonkheer, Lady, Madame, etc.) are used once or twice. its seems that they represent some type of
# nobility

#lets see this on the basis of Pclas
#created a table having Pclass and titles
table(full$Pclass, full$Titles)

#titles like Don, Jonkheer, and Sir used only once and repersent male and come from same class , so merging them in "Sir"
# same with titles Dona, Lady, Madame:- "Lady"
# for thus agian use gsub function
full$Titles <- gsub("Dona|Lady|Madame|the Countess", "Lady", full$Titles)
full$Titles <- gsub("Don|Jonkheer|Sir", "Sir", full$Titles)

unique(full$Titles)
# now only 14 titles.


#titles should be factor not character
class(full$Titles)
full$Titles=as.factor(full$Titles)

# its is important to make family size as Sibsp, Parch variable are in data set.
full <- mutate(full, FamilySize = SibSp + Parch+1 )
hist(full$FamilySize, main="Histogram for Family Size" , xlab= "Family Size", col="Blue",
                      xlim=c(1,12))
# showing that most of the people travelling alone


# Mising data: data set contain so many NA , lets deal with thems
summary(full)
# look like we have missing values  in Age, Embarked, fare.

#lets start first with fare
full[(which(is.na(full$Fare))) , 1]
#  passenger no 1044 have missing valeu of fare
# lets check from where he begins his journey ansd in which class he is travelling
#first arrange data as pe passengerID
full1 <- arrange(full, PassengerId)

full1[1044, c(3, 12)]
# passenger travelling in class 3 and leave from Embark "S"

# lets see people of same class and having same Embark paid how much?
full1 %>%
  filter(Pclass == '3' & Embarked == 'S') %>%
  summarise(missing_fare = median(Fare, na.rm = TRUE))
# its comes out $8.05
# replace NA from this value in fare for passenger if 1044
full1$Fare[1044] <- 8.05

summary(full1$Fare)

# lets see whcih passenger not have Embark
full1$Embarked[full1$Embarked == ""] <- NA

full1[(which(is.na(full1$Embarked))), 1]
#passenger id 62, 830 not have listed Embark 

#check their class of ticket and fare 
full1[c(62, 830), c(1,3,10)]
## both passenger have first class ticket and paid $80

# let see for Embark haiving this combination , ticket class=1 and fare= 80
full1 %>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(mfare = median(Fare),
            n = n())
# from this its seems that both passenger starttheir journey from Embark "C" as 
# embark "C" with first class ticket have face near about $77
# replace NA value in Embark with "C"
full1$Embarked[c(62,830)] <- 'C'

# Missing Age
# for finding missing value of age a new package is installed to predict the missing value
# "mice'
install.packages("mice")
library(mice)

#on the basis of importanct factor predicting age for missing value in age column
set.seed(125)
mice_mod = mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin',
                                  'FamilySize','Titles','Survived')], method='rf')
#saving result
mice_output = complete(mice_mod)

# Replace Age variable from the mice model.
full1$Age = mice_output$Age
# Show new number of missing Age values
sum(is.na(full1$Age))
write.csv(file = "full.csv", row.names = FALSE)
write.csv(full1, "mydata_1.csv", row.names = FALSE)

#####################################################################################
#############        Modelling For Survival           ##############################
#####################################################################################
 
#for modelling Logestic regression is used

survived_Logestic= glm(Survived~Age+Pclass+Sex+SibSp+Parch+Fare, data=train, family = binomial)
summary(survived_Logestic)

##this is for getting probability in P(Y=1|X)
log_prob= predict(survived_Logestic, type = "response")
log_prob[1:20]


##sorting on the basis of survived or not survived
log_pred= rep("not_survived",891)
log_pred[log_prob>0.5]="survived"

##binomial matrix
table(log_pred, train$Survived)

##level of accuracy
(470+236)/891
for(i in c(1:11)) {
  train[,i] <- as.factor(train[,i])
}

test_as_factor= as.factor(test)
# Make predicted survival values
my_prediction_1 <- predict(survived_Logestic, test, type = "response")


# Create a data frame with two columns: PassengerId & Survived where Survived contains my predictions.
test_prediction_2 <- data.frame(PassengerID = test$PassengerId, 
                                Survived = my_prediction_1)

# Write the solution to a csv file 
write.csv(test_prediction_2, file = "my_solution_titanic_1.csv", row.names = FALSE)


# lets check the sensitivity of the model
# sensitivity= sum of true positive/ sum of condition positive
(470/(470+79))
# sensitivity of model is 85.61%

#specificity of model
#specificity = sum of true negative/ sum of condition negative
(236/(106+236))
#specificity is 69%


#let again fit the model after removing the non significant parameter 
# and further check model accuracy imporved or not
# from the first logistic model its come out the Parch and fare is not a significant parameter.
survived_Logestic_1= glm(Survived~Age+Pclass+Sex+SibSp, data=train, family = binomial)
summary(survived_Logestic_1)
log_prob_1= predict(survived_Logestic_1, type = "response")
log_prob_1[1:20]
log_pred_1= rep("not_survived",891)
log_pred_1[log_prob_1>0.5]="survived"
table(log_pred_1, train$Survived)
#accuracy
(347+132)/(891)
#53%
# with the removal of non significant parameter model accuracy reduces.

# sensitivity
(347)/(347+202)
#63%

#specificity
(132)/(210+132)
#38%

#lets try a new model with new cut off value.



#_________________________________________________________________________________
#__________________             KNN               ________________________________
#_________________________________________________________________________________

## covert the dependent variable to factor
## normalize the numeric vactor
train$Survived=factor(train$Survived)
numeric_variables= sapply(train, is.numeric)
train[numeric_variables]= lapply(train[numeric_variables], scale)


##selecting only those variable which are present in logistic model
my_variable= c("Age", "Pclass", "Sex","Sibsp","Parch","fare")
 summary(my_variable)
 
knn_1= knn(train, test, Survived, k=1)
## Error: no missing value allowed


train_knn=read.csv("train_knn.csv")
test_knn= read.csv("test_knn.csv")

#on the basis of importanct factor predicting age for missing value in age column
set.seed(120)
mice_mod_1 = mice(test[, !names(test) %in% c('PassengerId','Name','Ticket','Cabin'
                                           )], method='rf')
#saving result
mice_output_1 = complete(mice_mod_1)

# Replace Age variable from the mice model.
test$Age = mice_output_1$Age
# Show new number of missing Age values
sum(is.na(test$Age))

write.csv(test, "test_knn1.csv", row.names = FALSE)
test_knn1=read.csv("test_knn1.csv")


train_knn$Survived=factor(train_knn$Survived)
numeric_variables_1= sapply(train_knn, is.numeric)
train_knn[numeric_variables_1]= lapply(train_knn[numeric_variables_1], scale)

knn_model_1= knn(data.frame(train_knn), data.frame(test_knn1), Survived, k=1)
# Error: dims of 'test' and 'Train' differ
summary(train_knn)
summary(test_knn1)

dim(train_knn)
dim(test_knn1)
library(caret)
knn_model_2= knn((train_knn[,1:417]),(test_knn1), Survived, k=1)
length(Survived)

model=naiveBayes(Survived~., data=train_knn)
class(model)
summary(model)
print(model)
tbl_list=sapply(train_knn[-10], table,train_knn[ ,10])

data(train_knn)
sample_size= floor(0.75*nrow(train_knn))
set.seed(111)
train_data= sample(seq_len(nrow(train_knn)), size = sample_size)

data_train= train_knn[train_data, ]
data_test= train_knn[-train_data, ]

knn_model_dta_split= knn(data.frame(data_train), data.frame(data_test), Survived, k=1)
