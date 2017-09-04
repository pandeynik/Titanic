######################################################################################
######################## Random Forest  #############################################
######################################################################################

#First things first, I need to split out the test and training data back into separate data sets, 
# now called train_complete and test_complete.
train_complete <- full[full1$Dataset == 'train', ]
test_complete <- full[full1$Dataset == 'test', ]
#using caret package to use random Forest

# creating a system that will perform 10 repeats of a 10-Fold cross-validation of the data.
myControl = trainControl(
  method = "cv", 
  number = 10,
  repeats = 10, 
  verboseIter = TRUE
)


#fitting Random Forest
rf_model <- train(
  Survived ~ Age + Pclass + Sex + SibSp + Parch + Fare + Embarked + Titles + FamilySize,
  tuneGrid = data.frame(mtry = c(2, 5, 8, 10, 15)),
  data = train_complete, 
  method = "ranger", 
  trControl = myControl,
  importance = 'impurity'
)