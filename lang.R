library(readxl)
library(caret)
library(dplyr)
library(ggplot2)

df <- read_xlsx("regen20-dataset.xlsx")
df %>% head()
colnames(df)

#check if there is any missing data
df %>% is.na() %>%
  sum()

#check the distribution of age
summary(as.factor(df$age))
df$age <- as.factor(df$age)

#check the number of patients
summary(as.factor(df$target))
df$target <- as.factor(df$target)


#look at how cardiovascular disease is spread among people's age
g <- ggplot(df, aes(age, fill = target))
g + geom_bar(position = 'dodge') +
  labs(title = 'age vs target') +
  scale_fill_manual(labels = c('healthy','disease'), values = c('deepskyblue1', 'orangered2'))


#svm
df$target <- as.factor(df$target)
intrain <- createDataPartition(y = df$target, p= 0.7, list = FALSE)
training <- df[intrain,]
testing <- df[-intrain,]
dim(testing)
dim(training)

anyNA(df)
summary(df)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(target ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

test_pred <- predict(svm_Linear, newdata = testing)
test_pred
table(test_pred, testing$target)
confusionMatrix(table(test_pred, testing$target))


grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(target ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)



test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid

confusionMatrix(table(test_pred_grid, testing$target))


#naive Bayesian
library(e1071)
#dont factorize age in this model
Naive_Bayes_Model = naiveBayes(target ~.,data = training)
Naive_Bayes_Model
NB_Predictions = predict(Naive_Bayes_Model, testing)
confusionMatrix(table(NB_Predictions,testing$target))


#decision tree
library(rpart.plot)
df %>% head()
#do we need to normalize continuous data and as.factor() categorical data?
trctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(target ~.,data = training, method = 'rpart',
                   parms = list(split = 'information'),
                   trControl = trctrl,
                   tuneLength = 10)
dtree_fit
prp(dtree_fit$finalModel, box.palette = 'Reds', tweak = 1.2)

test_pred_dt <- predict(dtree_fit, newdata= testing)
confusionMatrix(test_pred_dt,testing$target)



#random forest
library(randomForest)
set.seed(100)
rf <- randomForest(target ~., data = training, importance = TRUE, ntree = 500, mtry = 6)
rf
predTrain <- predict(rf, training, type = 'class')
table(predTrain,training$target)
predtest <- predict(rf,testing, type = 'class')
table(predtest, testing$target)
confusionMatrix(table(predtest, testing$target))
