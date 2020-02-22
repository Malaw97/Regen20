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
