library(readxl)
library(caret)
library(dplyr)
df <- read_xlsx("regen20-dataset.xlsx")
df %>% head()
colnames(df)

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
