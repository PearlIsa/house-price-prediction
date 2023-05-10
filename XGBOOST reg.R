train_data<-read.csv("C:\\Users\\Pearl\\OneDrive\\Desktop\\new test and train dataset\\msc_training_dataset.csv")
test_data<-read.csv("C:\\Users\\Pearl\\OneDrive\\Desktop\\new test and train dataset\\msc_testing_dataset.csv")
library(xgboost)
library(caret)
xgb_train <- xgb.DMatrix(data = as.matrix(train_data[, -14]), label = train_data$price)
xgb_test <- xgb.DMatrix(data = as.matrix(test_data[, -14]), label = test_data$price)



xgb_model <- xgboost(data = xgb_train, 
                     nrounds = 100,
                     objective = "reg:squarederror",
                     max_depth = 5, 
                     eta = 0.1, 
                     gamma = 0.5, 
                     colsample_bytree = 0.7,
                     min_child_weight = 1, 
                     subsample = 0.7)
xgb_predictions <- predict(xgb_model, newdata = as.matrix(test_data[, -14]))


xgb_rmse <- RMSE(xgb_predictions, test_data$price)
xgb_r_squared <- cor(xgb_predictions, test_data$price)^2
print(xgb_rmse)
print(xgb_r_squared)

# Predict on test data using the xgboost model
xgb_predictions <- predict(xgb_model, newdata = as.matrix(test_data[, -14]))

# Print the predicted prices
print(xgb_predictions)

# Get feature importance
importance <- xgb.importance(colnames(train_data[, -14]), model = xgb_model)
print(importance)
