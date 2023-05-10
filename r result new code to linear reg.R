train_data<-read.csv("C:\\Users\\Pearl\\OneDrive\\Desktop\\new test and train dataset\\msc_training_dataset.csv")
test_data<-read.csv("C:\\Users\\Pearl\\OneDrive\\Desktop\\new test and train dataset\\msc_testing_dataset.csv")
library(dplyr)
library(caret)
library(rsq)
library(ggplot2)
library(ggcorrplot)
library(car)
#Data pre-processing
str(train_data)
summary(train_data)
str(test_data)
summary(test_data)
any(is.na(train_data))
any(is.na(test_data))
#Fitting my regression model
model <- lm(price ~ room + bathroom + kitchen + french_door + backyard + furnished + green_paint + solar_power + woodfloor + qlm_security + club_access, data = train_data)
summary(model)
rsq(model)
# Predicting house prices using the model on the test data
predictions <- predict(model, newdata = test_data)

# Calculating the root mean squared error (RMSE) of the model
RMSE <- sqrt(mean((test_data$price - predictions)^2))
print(RMSE)
# Predict sale prices for test dataset using trained linear regression model
predicted_prices <- predict(model, newdata = test_data)

# Print predicted prices
print(predicted_prices)

# Extract coefficients from the model object
coef_df <- data.frame(feature = names(coef(model)[-1]),
                      importance = abs(coef(model)[-1]))

# Order the coefficients by importance
coef_df <- coef_df[order(coef_df$importance, decreasing = TRUE), ]

# Create a bar plot of the feature importances
ggplot(coef_df, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Feature") + ylab("Importance") +
  ggtitle("Feature Importances")
# histogram of predicted prices
ggplot(test_data, aes(x = predictions)) +
  geom_histogram() +
  xlab("Predicted Price") + ylab("Count") +
  ggtitle("Predicted Price Distribution")

ggplot(test_data, aes(x = predictions)) +
  geom_histogram(binwidth = 1000) +
  xlab("Predicted Price") + ylab("Count") +
  ggtitle("Predicted Price Distribution")

plot_data <- data.frame(actual = test_data$price, predicted = predictions)

ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  xlab("Actual Price") +
  ylab("Predicted Price") +
  ggtitle("Predicted Prices vs. Actual Prices")

