# Load the libraries after installation
library(data.table)

# Load the dataset
data <- fread("C:/Users/dell/Documents/machine/data.csv", fill = TRUE)

# Data Exploration
print(head(data) 
print(str(data)) 
summary(data)

# Ensure no rows with NA values remain
data <- na.omit(data)
# Handle missing values (if any) by replacing them with the mean
data$radius_mean[is.na(data$radius_mean)] <- mean(data$radius_mean, na.rm = TRUE)
data$texture_mean[is.na(data$texture_mean)] <- mean(data$texture_mean, na.rm = TRUE)

# Convert categorical values (M and B) to numeric
data$diagnosis <- ifelse(data$diagnosis == "M", 1, 0)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
data_split <- sample(1:nrow(data), size = floor(0.8 * nrow(data)))  # 80% for training set
train_data <- data[data_split, ]  # Training set
test_data <- data[-data_split, ]  # Testing set

# Check if train_data and test_data contain sufficient rows
if (nrow(train_data) == 0 || nrow(test_data) == 0) {
  stop("Train or Test data is empty. Check data splitting logic or dataset integrity.")
}

# Check for missing or invalid values in train_data
print(summary(train_data$radius_mean))
print(summary(train_data$texture_mean))

# Build the linear regression model
lm_model <- lm(texture_mean ~ radius_mean, data = train_data)

# Summarize the model
print(summary(lm_model))

# Predict on test data
predictions <- predict(lm_model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(mean((test_data$texture_mean - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Plot 1: Actual vs Predicted Values
plot(test_data$texture_mean, predictions,
     main = "Actual vs Predicted Values",
     xlab = "Actual Texture Mean",
     ylab = "Predicted Texture Mean",
     pch = 19, col = "blue")
abline(0, 1, col = "red")  # Add a reference line

# Plot 2: Residual Plot
residuals <- test_data$texture_mean - predictions
plot(predictions, residuals,
     main = "Residual Plot",
     xlab = "Predicted Values",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red")  # Add a horizontal reference line

# Plot 3: Linear Regression Line
plot(train_data$radius_mean, train_data$texture_mean,
     main = "Linear Regression Line",
     xlab = "Radius Mean",
     ylab = "Texture Mean",
     pch = 19, col = "blue")
abline(lm_model, col = "red")  # Add the regression line

# Plot 4: Scatter Plot
plot(train_data$radius_mean, train_data$texture_mean,
     main = "Scatter Plot of Radius Mean vs Texture Mean",
     xlab = "Radius Mean",
     ylab = "Texture Mean",
     pch = 19, col = "orange")

# Plot 5: Density Plot for Radius Mean
plot(density(train_data$radius_mean),
     main = "Density Plot of Radius Mean",
     xlab = "Radius Mean",
     ylab = "Density",
     col = "blue", lwd = 2)

# Plot 6: Density Plot for Texture Mean
plot(density(train_data$texture_mean),
     main = "Density Plot of Texture Mean",
     xlab = "Texture Mean",
     ylab = "Density",
     col = "red", lwd = 2)