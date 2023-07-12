# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(nnet)

# Load iris data
data(iris)

# Check the first few rows of the dataset
head(iris)

# Check the structure of the dataset
str(iris)

# Summary statistics
summary(iris)

# Check the distribution of classes
table(iris$Species)

# Plot pair plot
pairs(iris[1:4], main = "Iris Data (red=setosa, green=versicolor, blue=virginica)",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

# Split the dataset into training set and test set
set.seed(42)  # for reproducibility
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainSet <- iris[trainIndex,]
testSet <- iris[-trainIndex,]

# Train a multinomial logistic regression model
model <- multinom(Species~., data=trainSet)

# Make predictions on the test set
predictions <- predict(model, newdata=testSet)

# Calculate the accuracy
accuracy <- sum(predictions == testSet$Species) / nrow(testSet)
print(paste("Accuracy: ", accuracy))

# Generate confusion matrix
confusionMatrix(predictions, testSet$Species)

