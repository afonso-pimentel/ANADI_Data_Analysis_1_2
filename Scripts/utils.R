# =================================================================================================
# Utils
# =================================================================================================

create_decision_tree <- function(dataset, attribute, numeric_cols){
  
  # Load required libraries
  library(rpart)
  library(rpart.plot)
  
  # Split dataset into training and testing datasets
  set.seed(123)
  sample <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
  data.train <- dataset[sample,]
  data.test <- dataset[-sample,]
  
  # scale numeric columns
  data.train[numeric_cols] <- apply(data.train[numeric_cols], 2, minmaxnorm)
  data.test[numeric_cols] <- apply(data.test[numeric_cols], 2, minmaxnorm)
  
  # Construct the formula dynamically
  formula <- paste(attribute, "~ .")
  
  # Train the decision tree model using rpart
  tree.model <- rpart(formula,
                      data = data.train,
                      minsplit = 1,
                      cp = 0)
  
  # Plot the decision tree
  rpart.plot(
    tree.model,
    digits = 3,
    fallen.leaves = TRUE,
    type = 3
  )
  
  # Make predictions on the testing data using the trained tree model
  tree.pred = predict(tree.model, data.test, type = 'class')
  
  # Calculate accuracy and create confusion matrix
  m.conf <- table(data.test$Classe, tree.pred)
  acc <- 100 * round((m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf), 4)
  
  # Print the accuracy
  cat("Accuracy: ", acc, "%") #60%
  
  # Return the trained decision tree model
  return(tree.model)
}


# =================================================================================================

create_neural_network <- function(dataset, attribute, numeric_cols){
  
  # Load required libraries
  library(dplyr)
  library(neuralnet)
  
  # Identify the non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Use the mutate_at function from dplyr to apply the conversion to all of these columns
  dataset <- dataset %>%
    mutate_at(vars(non_numeric_cols), 
              list(~ as.numeric(factor(.))))
  
  # split the dataset into training and testing datasets
  set.seed(123)
  sample <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
  data.train <- dataset[sample,]
  data.test <- dataset[-sample,]
  
  # scale numeric columns
  data.train[numeric_cols] <- apply(data.train[numeric_cols], 2, minmaxnorm)
  data.test[numeric_cols] <- apply(data.test[numeric_cols], 2, minmaxnorm)
  
  # Prepare the formula for the neural network
  formula <- as.formula(paste(attribute, "~ ."))

  # Train the neural network model using neuralnet
  neuralnet_model <- neuralnet(formula,
                               data = data.train,
                               hidden = c(5, 3),  # Number of neurons in each hidden layer
                               linear.output = FALSE,  # Use nonlinear activation functions
                               threshold = 0.01,  # Convergence threshold
                               stepmax = 1e5  # Maximum number of steps
  )
  
  # Test the neural network model
  neuralnet_predictions <- compute(neuralnet_model, data.test)
  
  # Extract the predicted values
  predicted_values <- neuralnet_predictions$net.result
  
  # Convert the predicted values to binary classes if needed
  # For example, if the attribute is "Pro.level" with two classes: "Continental" and "World Tour"
  predicted_classes <- ifelse(predicted_values > 0.5, "World Tour", "Continental")
  
  # Compare predicted classes with actual classes
  actual_classes <- data.test[[attribute]]
  
  # Create a confusion matrix
  confusion_matrix <- table(actual_classes, predicted_classes)
  
  # Calculate accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
  
  # Print confusion matrix and accuracy
  cat("Confusion Matrix:\n")
  print(confusion_matrix)
  
  cat("\nAccuracy: ", accuracy, "%")
  
  return(neuralnet_model)
}

# =================================================================================================

create_knearest_model <- function(dataset, attribute, numeric_cols, k) {
  
  
  # Load required libraries
  library(dplyr)
  library(class)
  
  # Identify the non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Use the mutate_at function from dplyr to apply the conversion to all of these columns
  dataset <- dataset %>%
    mutate_at(vars(non_numeric_cols), 
              list(~ as.numeric(factor(.))))
  
  # split the dataset into training and testing datasets
  set.seed(123)
  sample <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
  data.train <- dataset[sample,]
  data.test <- dataset[-sample,]
  
  data.train[numeric_cols] <- apply(data.train[numeric_cols], 2, minmaxnorm)
  data.test[numeric_cols] <- apply(data.test[numeric_cols], 2, minmaxnorm)
  
  # Prepare the formula for the k-nearest neighbors model
  formula <- as.formula(paste(attribute, "~ ."))
  
  # Train the k-nearest neighbors model using knn.train from the class package
  library(class)
  knn_model <- knn(train = data.train[, -which(names(data.train) == attribute)],
                   test = data.test[, -which(names(data.test) == attribute)],
                   cl = data.train[, attribute],
                   k = k)
  
  # Make predictions on the testing data using the trained k-nearest neighbors model
  knn_predictions <- knn_model
  
  # Extract the actual classes from the testing data
  actual_classes <- data.test[[attribute]]
  
  # Create a confusion matrix
  confusion_matrix <- table(actual_classes, knn_predictions)
  
  # Calculate accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
  
  # Print confusion matrix and accuracy
  cat("Confusion Matrix:\n")
  print(confusion_matrix)
  
  cat("\nAccuracy: ", accuracy, "%")
  
  return(knn_model)
}

# =================================================================================================

create_multiple_regression_model <- function(dataset, attribute, numeric_cols) {
  set.seed(123)
  
  # Split data between test data and train data
  sample <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
  data.train <- dataset[sample,]
  data.test <- dataset[-sample,]
  
  # scale numeric columns
  data.train[numeric_cols] <- apply(data.train[numeric_cols], 2, minmaxnorm)
  data.test[numeric_cols] <- apply(data.test[numeric_cols], 2, minmaxnorm)
  
  # Remove attribute from possibly being in columns of multi regression
  attributes_for_regression <- setdiff(numeric_cols, attribute)
  
  # Create formula
  formula <- paste(attribute, "~", paste(attributes_for_regression, collapse = " + "))
  
  # Fit the regression model
  model <- lm(formula, data = data.train)
  
  # Assess the model
  summary(model)
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = data.test)
  
  # Calculate MAE and RMSE
  actual_values <- data.test[[attribute]]
  mae <- mean(abs(predictions - actual_values))
  rmse <- sqrt(mean((predictions - actual_values)^2))
  
  # Print MAE and RMSE
  cat("MAE Multiple Regression Model:", mae, "\n")
  cat("RMSE Multiple Regression Model:", rmse, "\n")
  
  # Return the model, predictions, MAE, and RMSE
  return(list(model = model, predictions = predictions, MAE = mae, RMSE = rmse))
}
# =================================================================================================

create_multiple_regression_model_max <- function(dataset, attribute) {
  # Load required libraries
  library(rpart)
  library(rpart.plot)
  library(fastDummies)
  library(stringr)
  
  # Identify non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Create dummy variables
  input = dummy_cols(as.data.frame(dataset), select_columns = non_numeric_cols, remove_selected_columns = TRUE)
  
  # Modify column names to replace spaces with underscores
  new_names <- colnames(input)
  new_names <- gsub(" ", "_", new_names)
  colnames(input) <- new_names
  
  # Retrieve the new attribute names with underscores
  new_attributes <- new_names[str_detect(new_names, paste(attribute))]
  
  # Generate the formula for regression model
  formula_names <- paste(new_names, collapse = " + ")
  finalformula <- paste(attribute, formula_names, sep = " ~ ")
  formula <- as.formula(finalformula)
  
  # Convert categorical columns to numeric
  input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
  
  # scale numeric columns
  input_scaled <- apply(input, 2, minmaxnorm)
  
  set.seed(123)
  
  # Split data between test data and train data
  sample <- sample(1:nrow(input), 0.7 * nrow(input))
  data.train <- input[sample, ]
  data.test <- input[-sample, ]
  
  # Fit the regression model
  model <- lm(formula, data = data.train)
  
  # Assess the model
  summary(model)
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = data.test)
  
  # Calculate MAE and RMSE
  actual_values <- data.test[[attribute]]
  mae <- mean(abs(predictions - actual_values))
  rmse <- sqrt(mean((predictions - actual_values)^2))
  
  # Print MAE and RMSE
  cat("MAE Multiple Regression Model:", mae, "\n")
  cat("RMSE Multiple Regression Model:", rmse, "\n")
  
  # Return the model, predictions, MAE, and RMSE
  return(list(model = model, predictions = predictions, MAE = mae, RMSE = rmse))
}

create_regression_tree_model <- function(dataset, attribute) {
  # Load required libraries
  library(rpart)
  library(rpart.plot)
  library(fastDummies)
  library(stringr)
  
  # Identify non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Create dummy variables
  input = dummy_cols(as.data.frame(dataset), select_columns = non_numeric_cols, remove_selected_columns = TRUE)
  
  # Modify column names to replace spaces with underscores
  new_names <- colnames(input)
  new_names <- gsub(" ", "_", new_names)
  colnames(input) <- new_names
  
  # Retrieve the new attribute names with underscores
  new_attributes <- new_names[str_detect(new_names, paste(attribute))]
  
  # Generate the formula for regression model
  formula_names <- paste(new_names, collapse = " + ")
  finalformula <- paste(attribute, formula_names, sep = " ~ ")
  formula <- as.formula(finalformula)

  # Convert categorical columns to numeric
  input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
  
  # scale numeric columns
  input_scaled <- apply(input, 2, minmaxnorm)
  
  summary(input_scaled)
  summary(input)
  
  set.seed(123)
  
  # Split data between test data and train data
  sample <- sample(1:nrow(input), 0.7 * nrow(input))
  data.train <- input[sample, ]
  data.test <- input[-sample, ]
  
  # Create formula
  # Fit the regression tree model
  model <- rpart(finalformula, data = data.train)
  
  # Plot the regression tree
  rpart.plot(model)
  
  # Assess the model
  printcp(model)
  
  # Make predictions on the test set
  predictions <- predict(model, newdata = data.test)
  
  # Calculate MAE and RMSE
  actual_values <- data.test[[attribute]]
  mae <- mean(abs(predictions - actual_values))
  rmse <- sqrt(mean((predictions - actual_values)^2))
  
  # Print MAE and RMSE
  cat("MAE Regression Tree Model:", mae, "\n")
  cat("RMSE Regression Tree Model:", rmse, "\n")
  
  # Return the model, predictions, MAE, and RMSE
  return(list(model = model, predictions = predictions, MAE = mae, RMSE = rmse))
}

# =================================================================================================

minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

minmaxdesnorm <- function(x, goal.attrib){
  return (x*(max(goal.attrib)-min(goal.attrib))+min(goal.attrib))
}

# =================================================================================================

