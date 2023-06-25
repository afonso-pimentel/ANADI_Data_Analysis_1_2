# =================================================================================================
# Utils K fold
# =================================================================================================

create_decision_tree_k_fold <- function(dataset, attribute, numeric_cols, nr_folds) {
  
  library(rpart)
  set.seed(123)
  
  # Initialize vectors to store accuracy, sensitivity, and precision values
  accuracy <- numeric(nr_folds)
  sensitivity <- numeric(nr_folds)
  precision <- numeric(nr_folds)
  specificity <- numeric(nr_folds)
  f1_score <- numeric(nr_folds)
  
  folds <- rep_len(1:nr_folds, nrow(dataset))
  folds <- sample(folds, length(folds))
  
  # Initialize list to store decision tree models
  tree_models <- list()
  
  # Perform k-fold cross-validation
  for (k in 1:nr_folds) {
    # Actual split of the data
    fold <- which(folds == k)
    data.train <- dataset[fold, ]
    data.test <- dataset[-fold, ]
    
    # Scale numeric columns
    data.train[numeric_cols] <- apply(data.train[numeric_cols], 2, minmaxnorm)
    data.test[numeric_cols] <- apply(data.test[numeric_cols], 2, minmaxnorm)
    
    # Train the decision tree model using rpart
    tree.model <- rpart(as.formula(paste(attribute, "~ .")),
                        data = data.train,
                        method = "class",
                        minsplit = 1,
                        cp = 0)
    
    # Make predictions on the testing data using the trained tree model
    tree.pred <- predict(tree.model, data.test, type = 'class')
    
    # Create confusion matrix
    m.conf <- table(data.test[[attribute]], tree.pred)
    
    # Calculate accuracy, sensitivity, precision and specificity
    accuracy[k] <- sum(diag(m.conf)) / sum(m.conf)
    sensitivity[k] <- m.conf[1, 1] / sum(m.conf[1, ])
    precision[k] <- m.conf[1, 1] / sum(m.conf[, 1])
    specificity[k] <- m.conf[2, 2] / sum(m.conf[1, ])
    
    # Calculate F1 score
    f1_score[k] <- 2 * (precision[k] * sensitivity[k]) / (precision[k] + sensitivity[k])
    
    # Store the decision tree model
    tree_models[[k]] <- tree.model
  }
  
  # Find the index of the model with the highest accuracy
  best_model_index <- which.max(accuracy)
  
  # Retrieve the best decision tree model
  best_tree_model <- tree_models[[best_model_index]]
  
  # Print the performance metrics for the best model
  cat("Best Model Metrics:\n")
  cat("Accuracy: ", 100*round(accuracy[best_model_index], 2), "%\n")
  cat("Sensitivity (Recall): ", round(sensitivity[best_model_index], 3), "\n")
  cat("Precision: ", round(precision[best_model_index], 3), "\n")
  cat("Specificity: ", round(specificity[best_model_index], 3), "\n")
  cat("F1 score: ", round(f1_score[best_model_index], 3), "\n")
  
  # Print mean and standard deviation of accuracy
  cat("Mean Accuracy: ",mean(accuracy), "%\n")
  cat("Standard Deviation of Accuracy: ", round(sd(accuracy), 4), "\n")
  
  cat("Mean Sensitivity (Recall): ", round(mean(sensitivity), 3), "\n")
  cat("Standard Deviation of Sensitivity (Recall): ", round(sd(sensitivity), 3), "\n")
  
  cat("Mean Precision: ", round(mean(precision), 3), "\n")
  cat("Standard Deviation of Precision: ", round(sd(precision), 3), "\n")
  
  cat("Mean Specificity: ", round(mean(specificity), 3), "\n")
  cat("Standard Deviation of Specificity: ", round(sd(specificity), 3), "\n")
  
  cat("Mean F1 score: ", round(mean(f1_score), 3), "\n")
  cat("Standard Deviation of F1 score: ", round(sd(f1_score), 3), "\n")
  
  # Return the best decision tree model
  return(best_tree_model)
}


# =================================================================================================

create_neural_network_k_fold <- function(dataset, attribute, numeric_cols, nr_folds, hidden_levels) {
  
  library(dplyr)
  library(neuralnet)
  library(fastDummies)
  library(stringr)
  
  # Retrieve unique values of the specified attribute
  attribute.values <- unique(eval(parse(text = paste("dataset$", attribute))))
  
  # Identify non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Create dummy variables
  if(length(non_numeric_cols) == 0){
    input <- dataset
  } else{
    input = dummy_cols(as.data.frame(dataset), select_columns = non_numeric_cols, remove_selected_columns = TRUE)
  }
  
  # Modify column names to replace spaces with underscores
  new_names <- colnames(input)
  new_names <- gsub(" ", "_", new_names)
  colnames(input) <- new_names
  
  # Retrieve the new attribute names with underscores
  new_attributes <- new_names[str_detect(new_names, paste(attribute))]
  
  # Generate the formula for neural network model
  formula_names <- paste(new_attributes, collapse = " + ")
  finalformula <- paste(formula_names, "~ .")
  formula <- as.formula(finalformula)
  
  # Convert categorical columns to numeric
  input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
  
  # scale numeric columns
  input_scaled <- apply(input, 2, minmaxnorm)
  
  set.seed(123)
  
  # Initialize vectors to store accuracy values
  accuracy <- numeric(nr_folds)
  sensitivity <- numeric(nr_folds)
  precision <- numeric(nr_folds)
  specificity <- numeric(nr_folds)
  f1_score <- numeric(nr_folds)
  
  
  folds <- rep_len(1:nr_folds, nrow(dataset))
  folds <- sample(folds, length(folds))
  
  # Initialize list to store neural network models
  neuralnet_models <- list()
  
  # Perform k-fold cross-validation
  for (k in 1:nr_folds) {
    # Actual split of the data
    fold <- which(folds == k)
    data.train <- input_scaled[fold, ]
    data.test <- input_scaled[-fold, ]
    
    # Train the neural network model using neuralnet
    neuralnet_model <- neuralnet(formula,
                                 data = data.train,
                                 hidden = hidden_levels,  # Number of neurons in each hidden layer
                                 linear.output = FALSE
    )
    
    # Test the neural network model
    neuralnet_predictions <- compute(neuralnet_model, data.test)
    
    # Extract the predicted values
    predict_result <- round(neuralnet_predictions$net.result, digits = 0)
    
    net.prediction = attribute.values[apply(predict_result,1, which.max)]
    
    # Compare predicted classes with actual classes
    actual_classes <- data.test[new_attributes]
    
    # create confusion matrix
    predict.table = table(eval(parse(text = paste("dataset$", attribute)))[-fold], net.prediction)
    print(predict.table)
    
    # Calculate accuracy
    hit.rate <- 100*(predict.table[1,1]+predict.table[2,2])/sum(predict.table)
    accuracy[k] <- hit.rate
    
    # Calculate Specificity
    specificity[k] <- predict.table[2, 2] / sum(predict.table[2, ])
    
    # Calculate Precision
    precision[k] <- predict.table[1, 1] / sum(predict.table[, 1])
    
    # Calculate Sensitivity
    sensitivity[k] <- predict.table[1, 1] / sum(predict.table[1,])
      
    # Calculate F1 Score
    f1_score[k] <- 2 * (precision[k] * sensitivity[k]) / (precision[k] + sensitivity[k])
    
    # Store the neural network model
    neuralnet_models[[k]] <- neuralnet_model
  }
  
  # Find the index of the model with the highest accuracy
  best_model_index <- which.max(accuracy)
  
  # Retrieve the best neural network model
  best_neuralnet_model <- neuralnet_models[[best_model_index]]
  
  # Print the performance metrics for the best model
  cat("Best Model Metrics:\n")
  cat("Accuracy: ", round(accuracy[best_model_index], 2), "%\n")
  cat("Sensitivity (Recall): ", round(sensitivity[best_model_index] * 100, 3), "%\n")
  cat("Precision: ", round(precision[best_model_index] * 100, 3), "%\n")
  cat("Specificity: ", round(specificity[best_model_index] * 100, 3), "%\n")
  cat("F1 score: ", round(f1_score[best_model_index] * 100, 3), "%\n")
  
  # Print mean and standard deviation of accuracy
  cat("Mean Accuracy: ",mean(accuracy), "%\n")
  cat("Standard Deviation of Accuracy: ", round(sd(accuracy), 4), "\n")
  
  cat("Mean Sensitivity (Recall): ", round(mean(sensitivity), 3), "\n")
  cat("Standard Deviation of Sensitivity (Recall): ", round(sd(sensitivity), 3), "\n")
  
  cat("Mean Precision: ", round(mean(precision), 3), "\n")
  cat("Standard Deviation of Precision: ", round(sd(precision), 3), "\n")
  
  cat("Mean Specificity: ", round(mean(specificity), 3), "\n")
  cat("Standard Deviation of Specificity: ", round(sd(specificity), 3), "\n")
  
  cat("Mean F1 score: ", round(mean(f1_score), 3), "\n")
  cat("Standard Deviation of F1 score: ", round(sd(f1_score), 3), "\n")
  
  # Return the best neural network model
  return(best_neuralnet_model)
}

create_neural_network_k_fold_NUMERIC_PREDICTION <- function(dataset, attribute, nr_folds, hidden_levels) {
  
  library(dplyr)
  library(neuralnet)
  library(fastDummies)
  library(stringr)
  
  # Retrieve unique values of the specified attribute
  attribute.values <- unique(eval(parse(text = paste("dataset$", attribute))))
  
  # Identify non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Create dummy variables
  if(length(non_numeric_cols) == 0){
    input <- dataset
  } else{
    input = dummy_cols(as.data.frame(dataset), select_columns = non_numeric_cols, remove_selected_columns = TRUE)
  }
  
  # Modify column names to replace spaces with underscores
  new_names <- colnames(input)
  new_names <- gsub(" ", "_", new_names)
  colnames(input) <- new_names
  
  # Retrieve the new attribute names with underscores
  new_attributes <- new_names[str_detect(new_names, paste(attribute))]
  
  # Generate the formula for neural network model
  formula_names <- paste(new_attributes, collapse = " + ")
  finalformula <- paste(formula_names, "~ .")
  formula <- as.formula(finalformula)
  
  # Convert categorical columns to numeric
  input <- as.data.frame(lapply(input, function(x) as.numeric(as.character(x))))
  
  # Scale numeric columns
  input_scaled <- apply(input, 2, minmaxnorm)
  set.seed(123)
  
  mae <- numeric(nr_folds)  # Store MAE values
  rmse <- numeric(nr_folds)  # Store RMSE values
  
  folds <- rep_len(1:nr_folds, nrow(dataset))
  folds <- sample(folds, length(folds))
  
  # Initialize list to store neural network models
  neuralnet_models <- list()
  
  # Perform k-fold cross-validation
  for (k in 1:nr_folds) {
    # Actual split of the data
    fold <- which(folds == k)
    data.train <- input_scaled[fold, ]
    data.test <- input_scaled[-fold, ]
    
    # Train the neural network model using neuralnet
    neuralnet_model <- neuralnet(
      formula,
      data = data.train,
      hidden = hidden_levels,
      linear.output = TRUE
    )
    
    # Test the neural network model
    neuralnet_predictions <- compute(neuralnet_model, data.test)
    concatenated <- paste(neuralnet_predictions, collapse = " ")
    
    # Extract the predicted values
    normalized_predictions <- lapply(neuralnet_predictions$net.result, minmaxdesnorm, goal.attrib = dataset$vo2_results)

    predict_result <- lapply(normalized_predictions, round, digits = 0)
    
    net.prediction = predict_result
    
    # Compare predicted classes with actual classes
    actual_values <- dataset[-fold, attribute]
    
    actual_values <- as.numeric(actual_values)
    net.prediction <- as.numeric(net.prediction)
    
    # Calculate MAE
    mae[k] <- mean(abs(net.prediction - actual_values))
    
    # Calculate RMSE
    rmse[k] <- sqrt(mean((net.prediction - actual_values)^2))
    
    # Store the neural network model
    neuralnet_models[[k]] <- neuralnet_model
  }
  
  # Find the index of the model with the least MAE
  best_model_index <- which.min(mae)
  
  # Retrieve the best neural network model
  best_neuralnet_model <- neuralnet_models[[best_model_index]]
  
  # Print the performance metrics for the best model
  cat("Best Model Metrics:\n")
  
  # Print mean and standard deviation of MAE
  cat("Mean Absolute Error (MAE): ", round(mean(mae), 3), "\n")
  
  # Print mean and standard deviation of RMSE
  cat("Root Mean Square Error (RMSE): ", round(mean(rmse), 3), "\n")
  
  # Return the best neural network model
  return(best_neuralnet_model)
}


# =================================================================================================

create_knearest_model_k_fold <- function(dataset, attribute, numeric_cols, nr_folds) {
  
  # Load required libraries
  library(dplyr)
  library(class)
  
  # Retrieve unique values of the specified attribute
  attribute.values <- unique(eval(parse(text = paste("dataset$", attribute))))
  
  # Identify the non-numeric columns
  non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
  
  # Use the mutate_at function from dplyr to apply the conversion to all of these columns
  dataset <- dataset %>%
    mutate_at(vars(non_numeric_cols), 
              list(~ as.numeric(factor(.))))
  
  # Create indices for cross-validation folds
  set.seed(123)
  indices <- sample(1:nrow(dataset))  # Randomly shuffle the indices
  fold_size <- ceiling(nrow(dataset) / nr_folds)  # Calculate size of each fold
  folds <- rep(1:nr_folds, each = fold_size, length.out = nrow(dataset))  # Assign indices to folds
  
  
  # Initialize vectors to store accuracy values and k values
  accuracy <- numeric()
  k_values <- numeric()
  
  # Perform k-fold cross-validation
  for (k in 1:nr_folds) {
    # Actual split of the data
    fold <- which(folds == k)
    data.train <- dataset[fold, ]
    data.test <- dataset[-fold, ]
    
    data.train[numeric_cols] <- apply(data.train[numeric_cols], 2, minmaxnorm)
    data.test[numeric_cols] <- apply(data.test[numeric_cols], 2, minmaxnorm)
    
    # Prepare the formula for the k-nearest neighbors model
    formula <- as.formula(paste(attribute, "~ ."))
    
    # Initialize vectors to store accuracy values for different k
    accuracy_k <- numeric()
    
    # Find the best k value
    for (i in seq(1, 50, 2)) {
      knn_model <- knn(train = data.train[, -which(names(data.train) == attribute)],
                       test = data.test[, -which(names(data.test) == attribute)],
                       cl = data.train[, attribute],
                       k = i)
      
      knn_predictions <- knn_model
      
      actual_classes <- data.test[[attribute]]
      
      confusion_matrix <- table(actual_classes, knn_predictions)
      
      accuracy_k <- c(accuracy_k, sum(diag(confusion_matrix)) / sum(confusion_matrix))
    }
    
    # Find the best k value for this fold
    best_k <- seq(1, 50, 2)[which.max(accuracy_k)]
    
    # Store the accuracy and k value for this fold
    accuracy <- c(accuracy, max(accuracy_k))
    k_values <- c(k_values, best_k)
  }
  
  # Find the overall best k value
  best_k <- k_values[which.max(accuracy)]
  
  # Train the k-nearest neighbors model using the best k value
  knn_model <- knn(train = dataset[, -which(names(dataset) == attribute)],
                   test = dataset[, -which(names(dataset) == attribute)],
                   cl = dataset[, attribute],
                   k = best_k)
  
  # Make predictions on the testing data using the trained k-nearest neighbors model
  knn_predictions <- knn_model
  
  # Extract the actual classes from the testing data
  actual_classes <- dataset[[attribute]]
  
  # Create a confusion matrix
  confusion_matrix <- table(factor(actual_classes, levels = 1:length(attribute.values), labels = attribute.values),
                            factor(knn_predictions, levels = 1:length(attribute.values), labels = attribute.values))
  
  # Calculate accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
  
  # Print confusion matrix and accuracy
  cat("Confusion Matrix:\n")
  print(confusion_matrix)
  
  # Calculate Sensitivity (Recall)
  sensitivity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  # Calculate Specificity
  specificity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  
  # Calculate Precision
  precision <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
  
  # Calculate F1 score
  f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  # Print the performance metrics for the best model
  cat("Best Model Metrics:\n")
  cat("Accuracy: ", round(accuracy, 2), "%\n")
  cat("Sensitivity (Recall): ", round(sensitivity, 3), "\n")
  cat("Precision: ", round(precision, 3), "\n")
  cat("Specificity: ", round(specificity, 3), "\n")
  cat("F1 score: ", round(f1_score, 3), "\n")
  
  return(knn_model)
}

# =================================================================================================

minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

minmaxdesnorm <- function(x, goal.attrib){
  return (x*(max(goal.attrib)-min(goal.attrib))+min(goal.attrib))
}


# =================================================================================================


