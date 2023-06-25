# =================================================================================================
# Dataframe set
# =================================================================================================
# 1. import the data
DATA1 <- read.csv("./assets/ciclismo.csv")
# use setwd() to set the work directory on the console before running the readcsv command
# setwd('C:/Users/afons/Documents/ANADI_REPO/Scripts')

# Load the functions from utils.R
source("utils.R")
source("utils_k_fold.R")

# =================================================================================================
# 2. Get age from date of birth
current_date <- Sys.Date()

# Convert dob to Date type
DATA1$dob <- as.Date(DATA1$dob, format = "%Y-%m-%d")

# Calculate age and assign it to a new column
DATA1$age <- as.integer((current_date - DATA1$dob) / 365.25)
# =================================================================================================

# lets proceed to the cleaning, with an automatized process
clean_outliers <- function(data) {
  has_outliers <- TRUE
  
  while (has_outliers) {
    # Identify outliers in altitude_results
    altitude_outliers <- boxplot.stats(data$altitude_results)$out
    
    # Identify outliers in vo2_results
    vo2_outliers <- boxplot.stats(data$vo2_results)$out
    
    # Identify outliers in hr_results
    hr_outliers <- boxplot.stats(data$hr_results)$out
    
    # Create a logical condition to filter out rows with outliers
    filter_condition <- !(data$altitude_results %in% altitude_outliers |
                            data$vo2_results %in% vo2_outliers |
                            data$hr_results %in% hr_outliers)
    
    # Remove rows with outliers
    data <- data[filter_condition, ]
    
    # Check if there are still outliers
    has_outliers <- any(altitude_outliers) || any(vo2_outliers) || any(hr_outliers)
  }
  
  # Return the cleaned dataset
  return(data)
}

# run the cleaning function
DATA1 <- clean_outliers(DATA1)

# remove the ID and dob column
DATA1 <- DATA1[, c(2:9,11,12)]

# define the numeric columns we will want to normalize
numeric_cols <- c("altitude_results","vo2_results","hr_results","age")

# =================================================================================================
# exercise 4.2 - Classification
#
# Let's first try and create general functions to analyze
# through the three diferent methods:
# - decision trees
# - neural networks
# - K- nearest neighbours

# =================================================================================================
# Exercise 4.2.1
# study Pro.level
# Full model (model 1) - pro.level ~ .
DATA_MODEL1 <- DATA1
# decision tree
prolevel_tree_model1 <- create_decision_tree_k_fold(DATA_MODEL1,"Pro.level", numeric_cols, 5)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  60 %
# Sensitivity (Recall):  0.357 
# Precision:  0.412 
# Specificity:  1.357 
# F1 score:  0.383 
# Mean Accuracy:  0.5732693 %
# Standard Deviation of Accuracy:  0.0199 
# Mean Sensitivity (Recall):  0.405 
# Standard Deviation of Sensitivity (Recall):  0.09 
# Mean Precision:  0.382 
# Standard Deviation of Precision:  0.024 
# Mean Specificity:  1.275 
# Standard Deviation of Specificity:  0.087 
# Mean F1 score:  0.389 
# Standard Deviation of F1 score:  0.041 
# =============================

# neural network
nr_folds <- 5
hidden_levels <- 5
prolevel_neuralnet_model1 <- create_neural_network_k_fold(DATA_MODEL1,"Pro.level", numeric_cols, nr_folds, hidden_levels)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  63.85 %
# Sensitivity (Recall):  33.213 %
# Precision:  47.423 %
# Specificity:  80.271 %
# F1 score:  39.066 %
# Mean Accuracy:  58.96383 %
# Standard Deviation of Accuracy:  3.0029 
# Mean Sensitivity (Recall):  0.392 
# Standard Deviation of Sensitivity (Recall):  0.047 
# Mean Precision:  0.402 
# Standard Deviation of Precision:  0.041 
# Mean Specificity:  0.693 
# Standard Deviation of Specificity:  0.063 
# Mean F1 score:  0.394 
# Standard Deviation of F1 score:  0.02 
# =============================

# k-nearest-neighbours
nr_folds <- 5
prolevel_knearest_model1 <- create_knearest_model_k_fold(DATA_MODEL1,"Pro.level", numeric_cols, nr_folds)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  67.98 %
# Sensitivity (Recall):  0.239 
# Precision:  0.574 
# Specificity:  0.908 
# F1 score:  0.338 
# =============================
# =================================================================================================
# Simpler Model (model 2) - pro.level ~ gender + Team + Background + Winter.Training.Camp + altitude_results + Continent
DATA_MODEL2 <- DATA1[,c("gender","Team","Background","Pro.level","Winter.Training.Camp","altitude_results","Continent")]
numeric_cols <- c("altitude_results")

# decision tree
prolevel_tree_model2 <- create_decision_tree_k_fold(DATA_MODEL2,"Pro.level", numeric_cols, 5)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  59 %
# Sensitivity (Recall):  0.413 
# Precision:  0.415 
# Specificity:  1.297 
# F1 score:  0.414 
# Mean Accuracy:  0.5833316 %
# Standard Deviation of Accuracy:  0.0093 
# Mean Sensitivity (Recall):  0.392 
# Standard Deviation of Sensitivity (Recall):  0.064 
# Mean Precision:  0.39 
# Standard Deviation of Precision:  0.021 
# Mean Specificity:  1.318 
# Standard Deviation of Specificity:  0.035 
# Mean F1 score:  0.389 
# Standard Deviation of F1 score:  0.036 
# =============================

# neural network
nr_folds <- 5
hidden_levels <- 12
prolevel_neuralnet_model2 <- create_neural_network_k_fold(DATA_MODEL2,"Pro.level", numeric_cols, nr_folds, hidden_levels)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  64.74 %
# Sensitivity (Recall):  42.599 %
# Precision:  49.372 %
# Specificity:  76.596 %
# F1 score:  45.736 %
# Mean Accuracy:  58.61144 %
# Standard Deviation of Accuracy:  3.5746 
# Mean Sensitivity (Recall):  0.44 
# Standard Deviation of Sensitivity (Recall):  0.051 
# Mean Precision:  0.407 
# Standard Deviation of Precision:  0.049 
# Mean Specificity:  0.663 
# Standard Deviation of Specificity:  0.068 
# Mean F1 score:  0.42 
# Standard Deviation of F1 score:  0.03 
# =============================

# k-nearest-neighbours
nr_folds <- 5
prolevel_knearest_model2 <- create_knearest_model_k_fold(DATA_MODEL2,"Pro.level", numeric_cols, nr_folds)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  69.28 %
# Sensitivity (Recall):  0.292 
# Precision:  0.604 
# Specificity:  0.901 
# F1 score:  0.394 
# =============================
# =================================================================================================
# a)
library(rpart.plot)
# tree model comparisons
# model 1
rpart.plot(prolevel_tree_model1)
# model 2
rpart.plot(prolevel_tree_model2)

# neural net comparisons
# model 1
plot(prolevel_neuralnet_model1)
# model 2
plot(prolevel_neuralnet_model2)

# =================================================================================================
# b)
# The K-Nearest Neighbours is known as lazy learning, as it does not actually train and create a model.
# It simply stores the training data points in memory and uses them during the prediction phase.
# Despite there not being any training time in K-NN, it uses a lot of resources depending on the size of the
# training dataset. Each time we want to make a prediction, the algorithm simply tries to find the nearest neighbours
# in the entire training dataset.

# https://sebastianraschka.com/faq/docs/lazy-knn.html

# =================================================================================================
# c)
# Extract the accuracy values for the k-nearest neighbors and neural network models
accuracy_knn <- c(67.98, 69.28)  # Replace with the accuracy values
accuracy_nn <- c(63.85, 64.74)   # Replace with the accuracy values

# Perform the t-test
t_test <- t.test(accuracy_knn, accuracy_nn)

# Print the t-test results
print(t_test)
# =============================
# RESULTS 
# =============================
# Welch Two Sample t-test
# 
# data:  accuracy_knn and accuracy_nn
# t = 5.5031, df = 1.7686, p-value = 0.04096
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.4830266 8.1869734
# sample estimates:
#   mean of x mean of y 
# 68.630    64.295 
# =============================

# =================================================================================================
# Exercise 4.2.2
# study Winter.Training.Camp
# Create decision tree models for different combinations of folds and attributes
DATA <- DATA1[, c(5,6,7,8)]
number_of_folds <- 5
cat('Decision Tree with only Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

cat('Decision Tree with every attributes and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA1,"Winter.Training.Camp", numeric_cols, number_of_folds)

DATA <- DATA1[, c(4,5,6,7,8)]
cat('Decision Tree with only Pro Level, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

DATA <- DATA1[, c(2,5,6,7,8)]
cat('Decision Tree with only Team, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

DATA <- DATA1[, c(1,5,6,7,8)]
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

DATA <- DATA1[, c(1,5,6,7,8,10)]
cat('Decision Tree with only Continent, Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

## Test best regression tree(Gender, Altitude Results, Heart Rate Results, vo2_results) with different folds
DATA <- DATA1[, c(1,5,6,7,8)]
number_of_folds <- 4
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

number_of_folds <- 3
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

number_of_folds <- 2
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

number_of_folds <- 8
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

number_of_folds <- 7
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

number_of_folds <- 6
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

number_of_folds <- 5
cat('Decision Tree with only Gender, Altitude Results, Heart Rate Results, vo2_results and ', number_of_folds,'folds\n')
winter_tree_model <- create_decision_tree_k_fold(DATA,"Winter.Training.Camp", numeric_cols, number_of_folds)

### Create Neural Network models for different combinations of folds, hidden layers and attributes
numeric_cols <- c('altitude_resuts', 'vo2_results','hr_results')
DATA <- DATA1[, c(1,5,6,7,8)]
nr_folds <- 2
hidden_levels <- 2
cat('Neural network with only Gender, Altitude Results, Heart Rate Results, vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)

DATA <- DATA1[, c(5,6,7,8)]
cat('Neural network with only Altitude Results, Heart Rate Results, vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)

DATA <- DATA1[, c(5,7)]
cat('Neural network with only vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)

DATA <- DATA1[, c(1,4,5,6,7,8)]
cat('Neural network with only ProLevel, Gender, Altitude Results, Heart Rate Results, vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)

DATA <- DATA1[, c(1,2,4,5,6,7,8)]
cat('Neural network with only Team, ProLevel, Gender, Altitude Results, Heart Rate Results, vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)
plot(winter_neuralnet_model)

DATA <- DATA1[, c(1,2,3, 4,5,6,7,8)]
cat('Neural network with only Background, Team, ProLevel, Gender, Altitude Results, Heart Rate Results, vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)

DATA <- DATA1[, c(1,2,4,5,6,7,8,10)]
cat('Neural network with only Continent, Team, ProLevel, Gender, Altitude Results, Heart Rate Results, vo2_results, ', nr_folds,'folds and ', hidden_levels,' hidden layers.\n')
winter_neuralnet_model <- create_neural_network_k_fold(DATA,"Winter.Training.Camp", numeric_cols, nr_folds, hidden_levels)

# Test with a significance level of 5% if there are any significant differences between the best models of each model type

## Neural network best model performance metrics
neural_network <- c(73.24, 47.37, 65.32, 86.86, 54.92)

## Decision tree best model performance metrics
decision_tree <- c(66, 48.1, 52.6, 72.5, 50.2)

# Perform the t-test
result <- t.test(neural_network, decision_tree, paired = FALSE)

# Print the test result
print(result)

# =================================================================================================
# Exercise 4.2.3
# study gender
# Full model (model 1) - gender ~ .
DATA_MODEL3 <- DATA1
# decision tree
gender_tree_model1 <- create_decision_tree_k_fold(DATA_MODEL3,"gender", numeric_cols, 5)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
#   Accuracy:  78 %
# Sensitivity (Recall):  0.813 
# Precision:  0.766 
# Specificity:  0.707 
# F1 score:  0.789 
# Mean Accuracy:  0.7293582 %
# Standard Deviation of Accuracy:  0.0336 
# Mean Sensitivity (Recall):  0.722 
# Standard Deviation of Sensitivity (Recall):  0.109 
# Mean Precision:  0.754 
# Standard Deviation of Precision:  0.034 
# Mean Specificity:  0.678 
# Standard Deviation of Specificity:  0.046 
# Mean F1 score:  0.733 
# Standard Deviation of F1 score:  0.046 
# =============================

# neural network
nr_folds <- 5
hidden_levels <- 5
gender_neuralnet_model1 <- create_neural_network_k_fold(DATA_MODEL3,"gender", numeric_cols, nr_folds, hidden_levels)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
# Accuracy:  78.34 %
# Sensitivity (Recall):  73.872 %
# Precision:  83.378 %
# Specificity:  83.378 %
# F1 score:  78.338 %
# Mean Accuracy:  74.01917 %
# Standard Deviation of Accuracy:  3.5561 
# Mean Sensitivity (Recall):  0.737 
# Standard Deviation of Sensitivity (Recall):  0.036 
# Mean Precision:  0.76 
# Standard Deviation of Precision:  0.048 
# Mean Specificity:  0.745 
# Standard Deviation of Specificity:  0.056 
# Mean F1 score:  0.747 
# Standard Deviation of F1 score:  0.032 
# =============================

# k-nearest-neighbours
nr_folds <- 5
gender_knearest_model1 <- create_knearest_model_k_fold(DATA_MODEL3,"gender", numeric_cols, nr_folds)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
#   Accuracy:  86.71 %
# Sensitivity (Recall):  0.875 
# Precision:  0.871 
# Specificity:  0.859 
# F1 score:  0.873 
# =============================
# =================================================================================================
# Simpler Model (model 2) - gender ~ Pro.level + altitude_results + hr_results
DATA_MODEL4 <- DATA1[,c("gender","Pro.level","altitude_results","hr_results")]
numeric_cols <- c("altitude_results","hr_results")

# decision tree
gender_tree_model2 <- create_decision_tree_k_fold(DATA_MODEL4,"gender", numeric_cols, 5)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
#   Accuracy:  81 %
# Sensitivity (Recall):  0.835 
# Precision:  0.807 
# Specificity:  0.756 
# F1 score:  0.821 
# Mean Accuracy:  0.7605836 %
# Standard Deviation of Accuracy:  0.0325 
# Mean Sensitivity (Recall):  0.787 
# Standard Deviation of Sensitivity (Recall):  0.122 
# Mean Precision:  0.771 
# Standard Deviation of Precision:  0.058 
# Mean Specificity:  0.672 
# Standard Deviation of Specificity:  0.096 
# Mean F1 score:  0.772 
# Standard Deviation of F1 score:  0.043 
# =============================

# neural network
nr_folds <- 5
hidden_levels <- 3
gender_neuralnet_model2 <- create_neural_network_k_fold(DATA_MODEL4,"gender", numeric_cols, nr_folds, hidden_levels)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
#   Accuracy:  84.53 %
# Sensitivity (Recall):  86.842 %
# Precision:  84.223 %
# Specificity:  81.963 %
# F1 score:  85.512 %
# Mean Accuracy:  83.30814 %
# Standard Deviation of Accuracy:  1.2289 
# Mean Sensitivity (Recall):  0.849 
# Standard Deviation of Sensitivity (Recall):  0.026 
# Mean Precision:  0.836 
# Standard Deviation of Precision:  0.039 
# Mean Specificity:  0.817 
# Standard Deviation of Specificity:  0.044 
# Mean F1 score:  0.841 
# Standard Deviation of F1 score:  0.012 
# =============================

# k-nearest-neighbours
nr_folds <- 5
gender_knearest_model2 <- create_knearest_model_k_fold(DATA_MODEL4,"gender", numeric_cols, nr_folds)
# =============================
# RESULTS 
# =============================
# Best Model Metrics:
#   Accuracy:  86.81 %
# Sensitivity (Recall):  0.886 
# Precision:  0.864 
# Specificity:  0.848 
# F1 score:  0.875 
# =============================
# =================================================================================================
# a)
library(rpart.plot)
# tree model comparisons
# model 1
rpart.plot(gender_tree_model1)
# model 2
rpart.plot(gender_tree_model2)

# neural net comparisons
# model 1
plot(gender_neuralnet_model1)
# model 2
plot(gender_neuralnet_model2)

# =================================================================================================
# b)
# Extract the accuracy values for the k-nearest neighbors and neural network models
accuracy_knn <- c(86.71, 86.81)   # Replace with the accuracy values
accuracy_nn <- c(78.34, 84.53)  # Replace with the accuracy values

# Perform the t-test
t_test <- t.test(accuracy_knn, accuracy_nn)

# Print the t-test results
print(t_test)
# =============================
# RESULTS 
# =============================
# Welch Two Sample t-test
# 
# data:  accuracy_knn and accuracy_nn
# t = 1.7203, df = 1.0005, p-value = 0.3351
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -33.95738  44.60738
# sample estimates:
#   mean of x mean of y 
# 86.760    81.435 
# =============================
# =================================================================================================