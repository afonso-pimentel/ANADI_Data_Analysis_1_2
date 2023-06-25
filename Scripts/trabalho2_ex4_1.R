# =================================================================================================
# 1. import the data
# Load the functions from utils.R
source("utils.R")
source("utils_k_fold.R")

DATA1 <- read.csv("./assets/ciclismo.csv")
# use setwd() to set the work directory on the console before running the readcsv command

summary(DATA1)
# ID           gender              Team            Background         Pro.level         Winter.Training.Camp altitude_results  vo2_results    
# Min.   :  0.0   Length:1000        Length:1000        Length:1000        Length:1000        Length:1000          Min.   : 24.00   Min.   : 21.00  
# 1st Qu.:249.8   Class :character   Class :character   Class :character   Class :character   Class :character     1st Qu.: 57.00   1st Qu.: 60.00  
# Median :499.5   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character     Median : 68.00   Median : 70.00  
# Mean   :499.5                                                                                                    Mean   : 66.75   Mean   : 69.75  
# 3rd Qu.:749.2                                                                                                    3rd Qu.: 77.00   3rd Qu.: 80.00  
# Max.   :999.0                                                                                                    Max.   :100.00   Max.   :100.00  
# hr_results         dob             Continent        
# Min.   : 17.00   Length:1000        Length:1000       
# 1st Qu.: 58.00   Class :character   Class :character  
# Median : 69.00   Mode  :character   Mode  :character  
# Mean   : 68.57                                        
# 3rd Qu.: 79.00                                        
# Max.   :100.00         

# Assuming you have a data frame called DATA1
# with character attributes: gender, pro.level, winter.camp.training, etc.

# Create a vector of attribute names
non_numeric_cols <- names(DATA1)[sapply(DATA1, function(x) !is.numeric(x))]
attributes <- non_numeric_cols

# Loop through each attribute
for (attr in attributes) {
  # Get unique values
  attr_values <- unique(DATA1[[attr]])
  
  # Print the unique values
  cat(paste("Unique values of", attr, ":\n"))
  cat(attr_values, sep = "\n")
  cat("\n\n")
}

# =================================================================================================
# 2. Get age from date of birth
current_date <- Sys.Date()

# Convert dob to Date type
DATA1$dob <- as.Date(DATA1$dob, format = "%Y-%m-%d")

# Calculate age and assign it to a new column
DATA1$age <- as.integer((current_date - DATA1$dob) / 365.25)

# =================================================================================================
# 3. Analyze the most significant attributes using plots, statistical analysis 
# and/or other appropriate methods

# Standard deviation for performance metrics
sd(DATA1$altitude_results) 
sd(DATA1$vo2_results) 
sd(DATA1$hr_results) 

# Correlation coefficients for performance metrics and age
cor(DATA1[, c("altitude_results", "vo2_results", "hr_results", "age")]) 


# Perform t-test to validate if there are any significante differences in altitude_results between 
# males and females
males_altitude <- DATA1$altitude_results[DATA1$gender == "male"]
females_altitude <- DATA1$altitude_results[DATA1$gender == "female"]
t_test_altitude <- t.test(males_altitude, females_altitude)
print(t_test_altitude)

# Perform t-test to validate if there are any significant differences in vo2_results between 
# males and females
males_vo2 <- DATA1$vo2_results[DATA1$gender == "male"]
females_vo2 <- DATA1$vo2_results[DATA1$gender == "female"]
t_test_vo2 <- t.test(males_vo2, females_vo2)
print(t_test_vo2)

# Perform t-test to validate if there are any significant differences in hr_results between 
# males and females
males_hr <- DATA1$hr_results[DATA1$gender == "male"]
females_hr <- DATA1$hr_results[DATA1$gender == "female"]
t_test_hr<- t.test(males_hr, females_hr)
print(t_test_hr)

# Perform t-test to validate if there any significant differences in hr_results/altitude_results
# and vo2_results for different pro levels
continental_altitude <- DATA1$altitude_results[DATA1$Pro.level == "Continental"]
world_tour_altitude <- DATA1$altitude_results[DATA1$Pro.level == "World Tour"]
t_test_altitude <- t.test(continental_altitude, world_tour_altitude)
print(t_test_altitude)

continental_vo2 <- DATA1$vo2_results[DATA1$Pro.level == "Continental"]
world_tour_vo2 <- DATA1$vo2_results[DATA1$Pro.level == "World Tour"]
t_test_vo2 <- t.test(continental_vo2, world_tour_vo2)
print(t_test_vo2)

continental_hr <- DATA1$hr_results[DATA1$Pro.level == "Continental"]
world_tour_hr <- DATA1$hr_results[DATA1$Pro.level == "World Tour"]
t_test_hr <- t.test(continental_hr, world_tour_hr)
print(t_test_hr)

# =================================================================================================
# 4.
# a) Check if there are any NAs in the dataset
has_nas <- any(is.na(DATA1))
# Display the result
if (has_nas) {
  print("The dataset contains NAs. We will remove them.")
  DATA1 <- DATA1[complete.cases(DATA1), ]
} else {
  print("The dataset does not contain NAs.")
}

# =================================================================================================
# b) Check if there are inconsistencies or outliers
# for numeric values we can make plots and check visually if there are any outliers
boxplot(DATA1$ID)

boxplot(DATA1$altitude_results)
# altitude_results has one outlier

boxplot(DATA1$vo2_results)
# vo2_results has 4 outliers

boxplot(DATA1$hr_results)
# hr_results has 2 outliers

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
# =================================================================================================
# c) implement feature selection - lets select only the numeric attributes for now
DATA1_1 <- DATA1[, c("ID", "altitude_results", "vo2_results", "hr_results", "age")]

# =================================================================================================
# d) normalize data
# we have many methods to normalize the data. But looking at the values we have
# probably the most indicated one is Decimal Scaling.
# altitude_results, vo2_results and hr_results are all integer values between 0 and 100
# Decimal scaling is done by diving the variable values by 10^k.
# we should consider k=2 if we want all of the values to be between 0 and 1.

# Apply Decimal Scaling normalization to altitude_results
k <- 2
DATA1_1$altitude_results <- DATA1_1$altitude_results / 10^k

# Apply Decimal Scaling normalization to vo2_results
DATA1_1$vo2_results <- DATA1_1$vo2_results / 10^k

# Apply Decimal Scaling normalization to hr_results
DATA1_1$hr_results <- DATA1_1$hr_results / 10^k

summary(DATA1_1)
# looking at the summary, the max value is 1 and no value is under 0.28.
# we are good to go

# =================================================================================================
# 5. Calculate the correlation matrix and comment
cor_matrix <- cor(DATA1_1[, c("altitude_results", "vo2_results", "hr_results", "age")])

# view the correlation matrix
View(cor_matrix)

library(corrplot)
corrplot(round(cor_matrix[,1:4], digits=3))

# Looking at the correlation matrix, we realize that age does not have a strong correlation to any of the other variables. 
# In fact, all of the correlation values between the age and the other variables are under 0.008. 
# 
# - altitude and vo2_results have 0.7974108400;
# - altitude and hr_results have 0.786882417;
# - vo2_results and hr_results have 0.946043210;
#
# Considering this, maybe vo2_results and hr_results are somewhat redundant.

# We should assess the relationship between both:

# Create a scatter plot
plot(DATA1_1$vo2_results, DATA1_1$hr_results, xlab = "vo2_results", ylab = "hr_results")

# The plot indicates a strong linearity between both variables.
# Perform PCA
pca <- prcomp(DATA1_1[, c("vo2_results", "hr_results")], scale. = TRUE)

# Variance explained by each principal component
print(pca$sdev^2 / sum(pca$sdev^2))

# With the variance explained by each principal component being 0.97302161 and 0.02697839, 
# it suggests that "vo2_results" has a much larger influence on the value changes 
# captured by the principal components compared to "hr_results."

# Nevertheless, these two will be needed for posterior exercises like 6 and 7.
# =================================================================================================
# 6. Simple linear regression model to determine Altitude_results while using hr_results
# a) linear function
# Set the seed for reproducibility
set.seed(123)

# Calculate the number of rows for the training dataset
num_train <- round(0.7 * nrow(DATA1_1))

# Randomly sample rows for the training dataset
train_data <- DATA1_1[sample(nrow(DATA1_1), num_train), ]

# Create the testing dataset by excluding the rows in the training dataset
test_data <- DATA1_1[-as.numeric(rownames(train_data)), ]

model <- lm(altitude_results ~ hr_results, data = train_data)

summary(model)

# the p value is really low, under 2.2e-16, which means we can reject H0.
# H0 states that there is no relationship between the values.
# =================================================================================================
# b) See the regression line and the dispersion diagram
plot(DATA1_1$hr_results, DATA1_1$altitude_results, 
     xlab = "hr_results", ylab = "altitude_results",
     main = "Dispersion Diagram")

# Add the regression line
abline(model, col = "red")

# The dispersion is pretty constant. Almost like there is a direct relation between both fields,
# there is a proportional change in the values. The assumption of homoscedasticity holds.
# =================================================================================================
# c) Calculate MAE and RMSE of the model for 30% teste cases

# Make predictions on the test dataset
predicted <- predict(model, newdata = test_data)

# Calculate MAE
MAE <- mean(abs(predicted - test_data$altitude_results))
print(paste("Mean Absolute Error (MAE):", MAE))
# Mean Absolute Error (MAE): 0.0709951730334678 

# Calculate RMSE
RMSE <- sqrt(mean((predicted - test_data$altitude_results)^2))
print(paste("Root Mean Squared Error (RMSE):", RMSE))
# Root Mean Squared Error (RMSE): 0.0868432680718388
# The error values seem rather low, but we should not forget that we have normalized the values
# to stay between 0 and 1. 
# =================================================================================================
# d) Test if there is a more complex model that has a better result
# we can do this with cross-validation:
# Load the necessary library
library(boot)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lets define a new dataframe with all of the other attributes, with the corrrect dummy variables
DATA1_2 <- DATA1[, c(1:9,11,12)]

formula_dummies <- as.formula("~ gender + Team + Background + Pro.level + Winter.Training.Camp")

library(caret)
DATA1_2_Dummy <- dummyVars(formula_dummies, data = DATA1_2)
DATA1_2_aux <- predict(DATA1_2_Dummy, newdata = DATA1_2)
DATA1_2_final <- DATA1_2_aux[,c(1,3:13,15,16)]
# Change column names
colnames(DATA1_2_final)[1] <- "Gender"
colnames(DATA1_2_final)[13] <- "Pro.level"
colnames(DATA1_2_final)[14] <- "Winter.Training.Camp"

# we can remove, for example Teamgroup E and Backgroundnone as dummy variables.
# if every other dummy variable assumes the value 0, it is the missing one
DATA1_2_final <- DATA1_2_final[,-c(6,10)]

# we finally have a good dataframe with the dummy variables defined following the K-1 rule
# we need to add the numeric values back
# Select the columns from DATA1_1 to add to DATA1_2_final
columns_to_add <- DATA1_1[, c("altitude_results", "vo2_results", "hr_results", "age")]

# Add the selected columns to DATA1_2_final
DATA1_2_final <- cbind(DATA1_2_final, columns_to_add)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define the linear regression model formula
formula1 <- altitude_results ~ hr_results
formula2 <- altitude_results ~ hr_results + vo2_results
formula3 <- altitude_results ~ .

# Perform cross-validation with the initial formula (this could be made with a custom function)
cv_results_1_1 <- cv.glm(data = DATA1_1, glmfit = glm(formula1, data = DATA1_1))
cv_results_1_2 <- cv.glm(data = DATA1_1, glmfit = glm(formula1, data = DATA1_1), K = 5)

# Perform cross-validation with the second formula
cv_results_2_1 <- cv.glm(data = DATA1_1, glmfit = glm(formula2, data = DATA1_1))
cv_results_2_2 <- cv.glm(data = DATA1_1, glmfit = glm(formula2, data = DATA1_1), K = 5)

# Perform cross-validation with the third formula for the full model with dummy variables
cv_results_3_1 <- cv.glm(data = DATA1_2_final, glmfit = glm(formula3, data = DATA1_2_final))
cv_results_3_2 <- cv.glm(data = DATA1_2_final, glmfit = glm(formula3, data = DATA1_2_final), K = 5)

print("Results for the simple model:")
print(cv_results_1_1$delta) # 0.007527248 0.007527234
print(cv_results_1_2$delta) # 0.007512965 0.007511297

print("Results for the more complex model:")
print(cv_results_2_1$delta) # 0.007013100 0.007013079
print(cv_results_2_2$delta) # 0.007030099 0.007023603

print("Results for the full model:")
print(cv_results_3_1$delta) # 0.002995135 0.002995086
print(cv_results_3_2$delta) # 0.003027161 0.003012883

# the conclusion is that the second model with vo2_results has a better delta result.
# and the one with the full model is by far better, with really low results
# lets try and redo the model with the hold out method for the second model
model_complex <- lm(altitude_results ~ hr_results + vo2_results, data = train_data)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the seed for reproducibility
set.seed(123)

# Calculate the number of rows for the training dataset
num_train_full <- round(0.7 * nrow(DATA1_2_final))

# Randomly sample rows for the training dataset
train_data_full <- DATA1_2_final[sample(nrow(DATA1_2_final), num_train_full), ]

# Create the testing dataset by excluding the rows in the training dataset
test_data_full <- DATA1_2_final[-as.numeric(rownames(train_data_full)), ]

full_model <- lm(altitude_results ~ ., data = train_data_full)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(model_complex)
summary(full_model)
# the full model shows the significance of the cyclists' background and the age is low
# probably there is a really low correlation between those dummy values and the rest of the data

# Make predictions on the test dataset
predicted_complex <- predict(model_complex, newdata = test_data)
predicted_full <- predict(full_model, newdata = test_data_full)

# Calculate MAE - complex model
MAE_complex <- mean(abs(predicted_complex - test_data$altitude_results))
print(paste("Mean Absolute Error (MAE) for the complex model:", MAE_complex))
#"Mean Absolute Error (MAE): 0.067636972630892"

# Calculate RMSE - complex model
RMSE_complex <- sqrt(mean((predicted_complex - test_data$altitude_results)^2))
print(paste("Root Mean Squared Error (RMSE) for the complex model:", RMSE_complex))
# "Root Mean Squared Error (RMSE): 0.0815823224946224"

# Calculate MAE - full model
MAE_full<- mean(abs(predicted_full - test_data_full$altitude_results))
print(paste("Mean Absolute Error (MAE) for the full model:", MAE_full))
#"Mean Absolute Error (MAE): 0.0433283473310209"

# Calculate RMSE - full model
RMSE_full <- sqrt(mean((predicted_full - test_data_full$altitude_results)^2))
print(paste("Root Mean Squared Error (RMSE) for the full model:", RMSE_full))
# "Root Mean Squared Error (RMSE): 0.0528638706167906"


# comparing the values with the simpler model, the "complex" model is more precise but the "full" value is clearly better
# the error drops to pratically half.
# =================================================================================================

# 7/8/9 ->  Predict the attribute “vo2_results”
DATA <- DATA1[, c(2:9,11,12)]
## a) Multiple linear regression

### Multiple Linear Regression with all possible variables
cat('Multiple regression model with maximum ammount of variables.\n')
model_multiple_regression <- create_multiple_regression_model_max(DATA,"vo2_results")
summary(model_multiple_regression)

### Multiple Linear Regression with only most relevant numeric variables 
cat('Multiple regression model with altitude_results hr_results.\n')
model_multiple_regression <- create_multiple_regression_model(DATA,"vo2_results", c('altitude_results', 'vo2_results', 'hr_results'))

## b) Regression Tree
model_regression_tree <- create_regression_tree_model(DATA,"vo2_results")

## c) Neural Network

## Tests with 5 hidden layers and 5 kfolds
number_of_hidden_layers <- 5
number_of_k_folds <- 5

# DATA TEST with only altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(7,8,9)]
cat('Neural network with altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Pro Level, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(5,7,8,9)]
cat('Neural network with Pro Level, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9)]
cat('Neural network with Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Continent, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 11)]
cat('Neural network with Continent, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Age, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 12)]
cat('Neural network with Age, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

## Tests with 10 hidden layers and 5 kfolds
number_of_hidden_layers <- 10
number_of_k_folds <- 5

# DATA TEST with only altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(7,8,9)]
cat('Neural network with altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Pro Level, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(5,7,8,9)]
cat('Neural network with Pro Level, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9)]
cat('Neural network with Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)
plot(model_neural_network)

# DATA TEST with only Continent, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 11)]
cat('Neural network with Continent, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Age, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 12)]
cat('Neural network with Age, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

## Tests with 10 hidden layers and 10 kfolds
number_of_hidden_layers <- 10
number_of_k_folds <- 10

# DATA TEST with only altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(7,8,9)]
cat('Neural network with altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Pro Level, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(5,7,8,9)]
cat('Neural network with Pro Level, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9)]
cat('Neural network with Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Continent, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 11)]
cat('Neural network with Continent, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Age, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 12)]
cat('Neural network with Age, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)


## Tests with 3 hidden layers and 3 kfolds
number_of_hidden_layers <- 3
number_of_k_folds <- 3

# DATA TEST with only altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(7,8,9)]
cat('Neural network with altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Pro Level, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(5,7,8,9)]
cat('Neural network with Pro Level, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9)]
cat('Neural network with Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)


# DATA TEST with only Continent, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 11)]
cat('Neural network with Continent, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Age, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 12)]
cat('Neural network with Age, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)


## Tests with 2 hidden layers and 2 kfolds
number_of_hidden_layers <- 2
number_of_k_folds <- 2

# DATA TEST with only altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(7,8,9)]
cat('Neural network with altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Pro Level, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(5,7,8,9)]
cat('Neural network with Pro Level, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9)]
cat('Neural network with Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)
plot(model_neural_network)

# DATA TEST with only Continent, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 11)]
cat('Neural network with Continent, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

# DATA TEST with only Age, Winter Training Camp, altitude_results, vo2_results and hr_results for prediction
DATA_TEST <- DATA1[, c(6,7,8,9, 12)]
cat('Neural network with Age, Winter Training Camp, altitude_results, vo2_results, hr_results, ', number_of_k_folds, 'folds and ',number_of_hidden_layers, 'hidden layers.\n')
model_neural_network <- create_neural_network_k_fold_NUMERIC_PREDICTION(DATA_TEST,"vo2_results", number_of_k_folds, number_of_hidden_layers)

## Perform significance test with a significance level of 5%
# Create vectors to store the results for each model
multilinear_regression_best_MAE_and_RMSE <- c(0.057, 0.073)
neural_network_regression_best_MAE_and_RMSE <- c(3.364, 4.218)

# Perform paired t-test
result <- t.test(multilinear_regression_best_MAE_and_RMSE, neural_network_regression_best_MAE_and_RMSE, alternative = "two.sided", conf.level = 0.95)

# Print the t-test results
print(result)

