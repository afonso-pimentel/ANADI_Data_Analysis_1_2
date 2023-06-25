library(FSA)

# Extract data out of CSV file.
dados3 <- read.csv("./assets/dados3.csv", header = TRUE, sep = ",")
View(dados3)

# Separate data into 3 different groups based on the number of cylinders that the car has.
group_of_4 <- subset(dados3, Cylinders == 4)
group_of_6 <- subset(dados3, Cylinders == 6)
group_of_8 <- subset(dados3, Cylinders == 8)

group_of_4_acc <- group_of_4$Acceleration
group_of_6_acc <- group_of_6$Acceleration
group_of_8_acc <- group_of_8$Acceleration

# Obtain a boxplot of the data
group_names <- c("4 Cylinders", "6 Cylinders", "8 Cylinders")
boxplot(group_of_4_acc, group_of_6_acc, group_of_8_acc, 
        names = group_names,
        main = "Acceleration by Vehicle Cylinder Group",
        xlab = "Vehicle Cylinder Group", ylab = "Acceleration")

# Apply Shapiro-Wilk test to the groups of data to check if the data is normally distributed.
shapiro.test(group_of_4_acc)
shapiro.test(group_of_6_acc)
shapiro.test(group_of_8_acc)

# Apply the Kruskal-Wallis test to the data
kruskal.test(list(group_of_4$Acceleration, group_of_6$Acceleration, group_of_8$Acceleration))

# Combine the groups into a single vector
data <- c(group_of_4_acc, group_of_6_acc, group_of_8_acc)

# Create a vector indicating the group for each observation
groups <- factor(rep(1:3, times = c(length(group_of_4_acc), length(group_of_6_acc), length(group_of_8_acc))))

# Perform the post hoc Dunn test
result <- dunnTest(data, groups, method = "bonferroni")
print(result)

#================================================================================================#
# b)
# Acceleration = y 
# Cylinders = x1 - this will be a dummy variable
# Weight = x2
# Horsepower = x3
# since we have multiple independent variables, it is called multiple regression.

# when creating the dummy variable, we need to take in consideration the number of categories.
# in this case, we will have 3 categories, so we need only 2 (k-1) indicating variables.
# We will have two new variables:
# Cylinders_6 - is 1 if it has 6 cylinders, 0 if it doesn't
# Cylinders_8 - is 1 if it has 8 cylinders, 0 if it doesn't
# We can "forget" about the 4 cylinders since the original variable is still there, 
# if it is none of those, then it has 4 cylinders.
# i)

dados3_1 <- dados3
dados3_1$Cylinders_6 <- ifelse(dados3_1$Cylinders == 6, 1, 0)
dados3_1$Cylinders_8 <- ifelse(dados3_1$Cylinders == 8, 1, 0)

# calculate MLR
model <- lm(Acceleration ~ Cylinders_6 + Cylinders_8 + Weight + Horsepower, data = dados3_1)

summary(model)
# Residuals:
#  Min      1Q  Median      3Q     Max 
# -4.5617 -1.2154 -0.2788  0.9860  7.1221 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 13.7490563  1.4169610   9.703 8.68e-16 ***
#  Cylinders_6 -1.5159977  0.7434813  -2.039 0.044283 *  
#  Cylinders_8 -4.8674507  1.2075112  -4.031 0.000114 ***
#  Weight       0.0031515  0.0006631   4.752 7.30e-06 ***
#  Horsepower  -0.0573811  0.0120787  -4.751 7.35e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Y = 13.7490563 - 1.5159977Cylinders_6 - 4.8674507Cylinders_8 + 0.0031515Weight - 0.0573811Horsepower

# Residual standard error: 2.12 on 93 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.6098,	Adjusted R-squared:  0.5931 
# F-statistic: 36.34 on 4 and 93 DF,  p-value: < 2.2e-16

# What we can take of the p-values of each variable is that they are extremely low except for Cylinders_6.
# Since that is of the lowest order, we can say that, with a significance of 95%, every coefficient βi is significantly diferent than zero.
# So the variables have a linear relation to the dependent variable Y.
# This model explains 60/61% of the variability of Y.

# let's check the assumptions of the regression model
par(mfrow=c(2,2))
plot(model)

# check the confidence interval
confint ( model , level =0.99)

# we can do predictions with a confidence interval
# ii)
prev1 <- data.frame(Cylinders_6=0, Cylinders_8=0,Weight=2950,Horsepower=100)
predict(model, prev1, interval="confidence", level=0.95)

#        fit      lwr      upr
# 1 17.30784 16.39779 18.21788
# we can predict the acceleration to be around 17.31 seconds. 
# It will be inside the interval of 16.398 and 18.218 seconds.    

# We can verify it by simply replacing values in the formula:
# Y = 13.7490563 - 1.5159977Cylinders_6 - 4.8674507Cylinders_8 + 0.0031515Weight - 0.0573811Horsepower
# Y = 13.7490563 - 0 - 0 + 0.0031515 * 2950 - 0.0573811 * 100
Y = 13.7490563 - 0 - 0 + 0.0031515 * 2950 - 0.0573811 * 100
# Y = 13.30784, it checks out.