library(ggplot2)
# Extract data out of CSV file.
dados2 <- read.csv("assets/dados2.csv", header = TRUE, sep = ",")
View(dados2)

# Parse data to only obtain numeric values.
subset_dados_numericos <- dados2[1:10, 3:8]
View(subset_dados_numericos)

# a)
# Apply Pearson correlation test to every possible pair of the algorithms in the data set
matrizDeCorrelacao <- cor(dados2[, -(1:2)], method = "pearson")
View(matrizDeCorrelacao)

# b) and c)
# Apply Shapiro-Wilk test to all the data in the CSV file to check if the data is normally distributed.
sapply(subset_dados_numericos, function(x) shapiro.test(x)$p.value)

# Apply Friedman test to data
data <- as.matrix(dados2[, 3:8])
View(data)
friedman.test(data, groups = dados2$dsets, blocks = NULL)
