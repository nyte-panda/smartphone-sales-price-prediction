# Load Required Libraries
library(dplyr); library(psych); library(readxl); library(car); library(caret); library(rstatix)

# Data Preparation
df <- read.csv("Library/CloudStorage/OneDrive-YorkUniversity/4th Year/ADMS 4370/Final Project/smart-phone-sales-price-prediction/data/smartphones.csv")
df <- na.omit(df)


# Convert Variables to Appropriate Data Types 
df <- df %>% 
  mutate(
    Smartphone = as.factor(Smartphone),
    Brand = as.factor(Brand),
    Model = as.factor(Model),
    Color = as.factor(Color),
    Storage = as.numeric(Storage),
    Final.Price = as.numeric(Final.Price),
    RAM = as.numeric(RAM),
    Free = as.factor(Free)
  )

# Exploratory Data Analysis (EDA)
# Distribution of Final Price
hist(df$Final.Price, main = "Final Price Distribution", xlab = "Final Price", col = "lightblue") # Most prices fall below 500

# Boxplot : Final Price by Brand
boxplot(df$Final.Price ~ df$Brand, 
        main = "Final Price by Brand", 
        xlab = "Brand", 
        ylab = "Final Price",
        col = "grey") # Samsung has the highest median price

# Frequency of RAM
ram_prob_table = table(df$RAM)/ sum(table(df$RAM))
barplot(ram_prob_table, 
        main = "Distribution of RAM size as % of Total", 
        xlab = "RAM", 
        ylab = "Proportion",
        col = "green") # Most phones have 4GB and 8GB of RAM

# Frequency of Storage
storage_prob_table = table(df$Storage)/ sum(table(df$Storage))
barplot(storage_prob_table, 
        main = "Distribution of Storage Size as % of Total", 
        xlab = "Storage", 
        ylab = "Proportion",
        col = "lightyellow") # Most phones have 128GB of Storage

# Correlation Testing 
# Pearson Correlation Matrix
correlation_matrix <- cor_mat(df %>% select(RAM, Storage, Final.Price), method = "pearson")
print(correlation_matrix) # High correlation betwen RAM and Storage 

# Chi-Square Test: Dependency Between Free and Brand
contingency_table <- table(df$Brand, df$Free)
chi_square_results <- chisq.test(contingency_table)
print(chi_square_results) # Evidence that Brand and Free are dependent

# ANOVA Tests
# RAM vs Brand
ram_brand_anova <- aov(RAM ~ Brand, data = df)
summary(ram_brand_anova) 

# RAM vs. Model
ram_model_anova = aov(RAM ~ Model, data = df)
summary(ram_model_anova) # Evidence that RAM and Model are dependent

# Feature Engineering
df <- df %>%
  mutate(
    free_yes = if_else(Free == "Yes", 1, 0),
  )

# Model Building
# Null and Full Models
null_model <- lm(Final.Price ~ 1, data = df)
summary(null_model)

full_model <- lm(Final.Price ~ RAM + Storage + free_yes + RAM * Storage + free_yes * Storage + free_yes * RAM, data = df) 
summary(full_model)

# Forward Selection
forward_model <- step(null_model, direction = "forward", scope = formula(full_model, trace = 1))
summary(forward_model)

# Backward Selection
backward_model <- step(full_model, direction = "backward", trace = 1)
summary(backward_model)

# Stepwise Selection
stepwise_model <- step(null_model, direction = "both", scope = formula(full_model), trace =1)
summary(stepwise_model)

# Diagnostics
# Residuals Plot 
plot(stepwise_model$residuals, 
     main = "residuals of model",
     xlab = "Index",
     ylab = "Residuals",
     col = "purple",
     pch = 20)

# Diagnositic Plots 
par(mfrow = c(2, 2))
plot(stepwise_model)

# Testing for Multicollinearity
vif_results <- vif(stepwise_model, type ="predictor")
print(vif_results)

# Q-Q Plot for Residual Normality
qqnorm(stepwise_model$residuals, main = "Q-Q Plot of Residuals")
qqline(stepwise_model$residuals, col = "red")
