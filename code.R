# Load libraries
library(dplyr); library(psych); library(readxl); library(car); library(corrr)

# Data preparation
df <- read.csv("data/smartphones.csv")
df <- na.omit(df)

# Converting variables to appropriate data types

df$Smartphone <- as.factor(df$Smartphone)
df$Brand <- as.factor(df$Brand)
df$Model <- as.factor(df$Model)
df$Color <- as.factor(df$Color)
df$Storage <- as.numeric(df$Storage)
df$Final.Price <- as.numeric(df$Final.Price)
df$RAM <- as.numeric(df$RAM)
df$Free <- as.factor(df$Free)

####EDA####
final_price_hist = hist(df$Final.Price, main = "Final Price Distribution") ## most price falls below 500
boxplot(df$Final.Price~df$Brand, main = "Final Price by brand")
# frequency of RAM
ram_prob_table = table(df$RAM)/ sum(table(df$RAM))
barplot(ram_prob_table,main = "Distribution of RAM size as % of total") ## 4s and 8s RAm are mos prominent
## frequency of Storage
storage_prob_table = table(df$Storage)/ sum(table(df$Storage))
barplot(storage_prob_table,main = "Distribution of Storage size as % of total") ## 128gbs are most famous

### CORR TESTING
## heatmap of continuous var
cor_mat(df %>%select(RAM, Storage, Final.Price), method = "pearson") 
# high corr between RAM and Storage  => interactions term?

# Chi-squares for corr between "Free" and "Brand"
contin_table_brand_free = table(df%>% select(Brand, Free))
chisq.test(contin_table_brand_free) ## Reject Ho, there is enough evidence to say that brand and free are dependent

# ANNOVA test between RAM and Brand
ram_brand_annova = aov(df$RAM~factor(df$Brand))
summary.aov(ram_brand_annova)

# ANNOVA test between RAM and Model 
ram_model_annova = aov(df$RAM~factor(df$Model))
summary.aov(ram_model_annova) ## Reject Ho, there is enough evidence to say that brand and model are dependent 

# Feature Engineering & Regression
RAM_Storage <- df$RAM * df$Storage
Brand_RAM_Interaction <- as.numeric(df$Brand) * df$RAM
Model_Interaction <- as.numeric(df$Model) * df$RAM

# Model Building
null_model <- lm(Final.Price ~ 1, data = df)
summary(null_model)

full_model <- lm(Final.Price ~ RAM + Storage + RAM_Storage + Brand_RAM_Interaction + Model_Interaction, data = df)
summary(full_model)

# Forward Selection
forward_model <- step(null_model, direction = "forward", scope = formula(full_model))
summary(forward_model)

# Backward Selection
backward_model <- step(full_model, direction = "backward", trace = 1)
summary(backward_model)

# Stepwise Selection
stepwise_model <- step(null_model, direction = "both", scope = formula(full_model), trace =1)
summary(stepwise_model)

# Plot the residuals 
plot(model$residuals)

# Plot diagnostic plot
par(mfrow = c(2, 2))
plot(model)

# Testing for multicollinearity
vif(model)

# Q-Q plot for normality of residuals
qqnorm(model$residuals)
qqline(model$residuals, col = "red")
