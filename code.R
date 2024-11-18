library(dplyr)
library(psych)
library(readxl)

#read data
df <- read.csv("data/smartphones.csv")
summary(df)
str(df)

#clean data

colSums(is.na(df))
df <- na.omit(df)

# Converting variables to appropriate data types

df$Smartphone <- as.factor(df$Smartphone)
df$Brand <- as.factor(df$Brand)
df$Model <- as.factor(df$Model)
df$Color <- as.factor(df$Color)
df$Storage <- as.numeric(df$Storage)
df$`Final Price` <- as.numeric(df$`Final Price`)
df$RAM <- as.numeric(df$RAM)
df$Free <- as.factor(df$Free)

####EDA####
final_price_hist = hist(df$Final.Price, main = "Final Price Distribution") ## most price falls below 500
boxplot(df$Final.Price~df$Brand, main = "Final Price by brand")
#frequency of RAM
ram_prob_table = table(df$RAM)/ sum(table(df$RAM))
barplot(ram_prob_table,main = "Distribution of RAM size as % of total") ## 4s and 8s RAm are mos prominent
##frequency of Storage
storage_prob_table = table(df$Storage)/ sum(table(df$Storage))
barplot(storage_prob_table,main = "Distribution of Storage size as % of total") ## 128gbs are most famous

###CORR TESTING
##heatmap of continuous var
cor_mat(df %>%select(RAM, Storage, Final.Price), method = "pearson") 
#high corr between RAM and Storage  => interactions term?

#Chi-squares for corr between "Free" and "Brand"
contin_table_brand_free = table(df%>% select(Brand, Free))
chisq.test(contin_table_brand_free) ## Reject Ho, there is enough evidence to say that brand and free are dependent

#ANNOVA test between RAM and Brand
ram_brand_annova = aov(df$RAM~factor(df$Brand))
summary.aov(ram_brand_annova)

#ANNOVA test between RAM and Model 
ram_model_annova = aov(df$RAM~factor(df$Model))
summary.aov(ram_model_annova) ## Reject Ho, there is enough evidence to say that brand and model are dependent 
#Feature Engineering

#Interaction between RAM and Storage

RAM_Storage <- df$RAM * df$Storage

#Interaction between Brand and RAM/Storage

Brand_RAM_Interaction <- as.numeric(df$Brand) * df$RAM

#Model-Specific Interaction

Model_Interaction <- as.numeric(df$Model) * df$RAM


#Building Linear Regression Model

model <- lm(`Final Price` ~ RAM + Storage + RAM_Storage + Brand_RAM_Interaction + Model_Interaction, data = df)
summary(model)

#Stepwise model

stepwise_model <- step(model, direction = "both", trace = 1)
stepwise_model


#Plot the residuals 

plot(model$residuals)

#Plot diagnostic plot

par(mfrow = c(2, 2))
plot(model)
