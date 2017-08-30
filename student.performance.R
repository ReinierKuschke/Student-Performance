## Reinier Kuschke
## 10/08/2017
## Student Performance in a Mathematics Course Prediction

## Student Performance Data Set from UC Irvine's Machine Learning Repository


# Get data
df <- read.csv('student-mat.csv',sep=';')

# load Libraries

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)
library(caTools)

# View data
head(df)

summary(df)

# Clean Data

## Check for NA values

any(is.na(df))

## Check for duplicates

anyDuplicated(df,incomparables = FALSE)

## Check Catagorical Variables are factors
str(df)

# Exploritory Data Analysis

## Correlation 

### Grab only numeric columns
num.cols <- sapply(df, is.numeric)

### Filter numeric columns for correlation
cor.data <- cor(df[,num.cols])

cor.data

### Plot correlated numerical Values

corrplot(cor.data,method='color')

corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal()


# Build model


## Set Seed Value

set.seed(123) 

##Split Train/Test

sample <- sample.split(df$age, SplitRatio = 0.70) 

## Training Data

train = subset(df, sample == TRUE)

## Testing Data

test = subset(df, sample == FALSE)

#Train Model

model <- lm(G3 ~ .,train)

summary(model)


# Visualize MOdel

### Grab residuals

res <- residuals(model)

### Convert to DataFrame for gglpot

res <- as.data.frame(res)

head(res)

### Histogram of residuals
ggplot(res,aes(res)) +  geom_histogram(fill='blue',bins=20,alpha=0.5)


plot(model)

# Test Moedel(Predict G3)

G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)

# Deal with negative predictions

to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$pred <- sapply(results$pred,to_zero)

# Model Evaluation

mse <- mean((results$real-results$pred)^2)

print(mse)

rmse <- mse^0.5

print(rmse)

SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

r2 = 1 - SSE/SST
print (r2)



























