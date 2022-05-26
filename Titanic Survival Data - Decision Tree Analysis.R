# Decision Tree Analysis - Titanic Survival


# Recall some important libraries
library(htmlTable)
library(tidyverse)
library(ggplot2)
library(rvest)
library(naniar)
library(readr)
library(rpart)
library(rpart.plot)

train <- read.csv('C:/Users/Puneet Kishore/OneDrive/Desktop/Imarticus/R/Project 5 - Titanic Survival/train.csv')
test <- read.csv('C:/Users/Puneet Kishore/OneDrive/Desktop/Imarticus/R/Project 5 - Titanic Survival/test.csv')

# Combine Train and Test datasets to treat the Missing Values at once
dataset <- bind_rows(train, test)

# Omit Survived column from the Train dataset to nullify the NA
Survived <- train$Survived
train$Survived <- NULL

# Check the Dimensions of the Dataset
dim(dataset)

# Check the Structure of the Dataset
str(dataset)

# Check the Summary of the Dataset
summary(dataset)

# Check for the Missing Values
sum(is.na(dataset))
sapply(dataset, function(x)sum(is.na(x)))

# Missing Values Treatment
dataset$Fare[dataset$PassengerId==1044] <- median(dataset$Fare, na.rm = T)

# Replace Missing Values of Age column
dataset$Age <- sapply(dataset$Age, FUN = function(x){ifelse(is.na(x),median(dataset$Age, na.rm = T),x)})

# Alternate way to check the Missing Values in the Dataset
colSums(is.na(dataset))

# Remove the unimportant columns
dataset1 <- subset(dataset,select=-c(1,4,5,9,11,12))

# Build the Model
train_cleanned <- dataset1[1:891,]
test_cleanned <- dataset1[892:1309,]
train_cleanned$Survived <- Survived

# Decision Tree
DT <- rpart(Survived ~ Pclass + Age + SibSp + Parch + Fare, train_cleanned, method = "class")
summary(DT)

# Plotting the Decision Tree for the dataset
rpart.plot(DT, type=1, extra = 102)

# Output
predict_dt <- predict(DT, test_cleanned, type = "class")
result <- data.frame( Survived = predict_dt)
write.csv(result, file = "C:/Users/Puneet Kishore/OneDrive/Desktop/Imarticus/R/Project 5 - Titanic Survival/result.csv", row.names = F)



