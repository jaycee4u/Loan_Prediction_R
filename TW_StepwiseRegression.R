
library(tidyverse)
library(caret)
library(leaps)
Data <- read.csv("F:/ThoughtWorks/trainingData.csv", header = TRUE)
SubData <- Data[, c("sex", "secondary_business", "annual_income", "monthly_expenses", "old_dependents", "young_dependents","type_of_house", "occupants_count", "house_area", "loan_tenure", "loan_amount" )]

#Remove outliers based on loan_amount
RemOutlr <- SubData[!SubData$loan_amount %in% boxplot.stats(SubData$loan_amount)$out, ]

##Select complete cases in RemOutlr#########
SampleData <- RemOutlr[complete.cases(RemOutlr), ]
library(MASS)
# Fit the full model 
full.model <- lm(loan_amount ~., data = SampleData)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both",trace = FALSE)
summary(step.model)


models <- regsubsets(loan_amount~., data = SampleData, nvmax = 4, method = "seqrep")
summary(models)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(loan_amount ~., data = SampleData,method = "leapBackward", tuneGrid = data.frame(nvmax = 1:5),trControl = train.control)
step.model$results