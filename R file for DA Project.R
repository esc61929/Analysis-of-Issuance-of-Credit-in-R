library(tidyverse)
library(skimr)
library(fastDummies)
library(corrr)
library(caret)
library(MASS)

#Read the dataset and view it 
df <- read_csv("dataset.csv")
view(df)

#Summary of the variables in the data frame
str(df)

#Another function for finding summary of the data
skim(df)

df$Target <- factor(df$Target)

##Outlier Analysis with the Z-score method
#split the quantitative data from the categorical data
df_quant <- df[, c("Num_children", "Num_family", "Account_length", "Total_income", 
                   "Age", "Years_employed")]

#create the z-score function
z_scores <- function(x) {
  (x-mean(x))/sd(x)
}

#Apply the z-score function to the quantitative data
z_scores_quant <- as.data.frame(lapply(df_quant, z_scores))

#Identify rows that are more than three standard deviations away from mean
outliers_quant <- as.data.frame(lapply(z_scores_quant, function(x) abs(x)>3))

#Filter out the outliers 
outliers_combined <- apply(outliers_quant,1,any)

#Create the clean dataset with the outliers filtered out 
df_clean <- df[!outliers_combined, ]

##Label encode each categorical variable 
#Label encode income type 
df_clean$Income_type <- as.numeric(as.factor(df_clean$Income_type))

#Label encode education type
df_clean$Education_type <- as.numeric(as.factor(df_clean$Education_type))

#Label encode Family Status
df_clean$Family_status <- as.numeric(as.factor(df_clean$Family_status))

#Label encode Housing type
df_clean$Housing_type <- as.numeric(as.factor(df_clean$Housing_type))

#Label encode Occupation Type
df_clean$Occupation_type <- as.numeric(as.factor(df_clean$Occupation_type))

#create correlation matrix
correlation_matrix <- correlate(df_clean)

#create correlation matrix visual
rplot(correlation_matrix)

#set seed for reproducibility
set.seed(100)

#split the dataset into testing and training 
DataPartition <- createDataPartition(df_clean$Target, p = 0.7, list = FALSE, times = 1)
Target_train <- df_clean[DataPartition, ]
Target_test <- df_clean[-DataPartition, ]

head(Target_train)
head(Target_test)
#drop the ID feature
Target_train <- Target_train[, -1]
#Scale data with the 'caret' package
pre_process_values <- preProcess(Target_train[, c("Num_children", "Num_family", "Account_length",
                                          "Total_income", "Age", "Years_employed")], 
                                 method = c("center", "scale"))

Target_train <- predict(pre_process_values, Target_train)


##Creating p-values for each feature compared to the target feature 
#Chi-square Test for Gender and Target
contigency_table_gender <- table(Target_train$Gender, Target_train$Target)
chi_square_gender <- chisq.test(contigency_table_gender)
chi_square_gender

#Chi-square Test for Own_car and Target
contigency_table_car <- table(Target_train$Own_car, Target_train$Target)
chi_square_car <- chisq.test(contigency_table_car)
chi_square_car

#Chi-square Test for Own_Property and Target
contigency_table_property <- table(Target_train$Own_property, Target_train$Target)
chi_square_property <- chisq.test(contigency_table_property)
chi_square_property

#Chi-square Test for Work_Phone and Target
contigency_table_work_phone <- table(Target_train$Work_phone, Target_train$Target)
chi_square_work_phone <- chisq.test(contigency_table_work_phone)
chi_square_work_phone

#Chi-square Test for Phone and Target
contigency_table_phone <- table(Target_train$Phone, Target_train$Target)
chi_square_phone <- chisq.test(contigency_table_phone)
chi_square_phone

#Chi-square Test for Email and Target
contigency_table_email <- table(Target_train$Email, Target_train$Target)
chi_square_email <- chisq.test(contigency_table_email)
chi_square_email

#Chi-square Test for Unemployed and Target
contigency_table_unemployed <- table(Target_train$Unemployed, Target_train$Target)
chi_square_unemployed <- chisq.test(contigency_table_unemployed)
chi_square_unemployed

#Chi-square Test for Income type and Target
contigency_table_income <- table(Target_train$Income_type, Target_train$Target)
chi_square_income <- chisq.test(contigency_table_income)
chi_square_income

#Chi-square Test for Education type and Target
contigency_table_education <- table(Target_train$Education_type, Target_train$Target)
chi_square_education <- chisq.test(contigency_table_education)
chi_square_education

#Chi-square Test for Family Status and Target
contigency_table_family <- table(Target_train$Family_status, Target_train$Target)
chi_square_family <- chisq.test(contigency_table_family)
chi_square_family

#Chi-square Test for Housing type and Target
contigency_table_housing <- table(Target_train$Housing_type, Target_train$Target)
chi_square_housing <- chisq.test(contigency_table_housing)
chi_square_housing

#Chi-square Test for Occupation type and Target
contigency_table_occupation <- table(Target_train$Occupation_type, Target_train$Target)
chi_square_occupation <- chisq.test(contigency_table_occupation)
chi_square_occupation

#T-test for Number of Children and Target
t_test_children <- t.test(Num_children ~ Target, data=Target_train)
t_test_children

#T-test for Number of family and Target
t_test_family <- t.test(Num_family ~ Target, data=Target_train)
t_test_family

#T-test for Number of Account Length and Target
t_test_length <- t.test(Account_length ~ Target, data=Target_train)
t_test_length

#T-test for Total Income and Target
t_test_income <- t.test(Total_income ~ Target, data=Target_train)
t_test_income

#T-test for Age and Target
t_test_age <- t.test(Age ~ Target, data=Target_train)
t_test_age

#T-test for Years Employed and Target
t_test_employed <- t.test(Years_employed ~ Target, data=Target_train)
t_test_employed

##Stepwise regression for model selection 
Target_train$Target <- as.numeric(Target_train$Target)
Target_train$Target <- Target_train$Target -1
full.model <- glm(Target ~ ., data = Target_train)

stepwise.model <- stepAIC(full.model, direction = "both")

summary(stepwise.model)

##Random Forest for variable selection
#Set target as a factor variable
Target_train$Target <- as.factor(Target_train$Target)

#Create the random forest model
rf_model <- randomForest(Target ~., data = Target_train, importance=TRUE)

#Find importance scores for each feature
importance_scores <-importance(rf_model)
importance_scores

#Put the scores into a plot
varImpPlot(rf_model)

#Order the importance scores and select the top 6 in importance 
importance_scores <- importance_scores[order(-importance_scores[,1]),]
top_features <- rownames(importance_scores)[1:6]

#Create new data frame for random forest model
Target_train_randomforest <- Target_train[, c(top_features,"Target")]

##Apply SMOTE to the significance training data 
Target_train_significance <- Target_train[, c("Own_property", "Unemployed", "Family_status", "Account_length","Age", "Target")]
table(Target_train$Target)
smote_result <- SMOTE(X = Target_train_significance[,-6], target = Target_train_significance$Target, 
                      K=5, dup_size = 1.25)
Target_train_significance <- smote_result$data
Target_train_significance$Target <- factor(Target_train_significance$class)
Target_train_significance$class <- NULL

##Train a glm model based on variables from feature significance tests
model_significance <- train(Target ~ .,
                            data=Target_train_significance, 
                            method = "glm", 
                            family = "binomial")

predictions_significance <- predict(model_significance, newdata = Target_test)
conf_matrix_significance <- confusionMatrix(predictions_significance, Target_test$Target)
conf_matrix_significance

##Apply SMOTE to the Stepwise Regression Feature Selection Method 
Target_train_stepwise <- Target_train[, c("Own_property", "Work_phone", "Years_employed", "Account_length","Age", "Target")]
smote_result_stepwise <- SMOTE(X = Target_train_stepwise[,-6], target = Target_train_stepwise$Target, 
                      K=5, dup_size = 1.25)
Target_train_stepwise <- smote_result_stepwise$data
Target_train_stepwise$Target <- factor(Target_train_stepwise$class)
Target_train_stepwise$class <- NULL

##Train a glm model based on variables from feature Stepwise Regression tests
model_stepwise <- train(Target ~ .,
                        data=Target_train_stepwise, 
                        method = "glm", 
                        family = "binomial")

predictions_stepwise <- predict(model_stepwise, newdata = Target_test)
conf_matrix_stepwise <- confusionMatrix(predictions_stepwise, Target_test$Target)
conf_matrix_stepwise

##Apply SMOTE to the Random Selection Feature Selection Method 
smote_result_randomforest <- SMOTE(X = Target_train_randomforest[,-7], target = Target_train_randomforest$Target, 
                               K=5, dup_size = 2)
Target_train_randomforest<- smote_result_randomforest$data
Target_train_randomforest$Target <- factor(Target_train_randomforest$class)
Target_train_randomforest$class <- NULL

##Train a glm model based on variables from feature Random Forest tests
model_randomforest <- train(Target ~ .,
                      data=Target_train_randomforest, 
                      method = "glm", 
                      family = "binomial")

predictions_randomforest <- predict(model_randomforest, newdata = Target_test)
conf_matrix_randomforest <- confusionMatrix(predictions_randomforest, Target_test$Target)
conf_matrix_randomforest
