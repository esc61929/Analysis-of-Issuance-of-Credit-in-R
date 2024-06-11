library(tidyverse)

#Read the dataset and view it 
df <- read_csv("dataset.csv")
view(df)

#define the quantitative variables within a dataset
df_quant <- df[, c("Num_children", "Num_family", "Account_length", "Total_income", "Age", "Years_employed")]

#Variable analysis for Num_children
mean(df_quant[["Num_children"]])
median(df_quant[["Num_children"]])
max(df_quant[["Num_children"]])
min(df_quant[["Num_children"]])
sd(df_quant[["Num_children"]])

#Variable analysis for Num_family
mean(df_quant[["Num_family"]])
median(df_quant[["Num_family"]])
max(df_quant[["Num_family"]])
min(df_quant[["Num_family"]])
sd(df_quant[["Num_family"]])

#Variable analysis for Account length
mean(df_quant[["Account_length"]])
median(df_quant[["Account_length"]])
max(df_quant[["Account_length"]])
min(df_quant[["Account_length"]])
sd(df_quant[["Account_length"]])

#Variable analysis for income
mean(df_quant[["Total_income"]])
median(df_quant[["Total_income"]])
max(df_quant[["Total_income"]])
min(df_quant[["Total_income"]])
sd(df_quant[["Total_income"]])

#Variable analysis for Age
mean(df_quant[["Age"]])
median(df_quant[["Age"]])
max(df_quant[["Age"]])
min(df_quant[["Age"]])
sd(df_quant[["Age"]])

#Variable analysis for number of years employed 
mean(df_quant[["Years_employed"]])
median(df_quant[["Years_employed"]])
max(df_quant[["Years_employed"]])
min(df_quant[["Years_employed"]])
sd(df_quant[["Years_employed"]])
