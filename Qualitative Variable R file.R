library(tidyverse)

#Read the dataset and view it 
df <- read_csv("dataset.csv")
view(df)

#Define the categorical variables within a dataset
df_qual <- df[, c("Gender", "Own_car", "Own_property", "Work_phone", "Phone", "Email", "Unemployed", "Income_type", "Education_type", "Family_status", "Housing_type", "Occupation_type", "Target")]

#Variable analysis for Gender 
table(df_qual$Gender)
prop.table(table(df_qual$Gender))
ggplot(df_qual, aes(x=Gender)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Gender")

#Variable Analysis for Own_car
table(df_qual$Own_car)
prop.table(table(df_qual$Own_car))
ggplot(df_qual, aes(x=Own_car)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Car Ownership")

#Variable Analysis for Own_property 
table(df_qual$Own_property)
prop.table(table(df_qual$Own_property))
ggplot(df_qual, aes(x=Own_property)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Property Ownership")

#Variable Analysis for Work_phone 
table(df_qual$Work_phone)
prop.table(table(df_qual$Work_phone))
ggplot(df_qual, aes(x=Work_phone)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Work Phone")

#Variable Analysis for Phone 
table(df_qual$Phone)
prop.table(table(df_qual$Phone))
ggplot(df_qual, aes(x=Phone)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Phone Ownership")

#Variable Analysis for Email
table(df_qual$Email)
prop.table(table(df_qual$Email))
ggplot(df_qual, aes(x=Email)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Email")

#Variable Analysis for Unemployed 
table(df_qual$Unemployed)
prop.table(table(df_qual$Unemployed))
ggplot(df_qual, aes(x=Unemployed)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Unemployment")

#Variable Analysis for Income_type
table(df_qual$Income_type)
prop.table(table(df_qual$Income_type))
ggplot(df_qual, aes(x=Income_type)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Income Type")

#Variable Analysis for Education_type
table(df_qual$Education_type)
prop.table(table(df_qual$Education_type))
ggplot(df_qual, aes(x=Education_type)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Type of Education")

#Variable Analysis for Family_status
table(df_qual$Family_status)
prop.table(table(df_qual$Family_status))
ggplot(df_qual, aes(x=Family_status)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Family Status")

#Variable Analysis for Housing_type
table(df_qual$Housing_type)
prop.table(table(df_qual$Housing_type))
ggplot(df_qual, aes(x=Housing_type)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Housing Type")

#Variable Analysis for Occupation_type
table(df_qual$Occupation_type)
prop.table(table(df_qual$Occupation_type))
ggplot(df_qual, aes(x=Occupation_type)) + 
  geom_bar() + 
  ggtitle("Bar Plot of Occupation Type")

#Variable Analysis for the Target variable 
table(df_qual$Target)
prop.table(table(df_qual$Target))
ggplot(df_qual, aes(x=Target)) + 
  geom_bar() + 
  ggtitle("Bar Plot of the Target Variable")
