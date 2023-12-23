install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
library(ggplot2)
install.packages("vcd")
library(vcd)
install.packages("gmodels")
library(gmodels)
install.packages("class")
library(class)
install.packages("caret")
library(caret)
install.packages("kknn")
library(kknn)
install.packages("FNN")
library(FNN)
#install packages and libraries
MD <- read_csv("C:/Users/merce/Downloads/medical_clean.csv")
View(MD)
#upload csv file
str(MD)
summary(MD)
#Viewing data types and their examples
duplicates <- duplicated(MD)
#Checking data frame for duplicates
print(MD[duplicates, ])
#Print duplicate row 
missing_counts <- colSums(is.na(MD))
#Checking the missing values of each column
print(missing_counts)
#Show the sum of missing value
colnames(MD)[colnames(MD) == "Item1"] <- "Timely admission"
colnames(MD)[colnames(MD) == "Item2"] <- "Timely treatment"
colnames(MD)[colnames(MD) == "Item3"] <- "Timely visits"
colnames(MD)[colnames(MD) == "Item4"] <- "Reliability"
colnames(MD)[colnames(MD) == "Item5"] <- "Options"
colnames(MD)[colnames(MD) == "Item6"] <- "Hours of treatment"
colnames(MD)[colnames(MD) == "Item7"] <- "Courteous staff"
colnames(MD)[colnames(MD) == "Item8"] <- "Evidence of active listening from doctor"
#Change Item 1-8 names to relative descriptions
MD$ReAdmis <- as.numeric(MD$ReAdmis == "Yes")
MD$HighBlood <- as.numeric(MD$HighBlood == "Yes")
MD$Stroke <- as.numeric(MD$Stroke == "Yes")
MD$Overweight <- as.numeric(MD$Overweight == "Yes")
MD$Arthritis <- as.numeric(MD$Arthritis == "Yes")
MD$Diabetes <- as.numeric(MD$Diabetes == "Yes")
MD$Hyperlipidemia <- as.numeric(MD$Hyperlipidemia == "Yes")
MD$BackPain <- as.numeric(MD$BackPain == "Yes")
MD$Anxiety <- as.numeric(MD$Anxiety == "Yes")
MD$Allergic_rhinitis <- as.numeric(MD$Allergic_rhinitis == "Yes")
MD$Reflux_esophagitis <- as.numeric(MD$Reflux_esophagitis == "Yes")
MD$Asthma <- as.numeric(MD$Asthma == "Yes")
#convert categorical to numeric
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr
  outliers <- x[x < lower_fence | x > upper_fence]
  return(outliers)
}
#detect outliers
numeric_cols <- sapply(MD, is.numeric)
print(numeric_cols)
#numeric columns
columns_with_outliers <- sapply(MD[, numeric_cols], function(col) length(detect_outliers(col)) > 0)
outliers_columns <- names(columns_with_outliers[columns_with_outliers])
# Identify columns with outliers in the numeric columns
print(outliers_columns)
# Print columns with outliers
percentage_outliers <- length(columns_with_outliers) / nrow(MD) * 100
# Find the percentage of outliers
percentage_remaining <- 100 - percentage_outliers
#Calculate the percentage remaining
cat("Percentage of outliers:", percentage_outliers, "%\n")
cat("Percentage of data remaining:", percentage_remaining, "%\n")
#print results
MD$HighBlood <- factor(MD$HighBlood)
MD$Stroke <- factor(MD$Stroke)
MD$Overweight <- factor(MD$Overweight)
MD$Arthritis <- factor(MD$Arthritis)
MD$Diabetes <- factor(MD$Diabetes)
MD$Hyperlipidemia <- factor(MD$Hyperlipidemia)
MD$BackPain <- factor(MD$BackPain)
MD$Anxiety <- factor(MD$Anxiety)
MD$Allergic_rhinitis <- factor(MD$Allergic_rhinitis)
MD$Reflux_esophagitis <- factor(MD$Reflux_esophagitis)
MD$Asthma <- factor(MD$Asthma)
#convert values to factors
write.csv(MD, file = "C:/Users/merce/OneDrive/Desktop/md1_data4.csv", row.names = FALSE)
#CSV file
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Lat)) +
  labs(x = "", y = "Lat") +
  theme_bw() +
  ggtitle("Box Plot of Lat")
#Boxplot Lat
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Lng)) +
  labs(x = "", y = "Lng") +
  theme_bw() +
  ggtitle("Box Plot of Lng")
#Boxplot for LNG
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Population)) +
  labs(x = "", y = "Population") +
  theme_bw() +
  ggtitle("Box Plot of Population")
#Boxplot for Population
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Income)) +
  labs(x = "", y = "Income") +
  theme_bw() +
  ggtitle("Box Plot of Income")
#Boxplot for Income
boxplot_TotalCharge <- boxplot(MD$TotalCharge)
#boxplot for total charge 
boxplot_Additional_charges <- boxplot(MD$Additional_charges)
#Additional Charge boxplot
boxplot_Timely_admission <- boxplot(MD$`Timely admission`)
#boxplot for TA
boxplot_Timely_treatment <- boxplot(MD$`Timely treatment`)
#Boxplot TT
boxplot_Timely_visits <- boxplot(MD$`Timely visits`)
#Boxplot TV 
boxplot_Reliability <- boxplot(MD$Reliability)
#boxplot Reliabilty
boxplot_Options <- boxplot(MD$Options)
#Boxplot options 
boxplot_Hours_of_treatment <- boxplot(MD$`Hours of treatment`)
#boxplot Hours of Treatment





