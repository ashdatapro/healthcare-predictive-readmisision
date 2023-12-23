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
install.packages("pROC")
library(pROC)
#install packages and libraries
MD <- read_csv("C:/Users/merce/OneDrive/Desktop/md1_data4.csv")
View(MD)
#csv file
df <- data.frame(MD[, c("HighBlood", "Stroke", "Overweight", "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Income", "TotalCharge", "Additional_charges", "ReAdmis")])
View(df)
#dataframe
set.seed(123)
#set seed for reproducibility
train_prop <- 0.7
#set proportion for training data
train_indices <- createDataPartition(df$ReAdmis, p = train_prop, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]
# Split the data into training and test sets
write.csv(train_data, file = "training_data.csv", row.names = FALSE)
write.csv(test_data, file = "test_data.csv", row.names = FALSE)
#csv file
train_data <- read.csv("training_data.csv")
test_data <- read.csv("test_data.csv")
#load test
train_x <- train_data[, c("HighBlood", "Stroke", "Overweight", "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Income", "TotalCharge", "Additional_charges")]
test_x <- test_data[, c("HighBlood", "Stroke", "Overweight", "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Income", "TotalCharge", "Additional_charges")]
#extrac variables
train_y <- train_data$ReAdmis
test_y <- test_data$ReAdmis
#create train and test sets
k <- 5
#set value for k
knn_pred <- knn(train_x, test_x, train_y, k)
View(knn_pred)
#perform classification
accuracy <- sum(knn_pred == test_y) / length(test_y)
#accuracy
k_values <- 1:10
#kvalue
accuracy <- sapply(k_values, function(k) {
  knn_pred <- knn(train_x, test_x, train_y, k)
  sum(knn_pred == test_y) / length(test_y)
})
# Perform KNN classification 
k_accuracy <- data.frame(k = k_values, accuracy = accuracy)
View(k_accuracy)
# Create a data frame for plotting
true_labels <- as.numeric(test_y)
predicted_labels <- as.numeric(knn_pred)
#listed PTL and TL
tl <- as.factor(test_y)  
pl <- as.factor(knn_pred)
#level TL and PL
confusion_matrix <- confusionMatrix(pl, tl)
print(confusion_matrix)
#create confusion matrix
precision <- confusion_matrix$byClass["Precision"]
print(paste("Precision:", precision))
#calculate precision
recall <- confusion_matrix$byClass["Sensitivity"]
print(paste("Recall:", recall))
#calculate recall
f1_score <- confusion_matrix$byClass["F1"]
print(paste("F1 Score:", f1_score))
#calculate f1
auc_value <- roc(true_labels, predicted_labels)$auc
print(auc_value)
#calculate AUC
roc_curve <- roc(true_labels, predicted_labels)
print(roc_curve)
# Plot the ROC curve
roc_curve <- roc(true_labels, predicted_labels)
# Plot the graph
plot(roc_curve, main = "ROC Curve", print.thres = "best")
# Plot the ROC curve
text(0.5, 0.5, paste("AUC =", round(auc(roc_curve), 4)), adj = 0.5)
#AUC
title(xlab = "False Positive Rate")
title(ylab = "True Positive Rate")
legend("bottomright", legend = c("Model"), col = c("black"), lty = 1)
# Add labels and legend