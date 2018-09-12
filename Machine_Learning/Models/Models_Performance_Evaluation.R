library(rstudioapi)
library(RColorBrewer)
library(ROCR)
library(plyr)
library(xlsx)
library(neuralnet)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

results_model1 = readRDS(file="1_Decision_Tree/DT_Results.rds")
results_model2 = readRDS(file="2_Neural_Network/NN_Results.rds")

results_real_list = c()
results_pred_list = c()

results_real_model1 = results_model1$real
results_pred_model1 = results_model1$pred

results_real_model2 = results_model2$real
results_pred_model2 = results_model2$pred

results_real_list = list(results_real_model1, results_real_model2)
results_pred_list = list(results_pred_model1, results_pred_model2)

# Disply the ROC Graph
ROCRpred <- ROCR::prediction(results_pred_list, results_real_list)

# Calculating the performance considering: tpr = True Positive Rate, fpr = False Positive Rate
ROCRperf <- performance(ROCRpred, 'tpr','fpr')

# Calculating the auc = Area Under Curve, that's a value between 0 and 1. It allows to rank and compare different classifiers
perf <- performance(ROCRpred, measure = "auc")

plot(ROCRperf, xlab="False negative rate", ylab="True negative rate", col = as.list(c("green", "blue")), lwd = 2, main = "Test Set ROC Curves")
legend(x = "bottomright", col = as.list(c("green", "blue")),
       legend = c("Decision Tree" , "Neural Network"),
       fill = c("green","blue"))

abline(a=0, b=1)