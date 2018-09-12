library(rstudioapi)
library(rpart) 
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ROCR)
library(xlsx)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# The following codes allow to load an already trained Decision Tree model and get the performance measures about it.
# After running this codes, all the performance results will be contained in the "current_performance" data frame
# that can be easily saved as an excel table in the current directory folder just uncommenting the last row of this document.

##########################
#### INPUT PARAMETERS #### 
########################################################################################################
cp_select = 0.012
threshold_select = 0.3
attrSelect = c("SCHEDULED_DEPARTURE",  "AIRLINE", "PRECIPITATION", "ARRIVAL_DELAY")

# Load the already existing Decision Tree model 
# The following model is the default pre-trained model
load("FINAL_Decision_Tree_Model.rda")

# the following line is for getting the path of your current open file and the next lines set the working directory to the relevant one:
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
setwd("../../Datasets/")

# Import the datasets to test
dataset <- read.csv("TPA_Dataset.csv", stringsAsFactors=FALSE)
trainingset <- read.csv("TPA_Trainingset.csv", stringsAsFactors=FALSE)
testset <- read.csv("TPA_Testset.csv", stringsAsFactors=FALSE)

########################################################################################################

##############################################
#### PRE-PROCESSING OF THE INPUT DATASETS ####
##############################################
# NO Scheduled_Time, Distance, Arrival_Time
trainingset = subset(trainingset, select= attrSelect)
testset = subset(testset, select= attrSelect)
dataset = subset(dataset, select= attrSelect)

# Visualizzo il grafico dell'albero decisionale originale
#fancyRpartPlot(decisionTree, cex=1)

###############################################
#### PREDICTING VALUES USING CURRENT MODEL ####
###############################################
predictions = predict(decisionTree, testset, type="prob")
predictions = data.frame(predictions)
predictions$X0[predictions$X0 >= 1 - threshold_select] = 0 
predictions$X0[predictions$X0 < 1 - threshold_select & predictions$X0 != 0] = 1

results <- NULL
results$prediction = data.frame("prediction" = predictions$X0)
results$real = data.frame("real" = testset$ARRIVAL_DELAY)
results = data.frame(results)

saveRDS(results, file="../Models/1_Decision_Tree/DT_Results.rds") 

confusion.matrix = table(results$real, results$prediction)
plotcp(decisionTree) 

Model_Acc = (sum(results$prediction == results$real))/(nrow(testset))

# Visualizzo il grafico dell'albero decisionale originale
fancyRpartPlot(decisionTree, cex=1)

################################
#### Performance Evaluation ####
################################

# True positves: Not-Delayed flights correctly identified as not-delayed
True_POS = confusion.matrix[1,1]
# False negatives: Delayed flights incorrecly identified as not-delayed
False_NEG = confusion.matrix[1,2] 
# False positives: Not-delayed flights incorrecly identified as delayed
False_POS = confusion.matrix[2,1]
# True positves: Delayed flights correctly identified as delayed
True_NEG = confusion.matrix[2,2]

# Precision: How many selected items are relevant?  --> True positives / (True positives + False positives)
precision <- diag(confusion.matrix) / colSums(confusion.matrix)
Precision_0 = as.numeric(precision["0"])
Precision_1 = as.numeric(precision["1"])

# Recall: How many relevant items are selected? --> True positives / (True positives + False negatives)
recall <- (diag(confusion.matrix) / rowSums(confusion.matrix))
Recall_0 = as.numeric(recall["0"])
Recall_1 = as.numeric(recall["1"])

# F_Measure: It's a measure of a test's accuracy, and it considers both the precision and the recall.
F_Measure_0 <- 2 * Precision_0 * Recall_0 / (Precision_0 + Recall_0)
F_Measure_1 <- 2 * Precision_1 * Recall_1 / (Precision_1 + Recall_1)


# Disply the ROC Graph
ROCRpred <- ROCR::prediction(results$prediction, results$real)

# Calculating the performance considering: tpr = True Positive Rate, fpr = False Positive Rate
#ROCRperf <- performance(ROCRpred, 'tpr','fpr')
ROCRperf <- performance(ROCRpred, 'tnr','fnr')

# Calculating the auc = Area Under Curve, that's a value between 0 and 1. It allows to rank and compare different classifiers
perf <- performance(ROCRpred, measure = "auc")

# Plot the ROC graphic
plot(ROCRperf, lwd=2, colorize=T,main=paste("AUC:",(perf@y.values)))
abline(a=0, b= 1)

####################################
##### 10-Fold Cross Validation #####
####################################
dataset <- dataset[sample(nrow(dataset)),]

# Add a new column that assigns each row a number from 1 to 10, cutting the data up equally
dataset$fold = cut(1:nrow(dataset), breaks=10, labels=F)
# Here are the folds we got:
unique(dataset$fold)

# List that is going to contain the accuracy value for each testset of the 10-F CV
nb.accuracies = c()
# The length of this cycle is the same number of the Fold Cross Validation number
# dataset[dataset$fold != i, c(1:length(attrSelect))] ---> || trainset || --> 9/10 of the input_dataset, without the 16th column (the fold column)
# dataset[dataset$fold == i, c(1:length(attrSelect))] ---> || testset || --> 1/10 of the input_dataset, without the 16th column (the fold column)
for (i in 1:10) {
  decisionTree = rpart(ARRIVAL_DELAY ~ ., data=dataset[dataset$fold != i, c(1:length(attrSelect))], method="class", cp=cp_select ) 
  
  predictions = predict(decisionTree, dataset[dataset$fold == i, c(1:length(attrSelect))], type="prob")
  predictions = data.frame(predictions)
  predictions$X0[predictions$X0 >= threshold_select] = 0 
  predictions$X0[predictions$X0 < threshold_select & predictions$X0 != 0] = 1
  
  results <- NULL
  results$prediction = data.frame("prediction" = predictions$X0)
  results$real = data.frame("real" = dataset[dataset$fold == i, c(1:length(attrSelect))]$ARRIVAL_DELAY)
  results = data.frame(results)
  
  numcorrect = sum( results$prediction == results$real)
  
  nb.accuracies = append(numcorrect / nrow(dataset[dataset$fold == i, c(1:length(attrSelect)),]), nb.accuracies)
  
}

Min_Acc = min(nb.accuracies)
Max_Acc = max(nb.accuracies)
Mean_Acc = mean(nb.accuracies)

#######################################
##### Peformance Measures Results #####
#######################################

current_performance = data.frame(c("CP" = cp_select, "Threshold" = threshold_select,
                                   "Mean_Acc" = Mean_Acc,"Min_Acc" = Min_Acc, "Max_Acc" = Max_Acc, "Model_Acc" = Model_Acc, 
                                  "True_POS" = True_POS, "False_POS" = False_POS, "False_NEG" = False_NEG, "True_NEG" = True_NEG, 
                                   "Precision_0" = Precision_0, "Precision_1" = Precision_1, "Recall_0" = Recall_0, "Recall_1" = Recall_1, 
                                   "F_Measure_0" = F_Measure_0, "F_Measure_1" = F_Measure_1, "AUC" = perf@y.values))

temp = paste("Decision_Tree_Evaluation",cp_select, threshold_select, sep="_")
fileName = paste(temp, ".xlsx", sep="")
# Writing the final performance results into a excel file
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#write.xlsx(current_performance, file=fileName, sheetName = "Decision_Tree", col.names = TRUE, row.names = FALSE, append = FALSE)

