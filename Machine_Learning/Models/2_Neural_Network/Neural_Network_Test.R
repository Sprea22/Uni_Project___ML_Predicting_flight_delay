library(rstudioapi)
library(RColorBrewer)
library(ROCR)
library(plyr)
library(xlsx)
library(neuralnet)

# the following line is for getting the path of your current open file and the next lines set the working directory to the relevant one:
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# The following codes allow to load an already trained Neural Network model and get the performance measures about it.
# After running this codes, all the performance results will be contained in the "current_performance" data frame
# that can be easily saved as an excel table in the current directory folder just uncommenting the last row of this document.

##########################
#### INPUT PARAMETERS #### 
########################################################################################################

neurons_selected = 3
threshold_selected = 0.35
min_thr_selected = 0.01

# Load the already existing Neural Network model 
# The following model is the default pre-trained model
load("temp_FINAL_3N_Neural_Network_Model.rda")

# The following line is for getting the path of your current open file and the next lines set the working directory to the relevant one:
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
setwd("../../Datasets/")

# Import the datasets to test
dataset <- read.csv("TPA_Dataset.csv", stringsAsFactors=FALSE)
trainingset <- read.csv("TPA_Trainingset.csv", stringsAsFactors=FALSE)
testset <- read.csv("TPA_Testset.csv", stringsAsFactors=FALSE)

########################################################################################################

dataset$ARRIVAL_DELAY = as.logical(dataset$ARRIVAL_DELAY)
dataset$AIRLINE = as.factor(dataset$AIRLINE)
dataset$PRECIPITATION[which(dataset$PRECIPITATION == "T") ] = 0.00
dataset$PRECIPITATION = as.double(dataset$PRECIPITATION)

trainingset$ARRIVAL_DELAY = as.logical(trainingset$ARRIVAL_DELAY)
trainingset$AIRLINE = as.factor(trainingset$AIRLINE)
trainingset$PRECIPITATION[which(trainingset$PRECIPITATION == "T") ] = 0.00
trainingset$PRECIPITATION = as.double(trainingset$PRECIPITATION)

testset$ARRIVAL_DELAY = as.logical(testset$ARRIVAL_DELAY)
testset$AIRLINE = as.factor(testset$AIRLINE)
testset$PRECIPITATION[which(testset$PRECIPITATION == "T") ] = 0.00
testset$PRECIPITATION = as.double(testset$PRECIPITATION)

#Max-Min Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

attrSelect = c("SCHEDULED_DEPARTURE", "PRECIPITATION")

pre_processing = function(myData) {
  AIRLINE_DL <- myData$AIRLINE == 'DL'
  AIRLINE_WN <- myData$AIRLINE == 'WN'
  AIRLINE_NK <- myData$AIRLINE == 'NK'
  ARRIVAL_DELAY <- myData$ARRIVAL_DELAY
  
  myData = subset(myData, select= attrSelect)
  
  temp <- as.data.frame(lapply(myData, normalize))
  temp = cbind(temp, AIRLINE_DL)
  temp = cbind(temp, AIRLINE_WN)
  temp = cbind(temp, AIRLINE_NK)
  myData = cbind(temp, ARRIVAL_DELAY)
  
  rm(ARRIVAL_DELAY)
  rm(AIRLINE_DL)
  rm(AIRLINE_WN)
  rm(AIRLINE_NK)
  rm(temp)
  return(myData)
}

trainingset = pre_processing(trainingset)
testset = pre_processing(testset)
dataset = pre_processing(dataset)

##############################################
#### PRE-PROCESSING OF THE INPUT DATASETS ####
##############################################

predictions <- compute(neuralNetwork, testset[1:5])

tempRes = predictions
predictions = predictions$net.result
predictions[predictions >= threshold_selected] = 1
predictions[predictions < threshold_selected] = 0

results <- NULL
results$real = data.frame("real" = as.numeric(testset$ARRIVAL_DELAY))
results$prediction = data.frame("prediction" = predictions)
results = data.frame(results)

saveRDS(results, file="../Models/2_Neural_Network/NN_Results.rds") 

confusion.matrix = table(results$real, results$prediction)

Model_Acc = (sum(results$prediction == results$real))/(nrow(testset))

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

############################
# 10-Fold Cross Validation #
############################
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

#for (i in 1:10) {
 # nn <- neuralnet(ARRIVAL_DELAY ~ SCHEDULED_DEPARTURE + PRECIPITATION + AIRLINE_DL + AIRLINE_WN + AIRLINE_NK,
 #                 data = dataset[dataset$fold != i, c(1:6)], hidden = neurons_selected, stepmax = 1000000,
 #                 linear.output = FALSE, threshold = 0,005)
 # 
 # #Test the resulting output
 # predictions <- compute(nn, dataset[dataset$fold == i, c(1:5)])
 # 
 # tempRes = predictions
 # predictions = predictions$net.result
 # predictions[predictions >= threshold_selected] = 1
 # predictions[predictions < threshold_selected] = 0
  
 # results <- NULL
 # results$real = as.numeric(dataset[dataset$fold == i, c(1:6)]$ARRIVAL_DELAY)
 # results$prediction = data.frame("prediction" = predictions)
 # results = data.frame(results)
  
 # numcorrect = sum( results$prediction == results$real)
 # nb.accuracies = append(numcorrect / nrow(dataset[dataset$fold == i, c(1:5)]), nb.accuracies)
  
#}
Min_Acc = min(nb.accuracies)
Max_Acc = max(nb.accuracies)
Mean_Acc = mean(nb.accuracies)


#######################################
##### Peformance Measures Results #####
#######################################

current_performance = data.frame(c("Neurons number" = neurons_selected, "Threshold" = threshold_selected, "Min_thr_selected" = min_thr_selected,
                                   "Mean_Acc" = Mean_Acc,"Min_Acc" = Min_Acc, "Max_Acc" = Max_Acc, "Model_Acc" = Model_Acc, 
                                   "True_POS" = True_POS, "False_POS" = False_POS, "False_NEG" = False_NEG, "True_NEG" = True_NEG, 
                                   "Precision_0" = Precision_0, "Precision_1" = Precision_1, "Recall_0" = Recall_0, "Recall_1" = Recall_1, 
                                   "F_Measure_0" = F_Measure_0, "F_Measure_1" = F_Measure_1, "AUC" = perf@y.values))


temp = paste("Neural_Network_Evaluation",neurons_selected, threshold_selected, min_thr_selected, sep="_")
fileName = paste(temp, ".xlsx", sep="")
# Writing the final performance results into a excel file
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

#write.xlsx(current_performance, file=fileName, sheetName = "Neural_Network", col.names = TRUE, row.names = FALSE, append = FALSE)

