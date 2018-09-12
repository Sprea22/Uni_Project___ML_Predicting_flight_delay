library(rstudioapi)
library(rpart) 
library(rattle)
library(rpart.plot)

# the following line is for getting the path of your current open file and the next lines set the working directory to the relevant one:
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
setwd("../../Datasets/")

##########################
#### INPUT PARAMETERS #### 
########################################################################################################
cp_select = 0.016
threshold_select = 0.7
trainingset <- read.csv("TPA_Trainingset.csv", stringsAsFactors=FALSE)
attrSelect = c("SCHEDULED_DEPARTURE",  "AIRLINE", "PRECIPITATION", "ARRIVAL_DELAY")
########################################################################################################

plotcp(decisionTree) 


# NO Scheduled_Time, Distance, Arrival_Time
trainingset = subset(trainingset, select= attrSelect)

##########################################
#### TRAINING THE DECISION TREE MODEL #### 
##########################################

decisionTree = rpart(ARRIVAL_DELAY ~ ., data=trainingset, method="class", cp=cp_select )

# Visualizzo il grafico dell'albero decisionale originale
fancyRpartPlot(decisionTree)

# Save the trained model in the current directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# It's possible to modify the Decision Tree model's name changing the value in the attribute 'file'
save(decisionTree, file = "_Decision_Tree_Model.rda")
