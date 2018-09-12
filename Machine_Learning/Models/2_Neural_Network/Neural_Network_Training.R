library(rstudioapi)
library(neuralnet)

# the following line is for getting the path of your current open file and the next lines set the working directory to the relevant one:
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
setwd("../../Datasets/")

##########################
#### INPUT PARAMETERS #### 
########################################################################################################

neurons_selected = 3
threshold_selected = 0.35
min_thr_selected = 0.1

# Import the datasets to use for train the model
trainingset <- read.csv("TPA_Trainingset.csv", stringsAsFactors=FALSE)

########################################################################################################

##############################################
#### PRE-PROCESSING OF THE INPUT DATASETS ####
##############################################

trainingset$ARRIVAL_DELAY = as.logical(trainingset$ARRIVAL_DELAY)
trainingset$AIRLINE = as.factor(trainingset$AIRLINE)
trainingset$PRECIPITATION[which(trainingset$PRECIPITATION == "T") ] = 0.00
trainingset$PRECIPITATION = as.double(trainingset$PRECIPITATION)

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

temp_Trainingset = pre_processing(trainingset)

neuralNetwork <- neuralnet(ARRIVAL_DELAY ~ SCHEDULED_DEPARTURE + PRECIPITATION + AIRLINE_DL + AIRLINE_WN + AIRLINE_NK,
                data = temp_Trainingset, threshold = min_thr_selected, lifesign = "full", stepmax = 1000000,
                hidden = neurons_selected, linear.output = FALSE)

rm(temp_Trainingset,trainingset)

# Save the trained model in the current directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# It's possible to modify the Neural Network model's name changing the value in the attribute 'file'
save(neuralNetwork, file = "_3N_Neural_Network_Model.rda")

