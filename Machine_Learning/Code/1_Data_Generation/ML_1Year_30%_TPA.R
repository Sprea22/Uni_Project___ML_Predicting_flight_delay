
library("rstudioapi")

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
setwd("../../Datasets/")
# Import the final dataset about the ATL Airport
dataset <- read.csv("TPA_Dataset.csv", stringsAsFactors=FALSE)

year_dataset_ML = dataset
testset = 0

for(i in 1:12) {
  current_Month_perc = (nrow(year_dataset_ML[year_dataset_ML$MONTH == i,])/100)*30
  temp = testset
  testset <- rbind(year_dataset_ML[year_dataset_ML$MONTH == i,][sample(nrow(year_dataset_ML[year_dataset_ML$MONTH == i,]), current_Month_perc),], temp)
}

testset = testset[which(testset$ID != 0),]
trainingset = dataset[!(dataset$ID %in% testset$ID),]

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets")

#write.csv(testset, file = "TPA_Testset.csv", row.names = FALSE)
#write.csv(trainingset, file = "TPA_Trainingset.csv", row.names = FALSE)
