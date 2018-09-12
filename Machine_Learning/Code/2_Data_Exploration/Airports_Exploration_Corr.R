
library("rstudioapi")

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
setwd("../../Datasets/")
# Import the final dataset about the ATL Airport
ATL_Final_Dataset <- read.csv("ATL_Final_Dataset.csv", stringsAsFactors=FALSE)

# Create label for ML classification
ATL_Final_Dataset$ARRIVAL_DELAY[ATL_Final_Dataset$ARRIVAL_DELAY <= 0]<- 0
ATL_Final_Dataset$ARRIVAL_DELAY[ATL_Final_Dataset$ARRIVAL_DELAY > 0]<- 1

correlation_matrix = correlationFunction("TPA")

correlation_matrix

##### Correlation Function ########################################

correlationFunction = function(airportName) {
  test_dataset = ATL_Final_Dataset[which((ATL_Final_Dataset$ORIGIN_AIRPORT == airportName | ATL_Final_Dataset$ORIGIN_AIRPORT == "ATL") & (ATL_Final_Dataset$DESTINATION_AIRPORT == airportName | ATL_Final_Dataset$DESTINATION_AIRPORT == "ATL")),]
  str(test_dataset)
  test_dataset$ARRIVAL_DELAY = as.numeric(test_dataset$ARRIVAL_DELAY)
  test_dataset$PRECIPITATION[which(test_dataset$PRECIPITATION == "T") ] = 0.00
  test_dataset$PRECIPITATION = as.double(test_dataset$PRECIPITATION)
  
  test_dataset = subset(test_dataset, select=c("SCHEDULED_DEPARTURE", "SCHEDULED_ARRIVAL",
                                                             "SCHEDULED_TIME", "ARRIVAL_DELAY", "MAXTEMP", 
                                                             "MINTEMP", "PRECIPITATION"))
  correlation_matrix = cor(test_dataset)
  return(correlation_matrix) 
}

###################################################################
                 
