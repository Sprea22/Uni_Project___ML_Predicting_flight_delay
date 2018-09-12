
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

# Change this parameter in order to change the dataset that is going to be generated.
# Flights between ATL & airportName
airportName = "TPA"

Airport_Dataset = NULL
Airport_Dataset = ATL_Final_Dataset[which((ATL_Final_Dataset$ORIGIN_AIRPORT == airportName | ATL_Final_Dataset$ORIGIN_AIRPORT == "ATL") & (ATL_Final_Dataset$DESTINATION_AIRPORT == airportName | ATL_Final_Dataset$DESTINATION_AIRPORT == "ATL")),]

Airport_Dataset$ARRIVAL_DELAY = as.numeric(Airport_Dataset$ARRIVAL_DELAY)
Airport_Dataset$PRECIPITATION[which(Airport_Dataset$PRECIPITATION == "T") ] = 0.00
Airport_Dataset$PRECIPITATION = as.double(Airport_Dataset$PRECIPITATION)

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets")

fileName <- paste(airportName, "_Dataset.csv", sep="")

#### !!! To read !!! ######
# The write.csv it's commented because the dataset about TPA
# it's already saved as TPA_Dataset.csv in the dataset, ready to be used
############################

#write.csv(Airport_Dataset, file = fileName, row.names = FALSE)

