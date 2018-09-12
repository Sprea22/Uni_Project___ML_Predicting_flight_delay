
# Install.packages("plyr")
library(plyr)
# Install.packages("rstudioapi")
library(rstudioapi)
# The following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets/")

Flights_delay_ATL_Restored <- read.csv("Flights_delay_ATL_Restored.csv", stringsAsFactors=FALSE)
Weather_ATL_Restored <- read.csv("Weather_ATL_Restored.csv", stringsAsFactors=FALSE)

# Werge Flights_delay_ATL_Restored and Weather_ATL_Restored to create FINAL DATASET
ATL_Final_Dataset = merge(Flights_delay_ATL_Restored,Weather_ATL_Restored, by = c("MONTH","DAY"))

# When merge dataset coloumns "Precipitation" change changes from character to integer, then we bring the character type back to it
ATL_Final_Dataset$Precipitation = as.character(ATL_Final_Dataset$Precipitation)

# Rename the wrong columns name with the corrects
ATL_Final_Dataset = rename(ATL_Final_Dataset, c("MinTemp"="MINTEMP"))
ATL_Final_Dataset = rename(ATL_Final_Dataset, c("MaxTemp"="MAXTEMP"))
ATL_Final_Dataset = rename(ATL_Final_Dataset, c("Precipitation"="PRECIPITATION"))

# Add an ID column to the entire dataset
ATL_Final_Dataset$ID = 1:dim(ATL_Final_Dataset)
# Save final dataset 
write.csv(ATL_Final_Dataset, file = "ATL_Final_Dataset.csv", row.names =  FALSE)
