
#install.packages("rstudioapi")
library(rstudioapi)
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets/")

#################################################################
# DATASET 1: Select the origin dataset of the weather about ATL #
#################################################################

Weather_ATL <- read.csv("Weather_ATL.csv", stringsAsFactors=FALSE)

########################################################################################
# DATASET 2: Generate the dataset which is going to be used for the data quality phase #
########################################################################################

Weather_ATL_Data_Quality <- subset(Weather_ATL, select=c(Date, MaxTemp, MinTemp, Precipitation))

# Save the filtered dataset of ATL Weather as Weather_ATL_Data_Quality.csv 
write.csv(Weather_ATL_Data_Quality, file = "Weather_ATL_Data_Quality.csv ", row.names = FALSE) 

####################################################
################  DATA CONSTRUCTION ################
####################################################

#############################################################################################
# DATASET 3: Generate the final version of the weather dataset changing justthe date format #
#############################################################################################

# Change the date format in order to allow the integration process with the Flights dataset
toNumbers <- function(Date) {
  stopifnot(inherits(Date, c("Date", "POSIXt")))
  day <- as.numeric(strftime(Date, format = "%d")) 
  month <- as.numeric(strftime(Date, format = "%m"))
  year <- as.numeric(strftime(Date, format = "%Y")) 
  list(year = year, month = month, day = day) }

Weather_ATL_Data_Quality$Date = as.Date(Weather_ATL_Data_Quality$Date, format = "%m/%d/%y")
tempDate = toNumbers(Weather_ATL_Data_Quality$Date)
Weather_ATL_Data_Quality$DAY <-  tempDate$day
Weather_ATL_Data_Quality$MONTH <-  tempDate$month
Weather_ATL_Data_Quality$Date <- NULL

# Save the final dataset of ATL Weather as Weather_ATL_Restored.csv 
write.csv(Weather_ATL_Data_Quality, file = "Weather_ATL_Restored.csv", row.names = FALSE) 
