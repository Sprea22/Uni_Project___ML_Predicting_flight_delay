
#install.packages("rstudioapi")
library(rstudioapi)
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets/")

#install.packages("chron")
library("chron")
#install.packages("lubridate")
library(lubridate)

##############
# Uniqueness #
##############

ATL_Final_Dataset <- read.csv("ATL_Final_Dataset.csv", stringsAsFactors=FALSE)
Number_of_rows_dataset = nrow(ATL_Final_Dataset)
Number_of_cols_dataset = ncol(ATL_Final_Dataset)

Number_of_duplicated_tuple = Number_of_rows_dataset - nrow(unique(ATL_Final_Dataset))
sprintf("Number of duplicated tuple: %s", Number_of_duplicated_tuple)


######################
# Syntactic Accuracy #
######################

airports <- read.csv("Airports.csv")
airlines <- read.csv("Airlines.csv")

iata_airline = airlines$IATA_CODE
iata_airport = airports$IATA_CODE

rm(airports)
rm(airlines)

Number_of_correct_airline = length(ATL_Final_Dataset$AIRLINE[ATL_Final_Dataset$AIRLINE %in% iata_airline])
Number_of_correct_destination = length(ATL_Final_Dataset$DESTINATION_AIRPORT[ATL_Final_Dataset$DESTINATION_AIRPORT %in% iata_airport])
Number_of_correct_origin = length(ATL_Final_Dataset$ORIGIN_AIRPORT[ATL_Final_Dataset$ORIGIN_AIRPORT %in% iata_airport])

Percentage_of_correct_airline = Number_of_correct_airline /Number_of_rows_dataset
Percentage_of_correct_destination = Number_of_correct_destination /Number_of_rows_dataset
Percentage_of_correct_origin= Number_of_correct_origin /Number_of_rows_dataset

Percentage_of_total_correctness = (Percentage_of_correct_airline+Percentage_of_correct_destination+Percentage_of_correct_origin)/3
sprintf("Percentage of correct airline: %s", Percentage_of_correct_airline)
sprintf("Percentage of correct destination: %s", Percentage_of_correct_destination)
sprintf("Percentage of correct origin: %s", Percentage_of_correct_origin)

sprintf("Total percentage of data correctness: %s", Percentage_of_total_correctness)

######################
# Table Completeness #
######################

null = sum(is.na(ATL_Final_Dataset))
not_null = sum(!is.na(ATL_Final_Dataset))
Number_of_values_dataset = Number_of_rows_dataset*Number_of_cols_dataset
Comleteness_degree = not_null/Number_of_values_dataset

sprintf("Percentage of NOT NULL values: %f", Comleteness_degree)

######################################
# Referencial Integrity Consistency  #
######################################
percentage_correct_minTemp = length(ATL_Final_Dataset$ID[typeof(ATL_Final_Dataset$MINTEMP) == "integer" & ATL_Final_Dataset$MINTEMP >= -100])/Number_of_rows_dataset
percentage_correct_month = length(ATL_Final_Dataset$MONTH[typeof(ATL_Final_Dataset$MONTH) == "integer" & ATL_Final_Dataset$MONTH < 13 & ATL_Final_Dataset$MONTH > 0])/Number_of_rows_dataset
percentage_correct_day = length(ATL_Final_Dataset$DAY[typeof(ATL_Final_Dataset$DAY) == "integer" & ATL_Final_Dataset$DAY < 32 & ATL_Final_Dataset$DAY > 0])/Number_of_rows_dataset
percentage_correct_day_of_week = length(ATL_Final_Dataset$DAY_OF_WEEK[typeof(ATL_Final_Dataset$DAY_OF_WEEK) == "integer" & ATL_Final_Dataset$DAY_OF_WEEK > 0 & ATL_Final_Dataset$DAY_OF_WEEK < 8])/Number_of_rows_dataset
percentage_correct_airline = length(ATL_Final_Dataset$AIRLINE[typeof(ATL_Final_Dataset$AIRLINE) == "character" & nchar(ATL_Final_Dataset$AIRLINE) == 2])/Number_of_rows_dataset
percentage_correct_origin = length(ATL_Final_Dataset$ORIGIN_AIRPORT[typeof(ATL_Final_Dataset$ORIGIN_AIRPORT) == "character" & nchar(ATL_Final_Dataset$ORIGIN_AIRPORT) == 3])/Number_of_rows_dataset
percentage_correct_destination = length(ATL_Final_Dataset$DESTINATION_AIRPORT[typeof(ATL_Final_Dataset$DESTINATION_AIRPORT) == "character" & nchar(ATL_Final_Dataset$DESTINATION_AIRPORT) == 3])/Number_of_rows_dataset
percentage_correct_departure = length(ATL_Final_Dataset$SCHEDULED_DEPARTURE[typeof(ATL_Final_Dataset$SCHEDULED_DEPARTURE) == "integer" & ATL_Final_Dataset$SCHEDULED_DEPARTURE < 2400 & ATL_Final_Dataset$SCHEDULED_DEPARTURE >= 0])/Number_of_rows_dataset 
percentage_correct_time = length(ATL_Final_Dataset$SCHEDULED_TIME[typeof(ATL_Final_Dataset$SCHEDULED_TIME) == "integer"])/Number_of_rows_dataset
percentage_correct_distance = length(ATL_Final_Dataset$DISTANCE[typeof(ATL_Final_Dataset$DISTANCE) == "integer"])/Number_of_rows_dataset
percentage_correct_arrival = length(ATL_Final_Dataset$SCHEDULED_ARRIVAL[typeof(ATL_Final_Dataset$SCHEDULED_ARRIVAL) == "integer"  & ATL_Final_Dataset$SCHEDULED_ARRIVAL < 2400 & ATL_Final_Dataset$SCHEDULED_ARRIVAL >= 0]) /Number_of_rows_dataset
percentage_correct_delay = length(ATL_Final_Dataset$ARRIVAL_DELAY[typeof(ATL_Final_Dataset$ARRIVAL_DELAY) == "integer"])/Number_of_rows_dataset
percentage_correct_minTemp = length(ATL_Final_Dataset$MINTEMP[typeof(ATL_Final_Dataset$MINTEMP) == "integer" & ATL_Final_Dataset$MINTEMP >= -100])/Number_of_rows_dataset
percentage_correct_maxTemp = length(ATL_Final_Dataset$MAXTEMP[typeof(ATL_Final_Dataset$MAXTEMP) == "integer" & ATL_Final_Dataset$MAXTEMP < 100])/Number_of_rows_dataset
percentage_correct_precipitation = length(ATL_Final_Dataset$PRECIPITATION[typeof(ATL_Final_Dataset$PRECIPITATION) == "character" & as.numeric(ATL_Final_Dataset$PRECIPITATION[ATL_Final_Dataset$PRECIPITATION != "T"])>=0])/Number_of_rows_dataset

average_ref_int_consistency_percentage = (percentage_correct_month+percentage_correct_day+percentage_correct_day_of_week+percentage_correct_airline+
  percentage_correct_origin+percentage_correct_destination+percentage_correct_departure+percentage_correct_time+
  percentage_correct_distance+percentage_correct_arrival+percentage_correct_delay+percentage_correct_minTemp
  +percentage_correct_maxTemp+percentage_correct_precipitation)/14

sprintf("Average referencial integrity consistency percentage of the data: %f", average_ref_int_consistency_percentage)

#####################
# Data Consistency  #
#####################

# The following code is going to check that the column ARRIVAL_TIME, SCHEDULED_ARRIVAL and ARRIVAL_DELAY are consistents.

ATL_Final_Dataset$ARRIVAL_TIME = substr(as.POSIXct(sprintf("%04.0f", ATL_Final_Dataset$ARRIVAL_TIME), format='%H%M'), 12, 16)
ATL_Final_Dataset$SCHEDULED_ARRIVAL = substr(as.POSIXct(sprintf("%04.0f", ATL_Final_Dataset$SCHEDULED_ARRIVAL), format='%H%M'), 12, 16)

ATL_Final_Dataset$ARRIVAL_TIME = do.call(paste, c(ATL_Final_Dataset[c("ARRIVAL_TIME")], "00", sep = ":"))
ATL_Final_Dataset$SCHEDULED_ARRIVAL = do.call(paste, c(ATL_Final_Dataset[c("SCHEDULED_ARRIVAL")], "00", sep = ":"))

ATL_Final_Dataset$ARRIVAL_TIME = format(as.POSIXct(ATL_Final_Dataset$ARRIVAL_TIME, format="%H:%M:%S"), "%H:%M:%S")
ATL_Final_Dataset$SCHEDULED_ARRIVAL = format(as.POSIXct(ATL_Final_Dataset$SCHEDULED_ARRIVAL, format="%H:%M:%S"), "%H:%M:%S")

ATL_Final_Dataset$ARRIVAL_TIME[ATL_Final_Dataset$ARRIVAL_TIME == "00:00:00"] = format(as.POSIXct("24:00:00", format="%H:%M:%S"), "%H:%M:%S")
ATL_Final_Dataset$SCHEDULED_ARRIVAL[ATL_Final_Dataset$SCHEDULED_ARRIVAL == "00:00:00"] = format(as.POSIXct("24:00:00", format="%H:%M:%S"), "%H:%M:%S")


ATL_Final_Dataset$TIME_DIFF = ifelse(ATL_Final_Dataset$SCHEDULED_ARRIVAL > ATL_Final_Dataset$ARRIVAL_TIME, 
                                    ifelse(ATL_Final_Dataset$ARRIVAL_DELAY > 0, 
                                          #SCHEDULED_ARRIVAL > ARRIVAL_TIME & ARRIVAL_DELAY > 0
                                          difftime(as.POSIXct(ATL_Final_Dataset$ARRIVAL_TIME, format = '%H:%M:%S'), as.POSIXct(ATL_Final_Dataset$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), units = 'min') + 1440, 
                                          #SCHEDULED_ARRIVAL > ARRIVAL_TIME & ARRIVAL_DELAY < 0
                                          difftime(as.POSIXct(ATL_Final_Dataset$ARRIVAL_TIME, format = '%H:%M:%S'), as.POSIXct(ATL_Final_Dataset$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), units = 'min')),
                                    
                                    ifelse(ATL_Final_Dataset$ARRIVAL_DELAY == 0, 
                                           #SCHEDULED_ARRIVAL == ARRIVAL_TIME
                                           0,
                                           ifelse(ATL_Final_Dataset$ARRIVAL_DELAY > 0,
                                                  #SCHEDULED_ARRIVAL < ARRIVAL_TIME & ARRIVAL_DELAY > 0 
                                                  difftime(as.POSIXct(ATL_Final_Dataset$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), as.POSIXct(ATL_Final_Dataset$ARRIVAL_TIME, format = '%H:%M:%S'), units = 'min'),
                                                  #SCHEDULED_ARRIVAL < ARRIVAL_TIME & ARRIVAL_DELAY < 0
                                                  difftime(as.POSIXct(ATL_Final_Dataset$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), as.POSIXct(ATL_Final_Dataset$ARRIVAL_TIME, format = '%H:%M:%S'), units = 'min')+ 1440)))
                                    

data_consistency_percentage = length(which(abs(ATL_Final_Dataset$TIME_DIFF) == abs(ATL_Final_Dataset$ARRIVAL_DELAY)))/nrow(ATL_Final_Dataset)
sprintf("Data consistency percentage of the data: %f", data_consistency_percentage)

