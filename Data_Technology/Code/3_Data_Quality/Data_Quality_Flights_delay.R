
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

Flights_delay_ATL_Data_Quality <- read.csv("Flights_delay_ATL_Data_Quality.csv", stringsAsFactors=FALSE)
Number_of_rows_dataset = nrow(Flights_delay_ATL_Data_Quality)
Number_of_cols_dataset = ncol(Flights_delay_ATL_Data_Quality)

Number_of_duplicated_tuple = Number_of_rows_dataset - nrow(unique(Flights_delay_ATL_Data_Quality))
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

Number_of_correct_airline = length(Flights_delay_ATL_Data_Quality$AIRLINE[Flights_delay_ATL_Data_Quality$AIRLINE %in% iata_airline])
Number_of_correct_destination = length(Flights_delay_ATL_Data_Quality$DESTINATION_AIRPORT[Flights_delay_ATL_Data_Quality$DESTINATION_AIRPORT %in% iata_airport])
Number_of_correct_origin = length(Flights_delay_ATL_Data_Quality$ORIGIN_AIRPORT[Flights_delay_ATL_Data_Quality$ORIGIN_AIRPORT %in% iata_airport])

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

null = sum(is.na(Flights_delay_ATL_Data_Quality))
not_null = sum(!is.na(Flights_delay_ATL_Data_Quality))
Number_of_values_dataset = Number_of_rows_dataset*Number_of_cols_dataset
Comleteness_degree = not_null/Number_of_values_dataset

sprintf("Percentage of NOT NULL values: %f", Comleteness_degree)

######################################
# Referencial Integrity Consistency  #
######################################

percentage_correct_month = length(Flights_delay_ATL_Data_Quality$MONTH[typeof(Flights_delay_ATL_Data_Quality$MONTH) == "integer" & Flights_delay_ATL_Data_Quality$MONTH < 13 & Flights_delay_ATL_Data_Quality$MONTH > 0])/Number_of_rows_dataset
percentage_correct_day = length(Flights_delay_ATL_Data_Quality$DAY[typeof(Flights_delay_ATL_Data_Quality$DAY) == "integer" & Flights_delay_ATL_Data_Quality$DAY < 32 & Flights_delay_ATL_Data_Quality$DAY > 0])/Number_of_rows_dataset
percentage_correct_day_of_week = length(Flights_delay_ATL_Data_Quality$DAY_OF_WEEK[typeof(Flights_delay_ATL_Data_Quality$DAY_OF_WEEK) == "integer" & Flights_delay_ATL_Data_Quality$DAY_OF_WEEK > 0 & Flights_delay_ATL_Data_Quality$DAY_OF_WEEK < 8])/Number_of_rows_dataset
percentage_correct_airline = length(Flights_delay_ATL_Data_Quality$AIRLINE[typeof(Flights_delay_ATL_Data_Quality$AIRLINE) == "character" & nchar(Flights_delay_ATL_Data_Quality$AIRLINE) == 2])/Number_of_rows_dataset
percentage_correct_origin = length(Flights_delay_ATL_Data_Quality$ORIGIN_AIRPORT[typeof(Flights_delay_ATL_Data_Quality$ORIGIN_AIRPORT) == "character" & nchar(Flights_delay_ATL_Data_Quality$ORIGIN_AIRPORT) == 3])/Number_of_rows_dataset
percentage_correct_destination = length(Flights_delay_ATL_Data_Quality$DESTINATION_AIRPORT[typeof(Flights_delay_ATL_Data_Quality$DESTINATION_AIRPORT) == "character" & nchar(Flights_delay_ATL_Data_Quality$DESTINATION_AIRPORT) == 3])/Number_of_rows_dataset
percentage_correct_departure = length(Flights_delay_ATL_Data_Quality$SCHEDULED_DEPARTURE[typeof(Flights_delay_ATL_Data_Quality$SCHEDULED_DEPARTURE) == "integer" & Flights_delay_ATL_Data_Quality$SCHEDULED_DEPARTURE < 2400 & Flights_delay_ATL_Data_Quality$SCHEDULED_DEPARTURE >= 0])/Number_of_rows_dataset 
percentage_correct_time = length(Flights_delay_ATL_Data_Quality$SCHEDULED_TIME[typeof(Flights_delay_ATL_Data_Quality$SCHEDULED_TIME) == "integer"])/Number_of_rows_dataset
percentage_correct_distance = length(Flights_delay_ATL_Data_Quality$DISTANCE[typeof(Flights_delay_ATL_Data_Quality$DISTANCE) == "integer"])/Number_of_rows_dataset
percentage_correct_arrival = length(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL[typeof(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL) == "integer"  & Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL < 2400 & Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL >= 0]) /Number_of_rows_dataset
percentage_correct_delay = length(Flights_delay_ATL_Data_Quality$ARRIVAL_DELAY[typeof(Flights_delay_ATL_Data_Quality$ARRIVAL_DELAY) == "integer"])/Number_of_rows_dataset

average_ref_int_consistency_percentage = (percentage_correct_month+percentage_correct_day+percentage_correct_day_of_week+percentage_correct_airline+
  percentage_correct_origin+percentage_correct_destination+percentage_correct_departure+percentage_correct_time+
  percentage_correct_distance+percentage_correct_arrival+percentage_correct_delay)/11

sprintf("Average referencial integrity consistency percentage of the data: %f", average_ref_int_consistency_percentage)

#####################
# Data Consistency  #
#####################

# The following code is going to check that the column ARRIVAL_TIME, SCHEDULED_ARRIVAL and ARRIVAL_DELAY are consistents.

Flights_delay_ATL_Data_Quality$ARRIVAL_TIME = substr(as.POSIXct(sprintf("%04.0f", Flights_delay_ATL_Data_Quality$ARRIVAL_TIME), format='%H%M'), 12, 16)
Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL = substr(as.POSIXct(sprintf("%04.0f", Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL), format='%H%M'), 12, 16)

Flights_delay_ATL_Data_Quality$ARRIVAL_TIME = do.call(paste, c(Flights_delay_ATL_Data_Quality[c("ARRIVAL_TIME")], "00", sep = ":"))
Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL = do.call(paste, c(Flights_delay_ATL_Data_Quality[c("SCHEDULED_ARRIVAL")], "00", sep = ":"))

Flights_delay_ATL_Data_Quality$ARRIVAL_TIME = format(as.POSIXct(Flights_delay_ATL_Data_Quality$ARRIVAL_TIME, format="%H:%M:%S"), "%H:%M:%S")
Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL = format(as.POSIXct(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL, format="%H:%M:%S"), "%H:%M:%S")

Flights_delay_ATL_Data_Quality$ARRIVAL_TIME[Flights_delay_ATL_Data_Quality$ARRIVAL_TIME == "00:00:00"] = format(as.POSIXct("24:00:00", format="%H:%M:%S"), "%H:%M:%S")
Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL[Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL == "00:00:00"] = format(as.POSIXct("24:00:00", format="%H:%M:%S"), "%H:%M:%S")


Flights_delay_ATL_Data_Quality$TIME_DIFF = ifelse(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL > Flights_delay_ATL_Data_Quality$ARRIVAL_TIME, 
                                    ifelse(Flights_delay_ATL_Data_Quality$ARRIVAL_DELAY > 0, 
                                          #SCHEDULED_ARRIVAL > ARRIVAL_TIME & ARRIVAL_DELAY > 0
                                          difftime(as.POSIXct(Flights_delay_ATL_Data_Quality$ARRIVAL_TIME, format = '%H:%M:%S'), as.POSIXct(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), units = 'min') + 1440, 
                                          #SCHEDULED_ARRIVAL > ARRIVAL_TIME & ARRIVAL_DELAY < 0
                                          difftime(as.POSIXct(Flights_delay_ATL_Data_Quality$ARRIVAL_TIME, format = '%H:%M:%S'), as.POSIXct(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), units = 'min')),
                                    
                                    ifelse(Flights_delay_ATL_Data_Quality$ARRIVAL_DELAY == 0, 
                                           #SCHEDULED_ARRIVAL == ARRIVAL_TIME
                                           0,
                                           ifelse(Flights_delay_ATL_Data_Quality$ARRIVAL_DELAY > 0,
                                                  #SCHEDULED_ARRIVAL < ARRIVAL_TIME & ARRIVAL_DELAY > 0 
                                                  difftime(as.POSIXct(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), as.POSIXct(Flights_delay_ATL_Data_Quality$ARRIVAL_TIME, format = '%H:%M:%S'), units = 'min'),
                                                  #SCHEDULED_ARRIVAL < ARRIVAL_TIME & ARRIVAL_DELAY < 0
                                                  difftime(as.POSIXct(Flights_delay_ATL_Data_Quality$SCHEDULED_ARRIVAL, format = '%H:%M:%S'), as.POSIXct(Flights_delay_ATL_Data_Quality$ARRIVAL_TIME, format = '%H:%M:%S'), units = 'min')+ 1440)))
                                    

data_consistency_percentage = length(which(abs(Flights_delay_ATL_Data_Quality$TIME_DIFF) == abs(Flights_delay_ATL_Data_Quality$ARRIVAL_DELAY)))/nrow(Flights_delay_ATL_Data_Quality)
sprintf("Data consistency percentage of the data: %f", data_consistency_percentage)

