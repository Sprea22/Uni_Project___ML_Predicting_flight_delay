
#install.packages("rstudioapi")
library(rstudioapi)
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets/")

################################################################
# DATASET 1: Select the origin dataset about the Flights delay #
################################################################

Flights_delay <- read.csv("Flights_delay.csv", stringsAsFactors=FALSE)

########################################################################################
# DATASET 2: Generate the dataset which is going to be used for the data quality phase #
########################################################################################

# Select correct rows
Flights_delay_ATL_Data_Quality <- Flights_delay[which(Flights_delay$DESTINATION_AIRPORT=='ATL'| 
                                       Flights_delay$ORIGIN_AIRPORT=='ATL'|
                                       Flights_delay$DESTINATION_AIRPORT=='10397' |
                                       Flights_delay$ORIGIN_AIRPORT=='10397'),]

# Select correct columns
Flights_delay_ATL_Data_Quality <- subset(Flights_delay_ATL_Data_Quality, select=c(MONTH, DAY, DAY_OF_WEEK, AIRLINE, 
                                                ORIGIN_AIRPORT, DESTINATION_AIRPORT, 
                                                SCHEDULED_DEPARTURE, SCHEDULED_TIME, 
                                                DISTANCE, SCHEDULED_ARRIVAL,
                                                ARRIVAL_DELAY))

# Save the current dataset as 'Flights_delay_ATL_Data_Quality'
write.csv(Flights_delay_ATL_Data_Quality, file = "Flights_delay_ATL_Data_Quality.csv", row.names = FALSE) 

##################################################################
#DATASET 3: Generare dataset finale dopo la fase di data cleaning#
##################################################################

Flights_delay_ATL_Restored <- Flights_delay_ATL_Data_Quality

# Remvove duplicated rows
Flights_delay_ATL_Restored = Flights_delay_ATL_Restored[!duplicated(Flights_delay_ATL_Restored), ]

###################################################
# Modify airports codes from 5 numbers to 3 chars #
###################################################

# Modify wrong label of the ATL airport
Flights_delay_ATL_Restored$DESTINATION_AIRPORT[Flights_delay_ATL_Restored$DESTINATION_AIRPORT=='10397'] <- 'ATL'
Flights_delay_ATL_Restored$ORIGIN_AIRPORT[Flights_delay_ATL_Restored$ORIGIN_AIRPORT=='10397'] <- 'ATL'

# Import the dataset that links the IATA codes with the 5 numbers codes
Airports_code_translations <- read.csv("Airports_codes_translations.csv", stringsAsFactors=FALSE)

# Split dataset in 3 categories: 
# dest = wrong codes in destination
# origin = wrong codes in origin
# rest = wrong codes in both
dest = Flights_delay_ATL_Restored[which(nchar(Flights_delay_ATL_Restored$DESTINATION_AIRPORT) != 3), ]
origin = Flights_delay_ATL_Restored[which(nchar(Flights_delay_ATL_Restored$ORIGIN_AIRPORT) != 3), ]
rest = Flights_delay_ATL_Restored[which(nchar(Flights_delay_ATL_Restored$ORIGIN_AIRPORT) == 3 &
                                                   nchar(Flights_delay_ATL_Restored$DESTINATION_AIRPORT) == 3), ]

# Find the IATA code's index that have to be substituted
dest$POSITION = unlist(sapply(dest$DESTINATION_AIRPORT, grep, Airports_code_translations$FIVE_CODE))
origin$POSITION = unlist(sapply(origin$ORIGIN_AIRPORT, grep, Airports_code_translations$FIVE_CODE))

# Substit the IATA codes in the right columns
dest$DESTINATION_AIRPORT = Airports_code_translations$IATA_CODE[dest$POSITION]
origin$ORIGIN_AIRPORT = Airports_code_translations$IATA_CODE[origin$POSITION]

# Delete the temp columns
dest$POSITION = NULL
origin$POSITION = NULL

# Rebind the datasets
temp = rbind(dest,origin)

Flights_delay_ATL_Restored = rbind(temp, rest)

# Remove NULLs
Flights_delay_ATL_Restored = Flights_delay_ATL_Restored[!is.na(Flights_delay_ATL_Restored$ARRIVAL_DELAY),]

#############################################
############### CONSTRUCT DATA ##############
#############################################

# Save the final dataset for Flights delay, called Flights_delay_ATL_Restored
write.csv(Flights_delay_ATL_Restored, file = "Flights_delay_ATL_Restored.csv", row.names = FALSE)

