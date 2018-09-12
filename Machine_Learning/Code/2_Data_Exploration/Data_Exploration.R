
#############################
# EXPLORE DATA TRAINING SET #
#############################

library("plyr")
library("rstudioapi")

# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
setwd("../../Datasets/")
# Import the final dataset about the ATL-TPA Airports
TPA_Trainingset <- read.csv("TPA_Trainingset.csv", stringsAsFactors=FALSE)

# create a support dataset 
TPA_Dataset_v1 = TPA_Trainingset

################
# Explore data #
################

###########################################################################################

# Precipitation analysis
# I replace the values in coloumns "Precipitation" with some predetermined value to evaluate the intensity of precipitation 
TPA_Dataset_v1$PRECIPITATION <-ifelse(TPA_Dataset_v1$PRECIPITATION  <= 0.001 | TPA_Dataset_v1$PRECIPITATION  == "T" , "No precipitation",
                                      ifelse(TPA_Dataset_v1$PRECIPITATION  <= 0.1 , "Light rain",
                                             ifelse(TPA_Dataset_v1$PRECIPITATION <= 0.79 , "Rain",
                                                    ifelse(TPA_Dataset_v1$PRECIPITATION <= 1.5 , "Heavy rain",
                                                           ifelse(TPA_Dataset_v1$PRECIPITATION > 1.5, "Storm",0)))))
# Create precipitation table
Precipitation_per_numflights = data.frame(table(TPA_Dataset_v1$PRECIPITATION ))
# Rename the titles of the created table  
Precipitation_per_numflights = rename(Precipitation_per_numflights, c("Var1"= "Precipitation type", "Freq" = "Numbers of flights"))
# Order the results in the precipitation table
Precipitation_per_numflights = Precipitation_per_numflights[c(3,2,4,1,5),]

# Count delayed flights per precipitation type
ritP1 = count(TPA_Dataset_v1$PRECIPITATION == "No precipitation" & TPA_Dataset_v1$ARRIVAL_DELAY >0)
ritP1 = ritP1$freq[2]
ritP2 = count(TPA_Dataset_v1$PRECIPITATION == "Light rain" & TPA_Dataset_v1$ARRIVAL_DELAY >0)
ritP2 = ritP2$freq[2]
ritP3 = count(TPA_Dataset_v1$PRECIPITATION == "Rain" & TPA_Dataset_v1$ARRIVAL_DELAY >0)
ritP3 = ritP3$freq[2]
ritP4 = count(TPA_Dataset_v1$PRECIPITATION == "Heavy rain" & TPA_Dataset_v1$ARRIVAL_DELAY >0)
ritP4 = ritP4$freq[2]
ritP5 = count(TPA_Dataset_v1$PRECIPITATION == "Storm" & TPA_Dataset_v1$ARRIVAL_DELAY >0)
ritP5 = ritP5$freq[2]

# I add coloumns "Number of delayed flights" 
Precipitation_per_numflights$"Number of delayed flights" <-ifelse(Precipitation_per_numflights$`Precipitation type` == "No precipitation", ritP1,
                                      ifelse(Precipitation_per_numflights$`Precipitation type` == "Light rain", ritP2,
                                             ifelse(Precipitation_per_numflights$`Precipitation type` == "Rain", ritP3,
                                                    ifelse(Precipitation_per_numflights$`Precipitation type` == "Heavy rain", ritP4,
                                                           ifelse(Precipitation_per_numflights$`Precipitation type` == "Storm",ritP5, 0)))))


###########################################################################################

# Find total flights/delayed flights
All_flights_TPA = nrow(TPA_Dataset_v1)
All_flights_delayed_TPA = data.frame(table(which(TPA_Dataset_v1$ARRIVAL_DELAY>0)))
All_flights_delayed_TPA = sum(All_flights_delayed_TPA$Freq)
All_flights_delayed_TPA_Table = data.frame(table(All_flights_TPA,All_flights_delayed_TPA))
All_flights_delayed_TPA_Table$Freq = NULL
# Rename the titles of the created table
All_flights_delayed_TPA_Table = rename(All_flights_delayed_TPA_Table, c("All_flights_TPA"= "All flights", "All_flights_delayed_TPA" = "All flights delayed"))


# Find flights per days of week
Flights_per_day_TPA = data.frame(table(TPA_Dataset_v1$DAY_OF_WEEK))
# Rename the titles of the created table  
Flights_per_day_TPA = rename(Flights_per_day_TPA, c("Var1"= "Days of week", "Freq" = "Number of flights"))

# Find delayed flights per days of week

rit1 = count(TPA_Dataset_v1$DAY_OF_WEEK == 1 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit1 = rit1$freq[2]
rit2 = count(TPA_Dataset_v1$DAY_OF_WEEK == 2 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit2 = rit2$freq[2]
rit3 = count(TPA_Dataset_v1$DAY_OF_WEEK == 3 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit3 = rit3$freq[2]
rit4 = count(TPA_Dataset_v1$DAY_OF_WEEK == 4 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit4 = rit4$freq[2]
rit5 = count(TPA_Dataset_v1$DAY_OF_WEEK == 5 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit5 = rit5$freq[2]
rit6 = count(TPA_Dataset_v1$DAY_OF_WEEK == 6 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit6 = rit6$freq[2]
rit7 = count(TPA_Dataset_v1$DAY_OF_WEEK == 7 & TPA_Dataset_v1$ARRIVAL_DELAY >0)
rit7 = rit7$freq[2]

# insert the flight delay in the table 
Flights_per_day_TPA$`Number of delayed flights` <- ifelse(Flights_per_day_TPA$`Days of week`== 1, rit1,
                                                      ifelse(Flights_per_day_TPA$`Days of week`== 2, rit2,
                                                             ifelse(Flights_per_day_TPA$`Days of week`== 3, rit3,
                                                                    ifelse(Flights_per_day_TPA$`Days of week`== 4, rit4,
                                                                           ifelse(Flights_per_day_TPA$`Days of week`== 5, rit5,
                                                                                  ifelse(Flights_per_day_TPA$`Days of week`== 6, rit6,

                                                                                                                                                                                  ifelse(Flights_per_day_TPA$`Days of week`== 7, rit7,0)))))))
###########################################################################################

TPA_Dataset_v2 = TPA_Trainingset

##########################
# factor support dataset #
##########################

TPA_Dataset_v2$AIRLINE = factor(TPA_Dataset_v2$AIRLINE)
TPA_Dataset_v2$ARRIVAL_DELAY = factor(TPA_Dataset_v2$ARRIVAL_DELAY)
TPA_Dataset_v2$DAY = factor(TPA_Dataset_v2$DAY)
TPA_Dataset_v2$DAY_OF_WEEK = factor(TPA_Dataset_v2$DAY_OF_WEEK)
TPA_Dataset_v2$DESTINATION_AIRPORT = factor(TPA_Dataset_v2$DESTINATION_AIRPORT)
TPA_Dataset_v2$ORIGIN_AIRPORT = factor(TPA_Dataset_v2$ORIGIN_AIRPORT)
TPA_Dataset_v2$MONTH = factor(TPA_Dataset_v2$MONTH)

###################
# create graphics #
###################

#AIRLINE - ARRIVAL_DELAY
plot(TPA_Dataset_v2$AIRLINE, TPA_Dataset_v2$ARRIVAL_DELAY, col = TPA_Dataset_v2$ARRIVAL_DELAY)
#legend('topleft', legend = levels(TPA_Dataset_v2$ARRIVAL_DELAY), col = c(1,2), pch = 16)

#DAY - ARRIVAL_DELAY
plot(TPA_Dataset_v2$DAY, TPA_Dataset_v2$ARRIVAL_DELAY, col = TPA_Dataset_v2$ARRIVAL_DELAY)
#legend('topleft', legend = levels(TPA_Dataset_v2$ARRIVAL_DELAY), col = c(1,2), pch = 16)

#DAY_OF_WEEK - ARRIVAL_DELAY
plot(TPA_Dataset_v2$DAY_OF_WEEK, TPA_Dataset_v2$ARRIVAL_DELAY, col = TPA_Dataset_v2$ARRIVAL_DELAY)
#legend('topleft', legend = levels(TPA_Dataset_v2$ARRIVAL_DELAY), col = c(1,2), pch = 16)

#MONTH - ARRIVAL_DELAY
plot(TPA_Dataset_v2$MONTH, TPA_Dataset_v2$ARRIVAL_DELAY, col = TPA_Dataset_v2$ARRIVAL_DELAY)
#legend('topleft', legend = levels(TPA_Dataset_v2$ARRIVAL_DELAY), col = c(1,2), pch = 16)

#DESTINATION_AIRPORT - ARRIVAL_DELAY
plot(TPA_Dataset_v2$DESTINATION_AIRPORT, TPA_Dataset_v2$ARRIVAL_DELAY, col = TPA_Dataset_v2$ARRIVAL_DELAY)
#legend('topleft', legend = levels(TPA_Dataset_v2$ARRIVAL_DELAY), col = c(1,2), pch = 16)

