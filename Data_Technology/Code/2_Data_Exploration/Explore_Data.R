
# Install.packages("plyr")
library(plyr)
# Install.packages("rstudioapi")
library(rstudioapi)
# The following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets/")

Flights_delay_ATL_Data_Quality<- read.csv("Flights_delay_ATL_Data_Quality.csv", stringsAsFactors=FALSE)
Weather_ATL <- read.csv("Weather_ATL.csv", stringsAsFactors=FALSE)

# Install.packages("plyr")
# Library for rename
library(plyr)

################
# Explore data #
################

###########################################################################################

# Analysis performed on the Weather_ATL dataset

# I copy the Weather_ATL (renamed Weather_ATL_Data_Quality) dataset to a supporting dataset
# to perform data exploration, without changing the original dataset
Weather_ATL_v2 = Weather_ATL

# Precipitation analysis
# I replace the values in coloumns "Precipitation" with some predetermined value to evaluate the intensity of precipitation 
Weather_ATL_v2$Precipitation <-ifelse(Weather_ATL_v2$Precipitation  <= 0.001 | Weather_ATL_v2$Precipitation  == "T" , "No precipitation",
                                       ifelse(Weather_ATL_v2$Precipitation  <= 0.1 , "Light rain",
                                              ifelse(Weather_ATL_v2$Precipitation <= 0.79 , "Rain",
                                                     ifelse(Weather_ATL_v2$Precipitation <= 1.5 , "Heavy rain",
                                                            ifelse(Weather_ATL_v2$Precipitation > 1.5, "Storm",0)))))
# Create precipitation table
Precipitation_per_day = data.frame(table(Weather_ATL_v2$Precipitation ))
# Rename the titles of the created table  
Precipitation_per_day = rename(Precipitation_per_day, c("Var1"= "Precipitation type", "Freq" = "Numbers of days"))
# Order the results in the precipitation table
Precipitation_per_day = Precipitation_per_day[c(3,2,4,1,5),]

# Analysis on HDD (Heating degree day) / CDD (Cooling Degree Day) 
# (<65 F °) HDD> 0, (> 65 F °) CDD> 0, neutral if 0 to both
hdd = sum(Weather_ATL_v2$HDD >0)
neutral = sum(Weather_ATL_v2$HDD == 0 & Weather_ATL_v2$CDD == 0)
cdd= sum(Weather_ATL_v2$CDD >0)

#I replace the values in the support dataset to get some statistics
Weather_ATL_v2$Temperature <- ifelse(Weather_ATL_v2$HDD >0, "HDD",
                                      ifelse(Weather_ATL_v2$HDD == 0 & Weather_ATL_v2$CDD == 0 , "Neutral",
                                             ifelse(Weather_ATL_v2$CDD >0, "CDD",0)))
# Create temperature table
Temperature = data.frame(table(Weather_ATL_v2$Temperature ))
# Rename the titles of the created table  
Temperature = rename(Temperature, c("Var1"= "Category", "Freq" = "Numbers of days"))

###########################################################################################

# Analysis on the Flights_delay_ATL_Data_Quality dataset

# copy the dataset Flights_delay_ATL_Data_Quality on a support dataset to perform
# data exploration, without changing the original dataset

Flights_delay_ATL_Data_Quality_v2 = Flights_delay_ATL_Data_Quality

# Delete the lines inconsistent with the NA values, in our case we ran the program once with complete values also with the NA lines (commenting the line below) 
# and then re-executed by eliminating the NA values then making a difference to understand the dimension

Flights_delay_ATL_Data_Quality_v2= na.omit(Flights_delay_ATL_Data_Quality_v2, cols=Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY)


# Find total flights/delayed flights
All_flights = nrow(Flights_delay_ATL_Data_Quality_v2)
All_flights_delayed = data.frame(table(which(Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY>0)))
All_flights_delayed = sum(All_flights_delayed$Freq)
All_flights_delayed_Table = data.frame(table(All_flights,All_flights_delayed))
All_flights_delayed_Table$Freq = NULL
# Rename the titles of the created table
All_flights_delayed_Table = rename(All_flights_delayed_Table, c("All_flights"= "All flights", "All_flights_delayed" = "All flights delayed"))


# Find flights per days of week
Flights_per_day = data.frame(table(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK))
# Rename the titles of the created table  
Flights_per_day = rename(Flights_per_day, c("Var1"= "Days of week", "Freq" = "Number of flights"))

# Find delayed flights per days of week

rit1 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 1 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit1 = rit1$freq[2]
rit2 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 2 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit2 = rit2$freq[2]
rit3 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 3 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit3 = rit3$freq[2]
rit4 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 4 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit4 = rit4$freq[2]
rit5 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 5 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit5 = rit5$freq[2]
rit6 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 6 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit6 = rit6$freq[2]
rit7 = count(Flights_delay_ATL_Data_Quality_v2$DAY_OF_WEEK == 7 & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
rit7 = rit7$freq[2]

# insert the flight delay in the table 
Flights_per_day$`Number of delayed flights` <- ifelse(Flights_per_day$`Days of week`== 1, rit1,
                                                    ifelse(Flights_per_day$`Days of week`== 2, rit2,
                                                           ifelse(Flights_per_day$`Days of week`== 3, rit3,
                                                                  ifelse(Flights_per_day$`Days of week`== 4, rit4,
                                                                         ifelse(Flights_per_day$`Days of week`== 5, rit5,
                                                                                ifelse(Flights_per_day$`Days of week`== 6, rit6,
                                                                                       ifelse(Flights_per_day$`Days of week`== 7, rit7,0)))))))

# Find flights by distance

# Replace the different categories by distance in the support dataset
# Category: 0-450 miles short distance (short distance)
# 451-1500 miles medium distance (medium distance)
# > 1500 miles long distances (long distance)

Flights_delay_ATL_Data_Quality_v2$DISTANCE <- ifelse(Flights_delay_ATL_Data_Quality_v2$DISTANCE<= 450, "short distance",
                                ifelse(Flights_delay_ATL_Data_Quality_v2$DISTANCE<= 1500 , "medium distance",
                                       ifelse(Flights_delay_ATL_Data_Quality_v2$DISTANCE> 1500, "long distance",0)))
# Create a table with flights divided by distance
Distance = data.frame(table(Flights_delay_ATL_Data_Quality_v2$DISTANCE))
# Rename the titles of the created table  
Distance = rename(Distance, c("Var1"= "Type of distance", "Freq" = "Number of flights"))

# Delay on short/medium/long distance
shrit = count(Flights_delay_ATL_Data_Quality_v2$DISTANCE == "short distance" & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
shrit = shrit$freq[2]
medrit = count(Flights_delay_ATL_Data_Quality_v2$DISTANCE == "medium distance" & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
medrit = medrit$freq[2]
longrit = count(Flights_delay_ATL_Data_Quality_v2$DISTANCE == "long distance" & Flights_delay_ATL_Data_Quality_v2$ARRIVAL_DELAY >0)
longrit = longrit$freq[2]

# insert the flight delay in the table 
Distance$`Number of delayed flights` <- ifelse(Distance$`Type of distance`== "short distance", shrit,
                                               ifelse(Distance$`Type of distance`== "medium distance",medrit,
                                                      ifelse(Distance$`Type of distance`== "long distance",longrit,0)))

# Order the results in the distance table
Distance = Distance[c(3,2,1),]

