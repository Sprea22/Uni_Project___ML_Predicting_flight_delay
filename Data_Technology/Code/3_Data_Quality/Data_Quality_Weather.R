
#install.packages("rstudioapi")

library(rstudioapi)
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))

setwd("../../Datasets/")


##############
# Uniqueness #
##############

Weather_ATL_Data_Quality <- read.csv("Weather_ATL_Data_Quality.csv", stringsAsFactors=FALSE)
Number_of_rows_dataset = nrow(Weather_ATL_Data_Quality)
Number_of_cols_dataset = ncol(Weather_ATL_Data_Quality)
     
Number_of_duplicated_tuple = Number_of_rows_dataset - nrow(unique(Weather_ATL_Data_Quality))
sprintf("Number of duplicated tuple: %s", Number_of_duplicated_tuple)


######################
# Table Completeness #
######################

null = sum(is.na(Weather_ATL_Data_Quality))
not_null = sum(!is.na(Weather_ATL_Data_Quality))
Number_of_values_dataset = Number_of_rows_dataset*Number_of_cols_dataset
Comleteness_degree = not_null/Number_of_values_dataset

sprintf("Percentage of NOT NULL values: %f", Comleteness_degree)

######################################
# Referencial Integrity Consistency  #
######################################

percentage_correct_date = length(as.Date(as.character(Weather_ATL_Data_Quality$Date),format="%m/%d/%Y"))/Number_of_rows_dataset
percentage_correct_minTemp = length(Weather_ATL_Data_Quality$Date[typeof(Weather_ATL_Data_Quality$MinTemp) == "integer" & Weather_ATL_Data_Quality$MinTemp >= -100])/Number_of_rows_dataset
percentage_correct_maxTemp = length(Weather_ATL_Data_Quality$Date[typeof(Weather_ATL_Data_Quality$MaxTemp) == "integer" & Weather_ATL_Data_Quality$MaxTemp < 100])/Number_of_rows_dataset
percentage_correct_precipitation = length(Weather_ATL_Data_Quality$Date[typeof(Weather_ATL_Data_Quality$Precipitation) == "character" & as.numeric(Weather_ATL_Data_Quality$Precipitation[Weather_ATL_Data_Quality$Precipitation != "T"])>=0])/Number_of_rows_dataset
  
average_ref_int_consistency_percentage = (percentage_correct_date+percentage_correct_minTemp+
                                          percentage_correct_maxTemp+percentage_correct_precipitation)/4

sprintf("Average referencial integrity consistency percentage of the data: %f", average_ref_int_consistency_percentage)

#####################
# Data Consistency  #
#####################

consistent_rows = length(which(Weather_ATL_Data_Quality$MinTemp < Weather_ATL_Data_Quality$MaxTemp ))
data_consistency_percentage = consistent_rows / Number_of_rows_dataset 

sprintf("Data consistency percentage of the data: %f", data_consistency_percentage)
