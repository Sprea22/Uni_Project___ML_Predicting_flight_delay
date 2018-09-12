----------------
| Airlines.csv | 
----------------
This dataset provides the IATA code for each single airline contained in the 'Flights_delay.csv' dataset.

----------------
| Airports.csv |
----------------
This dataset provides several information (as name, location, IATA code,..) for each single airport contained in the 'Flights_delay.csv' dataset.


-----------------------------------
| Airports_codes_translations.csv |
-----------------------------------
This dataset provides the IATA code and the associated five-numbers code for each single airline contained in the 'Flights_delay.csv' dataset.


-------------------------
| ATL_Final_Dataset.csv |
-------------------------
This dataset is the reult of the data integration between the datasets "Weather_ATL_Restored.csv" and "Flights_delay_ATL_Restored.csv", which contains all the data about the ATL airport about flights and weather. To ensure that this dataset follows all the required quality metric, a data quality check has already been done on this dataset. 

---------------------
| Flights_delay.csv |
---------------------
This dataset is the original dataset. It contains 5819079 observations and 31 variables about national flights within the United States during the year 2015. 

--------------------------------------
| Flights_delay_ATL_Data_Quality.csv |
--------------------------------------
This dataset is a subset of the original dataset 'Flights_delay.csv'; in particular it's filtered by the columns that are going to be used during this work and just the rows that are refering to the Atlanta airport.

----------------------------------
| Flights_delay_ATL_Restored.csv |
----------------------------------
This dataset is the 'Flights_delay_ATL_Data_Quality.csv' after the data quality phase; it means that the current dataset follows all the required data metrics.

-------------------
| Weather_ATL.csv |
-------------------
This dataset contains 365 observations and 10 variables about the Atlanta city's weather during the 2015 year.

--------------------------------
| Weather_ATL_Data_Quality.csv |
--------------------------------
This dataset is a subset of the original dataset 'Weather_ATL.csv'; in particular it's filtered by the columns that are going to be used during this work.

----------------------------
| Weather_ATL_Restored.csv |
----------------------------
This dataset is the 'Weather_ATL_Data_Quality.csv' after the data quality phase; it means that the current dataset follows all the required data metrics.
