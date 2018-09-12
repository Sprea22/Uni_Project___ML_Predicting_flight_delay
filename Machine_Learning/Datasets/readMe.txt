-------------------------
| ATL_Final_Dataset.csv |
-------------------------
This dataset is the reult of the data integration between the datasets "Weather_ATL_Restored.csv" and "Flights_delay_ATL_Restored.csv", which contains all the data about the ATL airport about flights and weather. To ensure that this dataset follows all the required quality metric, a data quality check has already been done on this dataset. 

-------------------
| TPA_Dataset.csv |
-------------------
This dataset is a susbset of the "ATL_Final_Dataset.csv" which contains all the flights between the Atlanta airport (ATL) and the Tampa airport (TPA).

-------------------
| TPA_Testset.csv |
-------------------
This dataset is a susbset of the "TPA_Dataset.csv" which contains the 30% of the flights between the Atlanta airport (ATL) and the Tampa airport (TPA). The instances contained in the dataset have been selected choosing randomly the 30% of each single month of the "TPA_Dataset.csv".

-----------------------
| TPA_Trainingset.csv |
-----------------------
This dataset is a susbset of the "TPA_Dataset.csv" which contains the 70% of the flights between the Atlanta airport (ATL) and the Tampa airport (TPA). The instances contained in the dataset have been selected choosing the instances that were not already contained in the "TPA_Testset.csv" dataset.
