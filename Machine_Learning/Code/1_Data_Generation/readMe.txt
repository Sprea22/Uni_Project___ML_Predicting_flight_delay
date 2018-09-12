----------------------------
| TPA_Dataset_Generation.R |
----------------------------
This R file select the dataset that contains all the flights about the ATL airport, and it generates a new dataset called "TPA_Dataset.csv" which contains all the flights between the ATL and the TPA airports.

----------------------
| ML_1Year_30%_TPA.R |
----------------------
This R file selects the dataset that contains all the flights between the airports of ATL and TPA, that is "TPA_Dataset.csv".
Then it generates and saves the following datasets in the folder "Machine_Learning/Test_Dataset":
- "TPA_Trainingset.csv": 70% of the TPA_Dataset.csv
- "TPA_Testset.csv": 30% of the TPA_Dataset.csv


