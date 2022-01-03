#Importing Data----

library(reticulate)
library(tidyverse)


#1.0 Import Data----

#Python code for Kaggle api

repl_python()
import kaggle
import zipfile

from kaggle.api.kaggle_api_extended import KaggleApi
api = KaggleApi()
api.authenticate()

api.dataset_download_files('ramamet4/app-store-apple-data-set-10k-apps', path='./In_app_purchase/Data/')

with zipfile.ZipFile('./In_app_purchase/Data/app-store-apple-data-set-10k-apps.zip', 'r') as zipref:
    zipref.extractall('./In_app_purchase/Data/')
exit


##Load Data----
path_main<- "In_app_purchase/Data/AppleStore.csv"
path_desc<- "In_app_purchase/Data/appleStore_description.csv"


main_raw_tbl<- read.csv(path_main)
desc_raw_tbl<- read.csv(path_desc)

unified_raw_tbl<- merge(main_raw_tbl, desc_raw_tbl[,-c(2,3)], by = "id", all.x = T)
