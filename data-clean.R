library(tidyverse)
library(forcats)

read_auto_cached <- function ()
{
  if (!exists("CACHED_AUTO_DATA"))
    CACHED_AUTO_DATA <<- clean_auto_data()
  
  (CACHED_AUTO_DATA)
}

clean_auto_data <- function(path = "./data/",
                            fileClean = "clean_auto.csv") {
  
  cleanFullPath <- paste0(path, fileClean)
  
  if (file.exists(cleanFullPath)) {
    data <- read.csv(cleanFullPath)
  } else {
    data <- full_auto_clean(path = path)
    write.csv(data, cleanFullPath)
  }
  data
}


full_auto_clean <- function(path = "./data/", 
                            fileRaw = "autos.csv",
                            postalCodes = "de_postal_codes.csv"){
  
  tryCatch(auto <- read_csv(paste0(path, fileRaw)),
           error = function(e) 
             stop(sprintf("cannot read file: %s", paste0(path, fileRaw))))
  
  # the cleaning process based on the data cleaning performed on kaggle
  # https://www.kaggle.com/miguelrayon/get-the-clean-dataset-in-r
  
  clean_auto <- dplyr::filter(auto, brand != "sonstige_autos", # brand clearly identified
                     offerType != "Gesuch", # only offer
                     seller == "privat", #Sell by private individuals (not dealers)
                     price < 150000, price > 100, # Exclude prices lower than 100Euros or higher than 150000 Euros
                     yearOfRegistration > 1975, yearOfRegistration < 2017, #Exclude cars older than 1975 or after 2016
                     powerPS < 700, # Exclude cars were power higher than 700 HP
                     powerPS > 0 # exclude zeros 
  ) %>%
    # drop some variables
    dplyr::select (-c(dateCrawled, seller, offerType, abtest, 
                      monthOfRegistration, nrOfPictures, dateCreated, 
                      lastSeen))
  
  postal <- read.csv("./data/de_postal_codes.csv", 
                     colClasses = c("character", "NULL", "character", "character", "NULL", "numeric", "numeric", "NULL"))
  
  clean_auto <- left_join(clean_auto,
                          postal[ ,c("Postal.Code", "State.Abbreviation", "State",
                                     "Latitude", "Longitude")],
                          by = c("postalCode" = "Postal.Code"))
  
  Encoding(clean_auto$State) <- "latin1"
  clean_auto$State <- paste0("(", clean_auto$State.Abbreviation, ") ", clean_auto$State)
  
  ## convert to factor
  clean_auto$brand <- factor(clean_auto$brand)
  clean_auto$gearbox <- factor(clean_auto$gearbox)
  clean_auto$postalCode <- factor(clean_auto$postalCode)
  clean_auto$fuelType <- factor(clean_auto$fuelType)
  clean_auto$notRepairedDamage <- factor(clean_auto$notRepairedDamage)
  clean_auto$vehicleType <- factor(clean_auto$vehicleType)
  clean_auto$State <- factor(clean_auto$State)
  
  ## free memory
  rm(auto, postal)
  
  clean_auto
}


