# this file is based on data cleaning performed on kaggle
# https://www.kaggle.com/miguelrayon/get-the-clean-dataset-in-r

library(tidyverse)
library(forcats)
auto <- read_csv("./data/autos.csv")

clean_auto<-filter(auto, brand != "sonstige_autos", # brand clearly identified
                   offerType != "Gesuch", # only offer
                   seller == "privat", #Sell by private individuals (not dealers)
                   price < 150000, price > 100, # Exclude prices lower than 100Euros or higher than 150000 Euros
                   yearOfRegistration > 1975, yearOfRegistration < 2017, #Exclude cars older than 1975 or after 2016
                   powerPS < 700, # Exclude cars were power higher than 700 HP
                   powerPS > 0 # exclude zeros 
                  ) %>%
            # drop some variables
            select (-c(dateCrawled, seller, offerType, abtest, monthOfRegistration,
                       nrOfPictures, dateCreated, lastSeen)
                    )

postal <- read.csv("./data/de_postal_codes.csv", 
                   colClasses = c("character", NULL, NULL, "factor", rep(NULL, 4)))

clean_auto <- left_join(clean_auto,
                        postal[ ,c("Postal.Code", "State.Abbreviation", "State")],
                        by = c("postalCode" = "Postal.Code"))

clean_auto$State <- as.character(clean_auto$State)
Encoding(clean_auto$State) <- "latin1"
clean_auto$State <- paste0("(", 
                           as.character(clean_auto$State.Abbreviation),
                           ") ",
                           clean_auto$State)

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
