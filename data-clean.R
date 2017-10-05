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
                    ) %>%
            # adjust extraordinary low kilometer values to more trusty ones
            # (insurance companies in Germany use 15000 km/year in their formulas), 
            # but we allow 10000 km/year as a more or less sensible minimun 
            mutate (kilometer = ifelse((2017 - yearOfRegistration) * 10000 <= kilometer, 
                                            kilometer, 
                                           (2017 - yearOfRegistration) * 10000
                                           )
                    )

postal <- read.csv("./data/de_postal_codes.csv", 
                   colClasses = c("character", NULL, NULL, "factor", rep(NULL, 4)))

clean_auto <- left_join(clean_auto,
                        postal[ ,c("Postal.Code", "State.Abbreviation")],
                        by = c("postalCode" = "Postal.Code"))

## convert to factor
clean_auto$brand <- factor(clean_auto$brand)
clean_auto$gearbox <- factor(clean_auto$gearbox)
clean_auto$postalCode <- factor(clean_auto$postalCode)
clean_auto$fuelType <- factor(clean_auto$fuelType)
clean_auto$notRepairedDamage <- factor(clean_auto$notRepairedDamage)
clean_auto$vehicleType <- factor(clean_auto$vehicleType)
clean_auto$State.Abbreviation <- factor(clean_auto$State.Abbreviation)


## free memory
rm(auto, postal)
