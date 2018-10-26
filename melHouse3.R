
melHouseStart <- read.csv("~/Desktop/Melbourne data project/melbourne-housing-market/Melbourne_housing_FULL.csv")


library(tidyverse)

#----------------

melHouseStart <- as.tibble(melHouseStart)

# check the structure
str(melHouseStart)

#---------------

# Fix data types
# changing Distance to numeric, Propertycount to numeric, Date to date /d/m/y date format


(melHouseStart$Distance <- as.numeric(as.character(melHouseStart$Distance)))

(melHouseStart$Propertycount <- as.numeric(as.character(melHouseStart$Propertycount)))

(melHouseStart$Date <- as.Date(melHouseStart$Date, "%d/%m/%Y"))

#-------------

# Adding month and year to the columns & correcting spelling on 

melHouse <- melHouseStart %>%
  mutate(Month = as.factor(strftime(Date, "%m")),
         Year = as.factor(strftime(Date, "%y"))) %>%
  rename("Lat" = "Lattitude", "Long" = "Longtitude", "Region" = "Regionname", "PropertyCount" = "Propertycount")

glimpse(melHouse) # verifying that the columns were created and renamed

#-------------

# Finding the descriptive statistics

summary(melHouse) # descriptive statistics with out standard deviation

# getting standard deviation for each variable

melHouseSD <- melHouse %>%
  summarise_all(funs(sd(., na.rm=TRUE)))

glimpse(melHouseSD) # getting a clean print out of the SD values

#------------

# checking NA values

library(Amelia)
missmap(melHouse) # visualise missing values

colSums(is.na(melHouse))

#----------

# Finding the percentage of NA values per variable

colMeans(is.na(melHouse))*100

#---------

# inspecting rooms vs bedroom2 as Bedroom2 was scraped from a different source

melHouse %>%
  select(Rooms, Bedroom2)

# inspecting the rows that are not equal to each other
melHouse %>%
  select(Rooms, Bedroom2) %>%
  filter(Rooms != Bedroom2)

# getting a count of how many rows containt the same values
melHouse %>%
  select(Rooms, Bedroom2) %>%
  count(Rooms == Bedroom2)

# checking the correlation
cor.test(melHouse$Rooms, melHouse$Bedroom2)

# Drop Bedroom2 due to multicolinearity issues

#--------

# Dropping columns that are not needed

(melHouseDropped <- select(melHouse, - BuildingArea, - YearBuilt, - SellerG, - Lat, - Long, 
                           - Method, - Suburb, -Postcode, -Address, 
                           - PropertyCount, - CouncilArea, - Bedroom2, - Date))

#----------

# checking complete cases

melHouseCompleteCases <- complete.cases(melHouseDropped)
table(melHouseCompleteCases)

#---------

# remove NA values

melHouseNaRemoved <- na.omit(melHouseDropped)
glimpse(melHouseNaRemoved)
summary(melHouseNaRemoved)

#--------------



# test for strength of varialbles

(summary(lm(Price ~., data =melHouseNaRemoved)))

#everything is strong - check factors


#------------

# remove categorical to get numeric only

(melHouseCorr <- melHouseNaRemoved %>%
  select(- Type, - Month, - Year, - Region))

library(corrplot)
corrplot(cor(melHouseCorr), method = 'number')


#-----------

# get statistics first and correlations before running this
# so the columns are preshrunk and not needed to be repeated

#separate into 4 groups, all, house, unit, townhouse (h/u/t)


(MelHouseUnit <- melHouseNaRemoved %>%
   filter(Type == 'u'))

(melHouseTown <- melHouseNaRemoved %>%
    filter(Type == 'h'))

(melHouseHouse <- melHouseNaRemoved %>%
    filter(Type == 't'))

#---------------------------




# bar charts for categorical variables even if being deleted
# scatterplots for descriptive
# boxplots for outliers
# determine what to do for outliers
# test for normality
# separate months and years into quarters
# find inflation rates and add them retroactively to years
