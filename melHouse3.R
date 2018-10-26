


melHouseStart <- read.csv("///Melbourne data project/melbourne-housing-market/Melbourne_housing_FULL.csv")


library(tidyverse)

#----------------

# check the structure and summary 
str(melHouseStart)
summary(melHouseStart)



#------------

#rename latitude and longitude

(melHouse <- melHouseStart %>% 
   rename("Lat" = "Lattitude", "Long" = "Longtitude"))

#-----------------

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

# Dropping Bedroom2 due to multicolinearity issues
melHouse <- select(melHouse, -Bedroom2)


#------------

# Logical test for complete rows

(melHouseCompleteCases <- complete.cases(melHouseDropped))

#---------------

# checking NA values

library(Amelia)
missmap(melHouse) # visualise missing values

colSums(is.na(melHouse))

#----------

# Finding the percentage of NA values per variable

colMeans(is.na(melHouse))*100

#---------

# Dropping columns that are not needed

(melHouseDropped <- select(melHouse, - BuildingArea, - YearBuilt, - SellerG, - Lat, - Long, 
                           - Method, - Suburb, -Postcode, -Address, - Propertycount, - CouncilArea))

#----------

# remove NA values

melHouseNaRemoved <- na.omit(melHouseDropped)
str(melHouseNaRemoved)
summary(melHouseNaRemoved)


#---------------

# change distance to a numeric

(melHouseNaRemoved$Distance <- as.numeric(as.character(melHouseNaRemoved$Distance)))

#---------------

# convert date to month and year as well as add to tibble

(melHouseNaRemoved$Date <- as.Date(melHouseNaRemoved$Date, "%d/%m/%Y"))

melHouseMonthYear <- melHouseNaRemoved %>%
  mutate(Month = as.factor(strftime(Date, "%m")),
         Year = as.factor(strftime(Date, "%y")))

# check classes to ensure code worked
glimpse(melHouseMonthYear)


# dropping the Date column

melHouseMonthYear <- melHouseMonthYear %>%
  select(-Date)

glimpse(melHouseMonthYear)

#--------------

# create quarters for the year based on months






#----------

# test for strength of varialbles

(summary(lm(Price ~., data =melHouseMonthYear)))

#everything is strong


#------------

# remove categorical to get numeric only

melHouseCorr <- melHouseNaRemoved %>%
  select(- Type, - Date, - Regionname)

library(corrplot)
corrplot(cor(melHouseNaRemoved), method = 'number')


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
#
