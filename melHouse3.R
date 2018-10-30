
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

# getting a count of how many rows contain the same values
melHouse %>%
  select(Rooms, Bedroom2) %>%
  count(Rooms == Bedroom2)

# checking the correlation
cor.test(melHouse$Rooms, melHouse$Bedroom2)

# Drop Bedroom2 due to multicolinearity issues

#--------

# Dropping columns that are not needed due to many NA values

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

#checking summary to look for unusual features
summary(melHouseNaRemoved)


# Eliminate any home where it is claimed it has more bathrooms than rooms

(melClean <- melHouseNaRemoved %>%
  filter(Bathroom < Rooms))

melClean %>%
  filter(Bathroom > Rooms) # confirming the above code worked

# inspect bathrooms > 5
melClean %>%
  filter(Bathroom > 5)

# Elinimate any home claiming to have 0 bathrooms or more and 5
melClean1 <- melClean %>%
  filter(Bathroom > 0 & Bathroom <= 5)
  
summary(melClean1) # verifying the code worked

# Eliminate any home with 0 Landsize
(melClean2 <- melClean1 %>%
    filter(Landsize > 0))

summary(melClean2) # inspecting corrections have been fixed and landsize issues

# take a look at the lowest values of landsize
melClean2 %>%
  select(Landsize, Type) %>%
  arrange(Landsize)

# using this link https://www.smh.com.au/business/melbournes-apartment-sizes-face-more-scrutiny-20150414-1mkuj4.html
# setting a lowest value of 40, this 10 square meters lower than 2002 requirements

(melClean3 <- melClean2 %>%
    filter(Landsize >= 40))

summary(melClean3)

# inspect largest property sizes

melClean3 %>%
  select(Landsize, Car, Price) %>%
  arrange(desc(Landsize))

# using 1500 as a max

melClean3 %>%
  select(Landsize, Price) %>%
  filter(Landsize > 1500)

# eliminating any lot under 1500

melClean4 <- melClean3 %>%
  filter(Landsize < 1500)

summary(melClean4)



#------------

# remove categorical to get numeric only

(melHouseCorr <- melClean4 %>%
  select(- Type, - Month, - Year, - Region))

library(corrplot)
corrplot(cor(melHouseCorr), method = 'number')


#-----------

#separate into 4 groups, all, house, unit, townhouse (h/u/t) while dropping 'Type'


(apartment <- melClean4 %>%
   filter(Type == 'u') %>%
   select(- Type))

(house <- melClean4 %>%
    filter(Type == 'h') %>%
    select(- Type))

(townhouse <- melClean4 %>%
    filter(Type == 't') %>%
    select( - Type))

#---------------------------

# check models adjustd R-squared prior to checking normalization of variables

(summary(lm(Price ~., data = apartment))) # 0.5362
(summary(lm(Price ~., data = townhouse))) # 0.5861
(summary(lm(Price ~., data = house)))     # 0.6134



# bar charts for categorical variables even if being deleted
# scatterplots for descriptive
# boxplots for outliers
# determine what to do for outliers
# test for normality
# separate months and years into quarters
# find inflation rates and add them retroactively to years
