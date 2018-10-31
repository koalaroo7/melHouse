

# training and testing for houses

glimpse(house5)

#  converting Price column to log10 prior to creating train/test data
houseLogPrice <- house5
houseLogPrice$Price <- log10(house5$Price)

View(houseLogPrice)

houseData <- houseLogPrice

set.seed(99) # setting seed for consistent results

# 2/3 split on data
trainIndexHouse <- sample(1:nrow(houseData), 0.67*nrow(houseData))

# create training data
trainHouse <- houseData[trainIndexHouse,]

#create testing data
testHouse <- houseData[-trainIndexHouse,]

# testing the training model
lmHouseModel <- lm(Price ~., trainHouse)
summary(lmHouseModel)

# Residual standard error: 0.102 on 8413 degrees of freedom
# Multiple R-squared:  0.7492,	Adjusted R-squared:  0.7485 
# F-statistic:  1005 on 25 and 8413 DF,  p-value: < 2.2e-16


# using the test data for prediction

predictHouse <- predict(lmHouseModel, testHouse)
predictHouse

#comparing actual vs predicted values - will need to revert back to $ figure
(compareHouse <- cbind(actual = testHouse$Price, predictHouse))

# calculating accuracy
mean (apply(compareHouse, 1, min)/apply(compareHouse, 1, max))


#---------------------

# using a ridge regression
