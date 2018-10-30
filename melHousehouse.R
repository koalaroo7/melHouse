# check for outliers
# check residuals
#check t-test if needed
# check normalization

# Houses only - log10 will be used for normalization, the value of 1 will be added to avoid 0 in needed cases
# Separated into individual variables a for graph's, normalization, etc.
#------------------


hist(house$Price)
p1 <- log10(house$Price)

hist(p1)


# Histograms on numeric variables

summary(house$Rooms) #min is 2 no need to add 1
hist(house$Rooms) 
h1 <- log10(house$Rooms)
hist(h1) # no difference

qqnorm(y = p1, x = house$Rooms ) # use normal rooms


#--------------
# normalizing distance with log base 10

hist(house$Distance) #skewed

(dist <- ggplot(aes(x = Distance), data = house) +
  geom_histogram()) # add 1 to distance here if you want to use the direct code below

dist + scale_x_log10() # prints out distribution - do not use here as ou need to add 1 to avoid 0 after log10
dist2
summary(dist2)
glimpse(dist2)

#alternatively 
qplot(x = log10(Distance + 1), data = house, binwidth = 0.1)
# the above stops anything from being 0 by adding 1 to any value preventing negatives after the log function


#alternative way to get done
d1 <- house$Distance + 1
d2 <- log10(d1)
hist(d2)

qqnorm(y = p1, x = d2) # use the normalized version here, 
qqnorm(y = p1, x = house$Distance) # not much of a difference to above

#---------

# Bathroom

summary(house$Bathroom)

(br <- ggplot(aes(x = Bathroom), data = house) +
  geom_histogram(bins = 6))

qplot(x = log10(Bathroom + 1 ), data = house, bins = 10) # makes no difference

# using basic R

hist(house$Bathroom)
br1 <- house$Bathroom + 1
br2 <- log10(br1)
hist(br2) 

qqnorm(y = p1, x = house$Bathroom) # use original Bathroom version
qqnorm(y = p1, x = br2)

#-----------

# Parking spots

hist(house$Car) #skewed
c1 <- house$Car + 1
c2 <- log10(c1)
hist(c1) # no difference


#----------------

# landsize 

hist(house$Landsize) #skewed
summary(house$Landsize)
land <- log10(house$Landsize)
hist(land)

qqnorm(y = p1, x = house$Landsize) # use original Bathroom version
qqnorm(y = p1, x = land)

#----------------------

# pretesting simple regression with log10 variables, just to get an idea of the best fitting model
# before separating into folds fro training and testing

qqnorm(y = h1, x= d2) 

trial <- lm(log10(Price) ~., data = house)
summary(trial) 

# Residual standard error: 0.111 on 12571 degrees of freedom
# Multiple R-squared:  0.706,	Adjusted R-squared:  0.7055 
# F-statistic:  1208 on 25 and 12571 DF,  p-value: < 2.2e-16


# using a log10 on Price and Distance

house2 <- house
house2$Distance <- house$Distance + 1
house2a <- house2
house2a$Distance <- log10(house2$Distance)

trial2 <- lm(log10(Price) ~., data = house2a)
summary(trial2) # improves on above model

# Residual standard error: 0.1045 on 12571 degrees of freedom
# Multiple R-squared:  0.7395,	Adjusted R-squared:  0.739 
# F-statistic:  1428 on 25 and 12571 DF,  p-value: < 2.2e-16


# log10 Price & Distance, month eliminated

(house3 <- house2a %>%
  select(-Month))

trial3 <- lm(log10(Price) ~., data = house3)
summary(trial3) #minimally worse than above, therefore keep month
 
# Residual standard error: 0.1048 on 12582 degrees of freedom
# Multiple R-squared:  0.7381,	Adjusted R-squared:  0.7378 
# F-statistic:  2532 on 14 and 12582 DF,  p-value: < 2.2e-16


# creating a max house price, using house2a data

house4 <- house2a %>%
  filter(Price < 3500000) # houses more than 3.5 million
# considering deletion above 3.5 million

trial4 <- lm(log10(Price) ~., data = house4)
summary(trial4) # makes model slightly worse

# Residual standard error: 0.1023 on 12435 degrees of freedom
# Multiple R-squared:  0.7272,	Adjusted R-squared:  0.7266 
# F-statistic:  1326 on 25 and 12435 DF,  p-value: < 2.2e-16



# using log10 on price, distance, landsize
house5 <- house2a
house5$Landsize <- log10(house5$Landsize)
trial5 <- lm(log10(Price) ~., data = house5)
summary(trial5) # best model yet

# Residual standard error: 0.1037 on 12571 degrees of freedom
# Multiple R-squared:  0.7437,	Adjusted R-squared:  0.7432 
# F-statistic:  1459 on 25 and 12571 DF,  p-value: < 2.2e-16
