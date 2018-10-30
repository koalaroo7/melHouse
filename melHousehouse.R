# check for outliers
# check residuals
#check t-test if needed
# check normalization

# Houses only


# Histograms on numeric variables

hist(house$Rooms)
summary(house$Rooms)


hist(house$Price)
summary(house$Price)
house %>%
  filter(Price > 3500000) # houses more than 3.5 million

#should I delete a certain range here?


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
# figure out how to move this to the model

#---------

# Bathroom

summary(house$Bathroom)

(br <- ggplot(aes(x = Bathroom), data = house) +
  geom_histogram(bins = 6))

qplot(x = log10(Bathroom + 1 ), data = house, bins = 10) # makes no difference

#using basic R

hist(house$Bathroom)
br1 <- house$Bathroom + 1
br2 <- log10(br1)
hist(br2)

qqnorm(y = house$Price, x = house$Bathroom)

#-----------

# Parking spots

hist(house$Car) #skewed








hist(house$Landsize) #skewed


#----------------------

#plots for normalization testing

qqnorm(y = house$Price, x= house$Distance) # no good

