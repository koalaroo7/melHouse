
# this model set has added the building area and year built, this will affect 
# total amount of variables due to NA values, but appears to make a slightly stronger model


# regression selections and models, 
# stepwise/forwards/backwards/
# Linear/Ridge/Lasso

glimpse(melHouse)


(melExtended <- select(melHouse, - SellerG, - Lat, - Long, 
                           - Method, - Suburb, -Postcode, -Address, 
                           - PropertyCount, - CouncilArea, - Bedroom2, - Date))


melExtended <- na.omit(melExtended)
str(melExtended)
summary(melExtended)

# YearBuilt 2019 does not exist yet, might be a presale, but will be removed

# seeing how man homes claim to be built after 2018

melExtended %>%
  select(YearBuilt) %>%
  filter(YearBuilt > 2018)

melExtended <- melExtended %>%
  filter(YearBuilt <= 2018)

summary(melExtended$YearBuilt)

melExtended %>%
  select(YearBuilt) %>%
  filter(YearBuilt < 1800)

# unsure of anything built in 1196, therefore it is being removed

melExtended <- melExtended %>%
  filter(YearBuilt > 1800)


# Eliminate any home where it is claimed it has more bathrooms than rooms

(melExtended <- melExtended %>%
    filter(Bathroom < Rooms))


# Elinimate any home claiming to have 0 bathrooms or more and 5
melExtended <- melExtended %>%
  filter(Bathroom > 0 & Bathroom <= 5)

# Eliminate any home with 0 Landsize
(melExtended<- melExtended %>%
    filter(Landsize > 0))

# using this link https://www.smh.com.au/business/melbournes-apartment-sizes-face-more-scrutiny-20150414-1mkuj4.html
# setting a lowest value of 40, this 10 square meters lower than 2002 requirements

(melExtended <- melExtended %>%
    filter(Landsize >= 40))


# eliminating any lot over 1500

melExtended <- melExtended %>%
  filter(Landsize < 1500)


# Eliminate any property where building area is > land size

melExtended %>%
  filter(BuildingArea > Landsize)

melExtended <- melExtended %>%
  filter(BuildingArea < Landsize)


#------------

# remove categorical to get numeric only

(melExCorr <- melExtended %>%
   select(- Type, - Month, - Year, - Region))

library(corrplot)
corrplot(cor(melExCorr), method = 'number')


#-----------

#separate into 4 groups, all, house, unit, townhouse (h/u/t) while dropping 'Type'


(mxapartment <- melExtended %>%
   filter(Type == 'u') %>%
   select(- Type))

(mxhouse <- melExtended %>%
    filter(Type == 'h') %>%
    select(- Type))

(mxtownhouse <- melExtended %>%
    filter(Type == 't') %>%
    select( - Type))

#---------------------------

# check models adjustd R-squared prior to normalizing variables

(summary(lm(Price ~., data = mxapartment))) # 0.5691
(summary(lm(Price ~., data = mxtownhouse))) # 0.633
(summary(lm(Price ~., data = mxhouse)))     # 0.6803


#--------------------------

# applying Log10 to variables in need for houses only preparing for regressions

library(caret)
#install.packages('psych')
library(pysch)
library(glmnet)
library(mlbench)



# Histograms on numeric variables

# price
hist(mxhouse$Price)
mxp1 <- log10(mxhouse$Price)
hist(mxp1)

# Rooms
summary(mxhouse$Rooms) #min is 2 no need to add 1
hist(mxhouse$Rooms) 
mxr1 <- log10(mxhouse$Rooms)
hist(mxr1) # no difference

qqnorm(y = mxp1, x = mxr1) # use normal rooms

# Distance

hist(mxhouse$Distance) #skewed

mxd1 <- house$Distance + 1
mxd2 <- log10(mxd1)
hist(mxd2)

qqnorm(y = mxp1, x = mxd2) # use the normalized version here, 

#---------

# Bathroom

summary(house$Bathroom)

# no need to log, but just testing
hist(mxhouse$Bathroom)
mxbr1 <- mxhouse$Bathroom + 1
mxbr2 <- log10(mxbr1)
hist(mxbr2) 

qqnorm(y = p1, x = house$Bathroom) # use original Bathroom version
qqnorm(y = p1, x = br2)

#-----------

# Parking spots
# log will make no difference

hist(house$Car) #skewed
mxc1 <- house$Car + 1
mxc2 <- log10(mxc1)
hist(mxc1) # no difference


# landsize 
# testing log
hist(mxhouse$Landsize) #skewed
summary(house$Landsize)
mxland <- log10(mxhouse$Landsize)
hist(mxland) # no real difference

qqnorm(y = p1, x = mxhouse$Landsize) # use original Bathroom version
qqnorm(y = p1, x = mxland)

# Building Area

hist(mxhouse$BuildingArea)
mxba <- log10(mxhouse$BuildingArea)
hist(mxba) # no real difference

#----------------------

#correlation, scatterplots, histograms
pairs.panels(mxhouse) # prior to setting anything to log10



# pretesting simple regression with log10 variables, just to get an idea of the best fitting model
# before separating into folds fro training and testing


# only price is logged

mxtrial <- lm(log10(Price) ~., data = mxhouse)
summary(mxtrial) 

# Residual standard error: 0.111 on 12571 degrees of freedom
# Multiple R-squared:  0.7715,	Adjusted R-squared:  0.7705
# F-statistic:  760.8 on 27 and 6085 DF,  p-value: < 2.2e-16


# using a log10 on Price and Distance

mxhdata <- mxhouse
mxhdata$Distance <- mxhouse$Distance + 1
mxhdata$Distance <- log10(mxhdata$Distance)

mxtrial2 <- lm(log10(Price) ~., data = mxhdata)
summary(mxtrial2) # improves on above model

# Residual standard error: 0.09798 on 6085 degrees of freedom
# Multiple R-squared:  0.7893,	Adjusted R-squared:  0.7884 
# F-statistic: 844.3 on 27 and 6085 DF,  p-value: < 2.2e-16


#---------

# testing selection methods


# backwards selection
summary(step(mxtrial2, direction = 'backward', trace = 0)) 

# Residual standard error: 0.09798 on 6085 degrees of freedom
# Multiple R-squared:  0.7893,	Adjusted R-squared:  0.7884 
# F-statistic: 844.3 on 27 and 6085 DF,  p-value: < 2.2e-16


# forwards selection
summary(step(mxtrial2, direction = 'forward', trace = 0)) 

# Residual standard error: 0.09798 on 6085 degrees of freedom
# Multiple R-squared:  0.7893,	Adjusted R-squared:  0.7884 
# F-statistic: 844.3 on 27 and 6085 DF,  p-value: < 2.2e-16

# stepwise selection (both)
summary(step(mxtrial2, direction = 'both', trace = 0))

# Residual standard error: 0.09798 on 6085 degrees of freedom
# Multiple R-squared:  0.7893,	Adjusted R-squared:  0.7884 
# F-statistic: 844.3 on 27 and 6085 DF,  p-value: < 2.2e-16

# all methods produce the same results

#--------------------

# log10 to Price

mxhdata3 <- mxhdata2
mxhdata3$Price <- log10(mxhdata3$Price)
glimpse(mxhdata3) # verifying it was logged

pairs.panels(mxhdata3, cex.cor = 2) # with log10 data, cex.cor =2 makes correlation #'s larger


# setting up training and test sets

set.seed(99)

# creating the index for the split to occur
mxIndex <- sample(2, nrow(mxhdata3), replace = T, prob = c(0.7, 0.3))


mxTrain <- mxhdata3[mxIndex == 1, ] # create training set, 70%
mxTest <- mxhdata3[mxIndex == 2, ] # create test set, 30%







# created cross validation, 10 fold
# control parameters

library(caret)

mxControls <- trainControl(method = 'repeatedcv', number = 10,  repeats = 5, verboseIter = T)

# setting up lm model training

set.seed(99)

mxLinTrain <- train(Price ~., mxTrain, method = 'lm', trControl = mxControls)
mxLinTrain$results

# intercept       RMSE      Rsquared     MAE         RMSESD        RsquaredSD    MAESD
# 1      TRUE  0.09726642   0.7904334    0.0735688   0.005429905   0.02041724   0.002877921

mxLinTrain

# 4268 samples
# 10 predictor

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 5 times) 
# Summary of sample sizes: 3842, 3842, 3841, 3839, 3841, 3843, ... 
# Resampling results:
  
#   RMSE        Rsquared   MAE      
# 0.09726642  0.7904334  0.0735688

summary(mxLinTrain)

# Residual standard error: 0.09698 on 4240 degrees of freedom
# Multiple R-squared:  0.7928,	Adjusted R-squared:  0.7915 
# F-statistic: 600.9 on 27 and 4240 DF,  p-value: < 2.2e-16

plot(mxLinTrain$finalModel)

#-----------------------


# setting up Ridge
library(glmnet) 
# By deleting year you should consider reverting Price back to original and add inflation for for 2016/2017

# a trial without Year
set.seed(99)

mxTrainRidge <- mxTrain %>%
  select(-Year)

glimpse(mxTrainRidge)

mxhRidge <- train(Price ~., 
                  mxTrainRidge, 
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0,
                  lambda = seq(0.0001, 1, length = 5)),
                  trControl = mxControls)
warnings()

plot(mxhRidge$finalModel)
# RMSE increases with Lamba, use lowest Lambda

plot(mxhRidge) # RMSE vs Regulariztion parameter
print(mxhRidge)

# lambda    RMSE        Rsquared   MAE       
# 0.000100  0.09850214  0.7863429  0.07453761
# 0.250075  0.12978326  0.7466315  0.10358939
# 0.500050  0.15004883  0.7335365  0.12108138
# 0.750025  0.16252492  0.7267024  0.13166248
# 1.000000  0.17087610  0.7223544  0.13869866


#------------
# done with factors - check for how it handled it

mxhRidgeFactors <- train(Price ~., 
                  mxTrain, 
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = seq(0.0001, 1, length = 5)),
                  trControl = mxControls)

summary(mxhRidgeFactors)

plot(mxhRidgeFactors)
print(mxhRidgeFactors)

# lambda    RMSE        Rsquared   MAE       
# 0.000100  0.09805218  0.7886728  0.07415073
# 0.250075  0.12980069  0.7450930  0.10363447
# 0.500050  0.14995229  0.7306593  0.12105039
# 0.750025  0.16236669  0.7232970  0.13157878
# 1.000000  0.17069360  0.7187126  0.13859195

plot(varImp(mxhRidgeFactors, scale = F)) # plot showing importance of variables
plot(varImp(mxhRidgeFactors, scale = T)) #changes scale to 0-100

#--------

set.seed(99)

mxhLassoFactors <- train(Price ~., 
                         mxTrain, 
                         method = 'glmnet', 
                         tuneGrid = expand.grid(alpha = 1,
                                                lambda = seq(0.0001, 1, length = 5)),
                         trControl = mxControls)


plot(mxhLassoFactors) # lowest lambda is best for lasso as well

plot(varImp(mxhLassoFactors, scale = T)) # importance level of variables

plot(mxhLassoFactors$finalModel, xvar = 'dev', label = T) #6 variable explain 60%


#-------------------


# Elastic net
