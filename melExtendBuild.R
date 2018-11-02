
# this model set has added the building area and year built, this will affect 
# total amount of variables due to NA values, but appears to make a slightly stronger model


# regression selections and models, 
# stepwise/forwards/backwards/
# Linear/Ridge/Lasso

glimpse(melHouse)

#year built needs to be split up in to section of 50 year time frames
# and made into a factor - in melExtended it is nominal/categorical


(melExtended <- select(melHouse, - SellerG, - Lat, - Long, 
                           - Method, - Suburb, -Postcode, -Address, 
                           - PropertyCount, - CouncilArea, - Bedroom2, - Date))


melExtended <- na.omit(melExtended)
str(melExtended)
summary(melExtended)


# possibly change this after you order in to time periods
# melExtended$YearBuilt <- as.factor(melExtended$YearBuilt) 
# glimpse(melExtended)


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


# Turning YearBuilt into a range of factors
# 1 = 1800 -1899
# 2 = 1900 - 1949
# 3 = 1950 - 1999
# 4 = 1999 - 2018


# adding column BuildYear for range of years built
melExtended$BuildYear  = 0

melExtended$BuildYear[melExtended$YearBuilt <= 1899] = 1
melExtended$BuildYear[melChang$YearBuilt >= 1900 & melExtended$YearBuilt<= 1949] = 2
melExtended$BuildYear[melExtended$YearBuilt >= 1950 & melExtended$YearBuilt<= 1999] = 3
melExtended$BuildYear[melExtended$YearBuilt >= 2000 & melExtended$YearBuilt<= 2018] = 4

View(melExtended)

# turn BuildYear into a factor
melExtended$BuildYear <- as.factor(melExtended$BuildYear)

# drop year built

melExtended <- melExtended %>%
  select(- YearBuilt)

glimpse(melExtended)

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
   select(- Type, - Month, - Year, - Region, - YearBuilt))

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

# check models adjusted R-squared prior to normalizing variables
set.seed(99)

(summary(lm(Price ~., data = mxapartment))) # 0.5723
(summary(lm(Price ~., data = mxtownhouse))) # 0.6362
(summary(lm(Price ~., data = mxhouse)))     # 0.685


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
hist(mxc2) # no difference


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
View(mxhouse)
mxtrial <- lm(log10(Price) ~., data = mxhouse)
summary(mxtrial) 

# Residual standard error: 0.1007 on 6082 degrees of freedom
# Multiple R-squared:  0.7776,	Adjusted R-squared:  0.7765 
# F-statistic: 708.8 on 30 and 6082 DF,  p-value: < 2.2e-16

# using a log10 on Price and Distance

mxhdata <- mxhouse
mxhdata$Distance <- mxhouse$Distance + 1
mxhdata$Distance <- log10(mxhdata$Distance)

mxtrial2 <- lm(log10(Price) ~., data = mxhdata)
summary(mxtrial2) # improves on above model

# Residual standard error: 0.09661 on 6082 degrees of freedom
# Multiple R-squared:  0.7953,	Adjusted R-squared:  0.7943 
# F-statistic: 787.5 on 30 and 6082 DF,  p-value: < 2.2e-16


#---------

# testing selection methods


# backwards selection
summary(step(mxtrial2, direction = 'backward', trace = 0)) 

# Residual standard error: 0.09661 on 6082 degrees of freedom
# Multiple R-squared:  0.7953,	Adjusted R-squared:  0.7943 
# F-statistic: 787.5 on 30 and 6082 DF,  p-value: < 2.2e-16


# forwards selection
summary(step(mxtrial2, direction = 'forward', trace = 0)) 

# Residual standard error: 0.09661 on 6082 degrees of freedom
# Multiple R-squared:  0.7953,	Adjusted R-squared:  0.7943 
# F-statistic: 787.5 on 30 and 6082 DF,  p-value: < 2.2e-16


# stepwise selection (both)
summary(step(mxtrial2, direction = 'both', trace = 0))

# Residual standard error: 0.09661 on 6082 degrees of freedom
# Multiple R-squared:  0.7953,	Adjusted R-squared:  0.7943 
# F-statistic: 787.5 on 30 and 6082 DF,  p-value: < 2.2e-16

# all methods produce the same results

#--------------------

# log10 to Price

mxhdata2 <- mxhdata
mxhdata2$Price <- log10(mxhdata2$Price)
glimpse(mxhdata2) # verifying it was logged
View(mxhdata2)


pairs.panels(mxhdata2, cex.cor = 2) # with log10 data, cex.cor =2 makes correlation #'s larger


# setting up training and test sets

set.seed(99)

# creating the index for the split to occur
mxIndex <- sample(2, nrow(mxhdata2), replace = T, prob = c(0.7, 0.3))


mxTrain <- mxhdata2[mxIndex == 1, ] # create training set, 70%
mxTest <- mxhdata2[mxIndex == 2, ] # create test set, 30%



# created cross validation, 10 fold
# control parameters

library(caret)

mxControls <- trainControl(method = 'repeatedcv', number = 10,  repeats = 5, verboseIter = T)

# setting up lm model training

set.seed(99)

mxLinTrain <- train(Price ~., mxTrain, method = 'lm', trControl = mxControls)
mxLinTrain$results
summary(mxLinTrain) # change as data changed for all

#  intercept    RMSE       Rsquared    MAE         RMSESD      RsquaredSD  MAESD
#  TRUE        0.09617014  0.7991227   0.07397352  0.00460806  0.01880923  0.003030378


mxLinTrain # prints out number of samples, resampling, summary of sample sizes, predictors, RMSE, R-Squared, MAE,

summary(mxLinTrain)

# Residual standard error: 0.09578 on 4258 degrees of freedom
# Multiple R-squared:  0.8018,	Adjusted R-squared:  0.8004 
# F-statistic: 574.2 on 30 and 4258 DF,  p-value: < 2.2e-16

plot(mxLinTrain$finalModel)

#-----------------------


# setting up Ridge
library(glmnet) 
# By deleting year you should consider reverting Price back to original and add inflation for for 2016/2017


mxhRidge <- train(Price ~., 
                  mxTrain, 
                  method = 'glmnet', 
                  tuneGrid = expand.grid(alpha = 0,
                                         lambda = seq(0.0001, 1, length = 5)),
                  trControl = mxControls)

plot(mxhRidge) # graph showing hte worsening of RMSE as Lambda increases
print(mxhRidge)

# lambda    RMSE        Rsquared   MAE       
# 0.000100  0.09679433  0.7974994  0.07459114
# 0.250075  0.12676638  0.7523354  0.10152798
# 0.500050  0.14654909  0.7361460  0.11858982
# 0.750025  0.15925384  0.7277271  0.12947566
# 1.000000  0.16798374  0.7225149  0.13689926

plot(varImp(mxhRidge, scale = F)) # plot showing importance of variables
plot(varImp(mxhRidge, scale = T)) #changes scale to 0-100

#--------

set.seed(99)

mxhLasso <- train(Price ~., 
                         mxTrain, 
                         method = 'glmnet', 
                         tuneGrid = expand.grid(alpha = 1,
                                                lambda = seq(0.0001, 1, length = 5)),
                         trControl = mxControls)


plot(mxhLasso) # lowest lambda is best for lasso as well

plot(varImp(mxhLasso, scale = T)) # importance level of variables

plot(mxhLasso$finalModel, xvar = 'dev', label = T) # 7 variables explain 60%


#-------------------


# Elastic net

mxhElastic <- train(Price ~., 
                         mxTrain, 
                         method = 'glmnet', 
                         tuneGrid = expand.grid(alpha = seq(0, 1, length =10),
                                                lambda = seq(0.0001, 1, length = 5)),
                         trControl = mxControls)

# optimal results for alpha and lambda
# Fitting alpha = 1.1, lambda = 1e-04 on full training set

plot(mxhElastic) #lambda is coloured lines, alpha along bottom, run again with length 3 for alpha for clarity
plot(varImp(mxhElastic))
varImp(mxhElastic)

mxhElastic$bestTune #finding optimal lambda & alpha values in a separate way
# alpha lambda
#  1.1  1e-04


# Comparing models

mxhModelList <- list(Linear = mxLinTrain, Ridge = mxhRidge, Lasso = mxhLasso, Elastic = mxhElastic)
mxhResample <- resamples(mxhModelList)
summary(mxhResample)


# Models: Linear, Ridge, Lasso, Elastic 
# Number of resamples: 50 

# MAE 
#          Min.       1st Qu.    Median   Mean      3rd Qu.       Max.       NA's
# Linear  0.06679373 0.07210747 0.07429309 0.07397352 0.07535753 0.08110961    0
# Ridge   0.06903858 0.07259716 0.07431354 0.07459114 0.07739205 0.07924662    0
# Lasso   0.06665498 0.07192616 0.07384257 0.07398507 0.07679026 0.08045237    0
# Elastic 0.06743638 0.07239888 0.07363186 0.07393242 0.07594637 0.08001653    0

# RMSE 
#           Min.        1st Qu.  Median       Mean       3rd Qu.   Max.      NA's
# Linear  0.08634323 0.09270655 0.09591327 0.09617014 0.09883494 0.1060284    0
# Ridge   0.08881766 0.09412620 0.09646447 0.09679433 0.09885603 0.1081108    0
# Lasso   0.08541115 0.09193823 0.09648567 0.09610910 0.09953557 0.1104360    0
# Elastic 0.08585379 0.09267748 0.09577662 0.09607029 0.09964271 0.1096180    0

# Rsquared 
#         Min.       1st Qu.    Median      Mean   3rd Qu.      Max.    NA's
# Linear  0.7643388 0.7881606 0.8006874 0.7991227 0.8120610 0.8360959    0
# Ridge   0.7481465 0.7863993 0.8019331 0.7974994 0.8084620 0.8332299    0
# Lasso   0.7487436 0.7861348 0.7983222 0.7992509 0.8162633 0.8466945    0
# Elastic 0.7431360 0.7870479 0.7992595 0.7996723 0.8128197 0.8509193    0



bwplot(mxhResample) # compaing models by boxplots, can be hard to see if too small
xyplot(mxhResample, metric = 'RMSE') #scatter plot between models Ridge/Linear
# dots below line are better for linear


#-------------

# Predictions with Training & Test data
# all models performed closely however the Ridge regression has best mean RMSE


# training data prediction & RMSE
mxhP1 <- predict(mxhRidge, mxTrain)
sqrt(mean((mxTrain$Price - mxhP1)^2)) # RMSE - lower the better
# 0.09620596

# Test data prediction RMSE
mxhP2 <- predict(mxhRidge, mxTest)
sqrt(mean((mxTest$Price - mxhP2)^2))
# 0.09963429


#---------------

# comparing accuracy

#comparing actual vs predicted values - will need to revert back to $ figure
(mxhCompare <- cbind(actual = mxTest$Price, mxhP2)) # combining actual and predicted

# calculating accuracy
mean (apply(mxhCompare, 1, min)/apply(mxhCompare, 1, max))

# 0.9880479


#-----------


#add explanation of how log variable word

# (Distance1/ Distance2) ^ Distance Coefficient - (-5.844e-01)


#--------------

# attempting Regression Decision Tree

library(rpart)

mxhTree <- rpart(Price ~., data = mxTrain, method = "anova")
summary(mxhTree)
print(mxhTree)
plot(mxhTree)

# done on training data
mxTreeP1 <- predict(mxhTree, mxTrain )
mxTreeP1
RMSE(mxTreeP1, mxTrain$Price)
# 0.120991

# done on test data

mxTreeP2 <- predict(mxhTree, mxTest )
mxTreeP2
RMSE(mxTreeP2, mxTest$Price)
# 0.1241534


#--------------

# Using random forest

# install.packages('randomForest')
library(randomForest)

MxhForest <- randomForest(Price ~., data = mxTrain, method = "anova",
                      ntree = 500,
                      mtry = 10,
                      replace = F,
                      nodesize = 5,
                      importance = T)

mxhForestP1 <- predict(MxhForest, mxTrain)

plot(MxhForest)
MxhForest$forest

(RMSE(mxhForestP1, mxTrain$Price))
# 0.03305815 - with mtry on default
# 0.04065278 - with mtry 10


