# Variable Types

# Rooms - Discrete, restricted by size
# Type - Categorical, nominal
# Price - Continuous
# Distance - Continuous
# Bathroom - Discrete, restriced by size
# Car - Discrete, restricted by size
# Landsize - Continuous
# Region - Categorical, nominal
# Month - categorical, ordinal
# Year - Categorical, ordinal

#---------

# messing around with charts

# charts etc.

glimpse(melHouseNaRemoved)
# categorical barcharts

# Barchart for Type of unit sold
ggplot(data = melHouseNaRemoved) + 
  geom_bar(mapping = aes(x = Type, fill = Type)) 

#-------
# Bar chart for Month sold
ggplot(data = melHouseNaRemoved) + 
  geom_bar(mapping = aes(x = Month, fill = Month)) 

# Barchart for month sold separared by Type of unit sold
ggplot(data = melHouseNaRemoved) + 
  geom_bar(mapping = aes(x = Month, fill = Type), position = 'dodge')

#---------
# Barchar for year sold
ggplot(data = melHouseNaRemoved) + 
  geom_bar(mapping = aes(x = Year, fill = Year)) 

#BarChart for Year sold separated by Type
ggplot(data = melHouseNaRemoved) + 
  geom_bar(mapping = aes(x = Year, fill = Type), position = "dodge") # Year sold

#----------
# Barchart for Region
ggplot(data = melHouseNaRemoved) +
  geom_bar(aes(x = Region, fill = Region))

# Barchart for Region by Type
ggplot(data = melHouseNaRemoved) +
  geom_bar(aes(x = Region, fill = Type), position = 'dodge')

#----------

# Boxplots

boxplot(melHouseNaRemoved$Rooms)
boxplot(melHouseNaRemoved$Price)


# boxplot on 2 variables below
ggplot(data = melHouseNaRemoved, mapping = aes(x = Type, y = Price)) + 
  geom_boxplot() +
  coord_flip()



# ---------

# scatterplots

ggplot(data = melHouseNaRemoved, aes(x = Distance, y = Price)) + 
  geom_point(position = 'jitter') +
  geom_smooth(method="lm", se=F) 


ggplot(data = melHouseNaRemoved) + 
  geom_point(aes(y = Region, x = Price, color = Type), position = 'jitter')
#--------

# frequency plots

ggplot(data = melHouseNaRemoved, mapping = aes(x =Price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Type), binwidth = 100000)
