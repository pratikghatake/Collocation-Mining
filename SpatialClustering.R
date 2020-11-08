library(spatstat)
library(data.table)
library(dplyr) 
library(ggplot2)

#setwd("C:/Users/prati/OneDrive - University Of Houston/Acadmics/Data Mining/Group Project")
df = read.csv("C:\\Users\\KEERTHANA\\Desktop\\Zinj_final.csv")
df[,3]= as.factor(df[,3])
xmin = min(df$Long)
xmax= max(df$Long)
ymin = min(df$Lat)
ymax = max(df$Lat)

#Exploratory Data Analysis of GeoSpatial Data

#Conversion of Data into point pattern object
pointpattern = ppp(df[,5],df[,4], marks=df[,3],c(xmin, xmax), c(ymin, ymax) )
unique(df[,3])
plot(pointpattern)
#contingency table of the marks of all points within a given radius of each data point
M = marktable(pointpattern, R= 0.1)
M[1:10,]



Quadrat = quadratcount(pointpattern, nx= 4, ny=5)
Quadrat
plot(Kest(pointpattern))

plot(envelope(pointpattern,Kest))

#Density Plot for generated point pattern
plot(density(pointpattern))

#Categorical Density plot for generated point pattern
plot(split(pointpattern))


#Auto Collocation 
# 1. Checking whether commercial buildings are collocated, randomly distributed or anti-collocated? 
plot(Kcross(pointpattern, 'commercial_building', 'commercial_building'), main="Auto-Collocation of commercial_building")

# 2. Checking whether collective houses are collocated, randomly distributed or anti-collocated? 
plot(Kcross(pointpattern, 'collective_house', 'collective_house'), main="Auto-Collocation of Collective Houses")


#Bi-Variate Collocation
#Filtering the dataframe for Zone 1 observations for bi-variate collocations
dfbi = filter(df, grepl('Zone_1', Zone))
dfbi[,3]= as.factor(dfbi[,3])
x_min = min(dfbi$Long)
x_max= max(dfbi$Long)
y_min = min(dfbi$Lat)
y_max = max(dfbi$Lat)

#Creating a point pattern in a rectangular bounded box
pointpatternbi = ppp(dfbi[,5],dfbi[,4], marks=dfbi[,3],c(x_min, x_max), c(y_min, y_max) )

# 1. Bi-variate collocation for commercial buildings and light buildings
plot(Kcross(pointpatternbi, 'commercial_building', 'light_building'), main="Bivariate Collocation of  commercial buildings and light buildings")

# 2. Bi-variate collocation for commercial buildings and single houses
plot(Kcross(pointpatternbi, 'commercial_building', 'single_house'), main="Bi-variate collocation for commercial buildings and single houses")

# 3. Bi-variate collocation for commercial buildings and garages
plot(Kcross(pointpatternbi, 'commercial_building', 'garage'), main="Bi-variate collocation for commercial buildings and garages")

# 4. Bi-variate collocation for commercial buildings and schools
plot(Kcross(pointpatternbi, 'commercial_building', 'school'), main="Bi-variate collocation for commercial buildings and schools")

# 5. Bi-variate collocation for collective houses and light buildings
plot(Kcross(pointpatternbi, 'collective_house', 'light_building'), main="Bi-variate collocation for collective houses and light buildings")

# 6. Bi-variate collocation for collective houses and single houses
plot(Kcross(pointpatternbi, 'collective_house', 'single_house'), main="Bi-variate collocation for collective houses and single houses")

# 7. Bi-variate collocation for collective houses and garages
plot(Kcross(pointpatternbi, 'collective_house', 'garage'), main="Bi-variate collocation for collective houses and garages")

# 8. Bi-variate collocation for collective houses and schools
plot(Kcross(pointpatternbi, 'collective_house', 'school'), main="Bi-variate collocation for collective houses and schools")

# 9. Bi-variate collocation for collective houses and commercial buildings
plot(Kcross(pointpatternbi, 'collective_house', 'commercial_building'), main="Bi-variate collocation for collective houses and commercial buildings")

#Plotting zones

#Zone 1 Plot
ggplot(data = df, aes(x = Long, y =Lat, color = Type)) +
  geom_point(cex = 0.8) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Zone 1", x = "Longitude", y = "Latitude" )


zone1 = df %>% filter(df$Zone == "Zone_1")
zone2 = df %>% filter(df$Zone == "Zone_2")
zone3 = df %>% filter(df$Zone == "Zone_3")

plot(zone1$Long, zone1$Lat, col=zone1$Type, main = "Zone 1", xlab = "Longitude", ylab="Latitude", legend("topleft", zone1$Type, fill = zone1$Type))

#Zone 1 Plot
ggplot(data = zone1, aes(x = Long, y = Lat, color = Type)) +
  geom_point(cex = 0.9) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
labs(title = "Zone1", x = "Longitude", y = "Latitude" )

#Zone 2Plot
ggplot(data = zone2, aes(x = Long, y = Lat, color = Type)) +
  geom_point(cex = 0.9) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Zone 2", x = "Longitude", y = "Latitude" )

#Zone 3 Plot
ggplot(data = zone3, aes(x = Long, y = Lat, color = Type)) +
  geom_point(cex = 0.9) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Zone 3", x = "Longitude", y = "Latitude" )

unique(df$Type)


#Shuffle Zone 1
set.seed(123789)

zone1_ran = zone1
# zone1_ran$Lat = sample(zone1$Lat)
# zone1_ran$Long = sample(zone1$Long)
zone1_ran$Type = sample(zone1$Type)


ggplot(data = zone1_ran, aes(x = Long, y = Lat, color = Type)) +
  geom_point(cex = 0.9) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Randomized Zone 3", x = "Longitude", y = "Latitude" )

#Shuffle Zone 2

zone2_ran = zone2
# zone2_ran$Lat = sample(zone2$Lat)
# zone2_ran$Long = sample(zone2$Long)
zone2_ran$Type = sample(zone2$Type)


ggplot(data = zone2_ran, aes(x = Long, y = Lat, color = Type)) +
  geom_point(cex = 0.9) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Zone 3", x = "Longitude", y = "Latitude" )


#Shuffle Zone 3

zone3_ran = zone3
# zone3_ran$Lat = sample(zone3$Lat)
# zone3_ran$Long = sample(zone3$Long)
zone3_ran$Type = sample(zone3$Type) 


ggplot(data = zone3_ran, aes(x = Long, y = Lat, color = Type)) +
  geom_point(cex = 0.9) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Randomized - Zone 3", x = "Longitude", y = "Latitude" )

#Combine complete dataset
random_dataset = rbind(zone1_ran, zone2_ran, zone3_ran)

head(random_dataset)

ggplot(data = random_dataset, aes(x = Long, y = Lat , color = Type)) +
  geom_point(cex = 0.8) + # plots the scatter plot
  theme_minimal() + # changes some of the formatting
  labs(title = "Randomized Data", x = "Longitude", y = "Latitude" )

#Collocation Plots for Randomized Data
pointpattern2 = ppp(random_dataset[,5],random_dataset[,4], marks=random_dataset[,3],c(xmin, xmax), c(ymin, ymax) )
unique(random_dataset[,3])
plot(pointpattern2)
#contingency table of the marks of all points within a given radius of each data point
M2 = marktable(pointpattern2, R= 0.1)
M2[1:10,]



Quadrat2 = quadratcount(pointpattern2, nx= 4, ny=5)
Quadrat2
plot(Kest(pointpattern2))

plot(envelope(pointpattern2,Kest))

#Density Plot for generated point pattern
plot(density(pointpattern2))

#Categorical Density plot for generated point pattern
plot(split(pointpattern2))


#Auto Collocation 
# 1. Checking whether commercial buildings are collocated, randomly distributed or anti-collocated? 
plot(Kcross(pointpattern2, 'commercial_building', 'commercial_building'), main="Randomized Auto-Collocation of commercial_building", xlab = "Neighbouring Radius")

# 2. Checking whether collective houses are collocated, randomly distributed or anti-collocated? 
plot(Kcross(pointpattern2, 'collective_house', 'collective_house'), main="Randomized Auto-Collocation of Collective Houses", xlab = "Neighbouring Radius")

#Bi-Variate Collocation
#Filtering the dataframe for Zone 1 observations for bi-variate collocations
dfbi2 = filter(random_dataset, grepl('Zone_1', Zone))
dfbi2[,3]= as.factor(dfbi2[,3])
x_min = min(dfbi2$Long)
x_max= max(dfbi2$Long)
y_min = min(dfbi2$Lat)
y_max = max(dfbi2$Lat)

#Creating a point pattern in a rectangular bounded box
pointpatternbi2 = ppp(dfbi2[,5],dfbi2[,4], marks=dfbi2[,3],c(x_min, x_max), c(y_min, y_max) )

# 1. Bi-variate collocation for commercial buildings and light buildings
plot(Kcross(pointpatternbi2, 'commercial_building', 'light_building'), main="Randomized Bivariate Collocation of  commercial buildings and light buildings", xlab = "Neighbouring Radius")

# 2. Bi-variate collocation for commercial buildings and single houses
plot(Kcross(pointpatternbi2, 'commercial_building', 'single_house'), main="Randomized Bi-variate collocation for commercial buildings and single houses", xlab = "Neighbouring Radius")

# 3. Bi-variate collocation for commercial buildings and garages
plot(Kcross(pointpatternbi2, 'commercial_building', 'garage'), main="Randomized Bi-variate collocation for commercial buildings and garages", xlab = "Neighbouring Radius")

# 4. Bi-variate collocation for commercial buildings and schools
plot(Kcross(pointpatternbi2, 'commercial_building', 'school'), main="Randomized Bi-variate collocation for commercial buildings and schools", xlab = "Neighbouring Radius")

# 5. Bi-variate collocation for collective houses and light buildings
plot(Kcross(pointpatternbi2, 'collective_house', 'light_building'), main="Randomized Bi-variate collocation for collective houses and light buildings", xlab = "Neighbouring Radius")

# 6. Bi-variate collocation for collective houses and single houses
plot(Kcross(pointpatternbi2, 'collective_house', 'single_house'), main="Randomized Bi-variate collocation for collective houses and single houses", xlab = "Neighbouring Radius")

# 7. Bi-variate collocation for collective houses and garages
plot(Kcross(pointpatternbi2, 'collective_house', 'garage'), main="Randomized Bi-variate collocation for collective houses and garages", xlab = "Neighbouring Radius")

# 8. Bi-variate collocation for collective houses and schools
plot(Kcross(pointpatternbi2, 'collective_house', 'school'), main="Randomized Bi-variate collocation for collective houses and schools", xlab = "Neighbouring Radius")

# 9. Bi-variate collocation for collective houses and commercial buildings
plot(Kcross(pointpatternbi2, 'collective_house', 'commercial_building'), main="Randomized Bi-variate collocation for collective houses and commercial buildings", xlab = "Neighbouring Radius")

