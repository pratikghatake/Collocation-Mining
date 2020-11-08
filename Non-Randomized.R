library(spatstat)
library(data.table)
library(dplyr) 

setwd("C:/Users/prati/OneDrive - University Of Houston/Acadmics/Data Mining/Group Project")
df = read.csv("Zinj_final.csv")

df[,3]= as.factor(df[,3])
xmin = min(df$Long)
xmax= max(df$Long)
ymin = min(df$Lat)
ymax = max(df$Lat)

#Exploratory Data Analysis of GeoSpatial Data

#Conversion of Data into point pattern object
pointpattern = ppp(df[,4],df[,5], marks=df[,3],c(xmin, xmax), c(ymin, ymax) )
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
plot(Kcross(pointpattern, 'commercial_building', 'commercial_building'))

# 2. Checking whether collective houses are collocated, randomly distributed or anti-collocated? 
plot(Kcross(pointpattern, 'collective_house', 'collective_house'))


#Bi-Variate Collocation
#Filtering the dataframe for Zone 1 observations for bi0-variate collocations
dfbi = filter(df, grepl('Zone_1', Zone))
dfbi[,3]= as.factor(dfbi[,3])
x_min = min(dfbi$Long)
x_max= max(dfbi$Long)
y_min = min(dfbi$Lat)
y_max = max(dfbi$Lat)

#Creating a point pattern in a rectangular bounded box
pointpatternbi = ppp(dfbi[,4],dfbi[,5], marks=dfbi[,3],c(x_min, x_max), c(y_min, y_max) )

# 1. Bi-variate collocation for commercial buildings and light buildings
plot(Kcross(pointpatternbi, 'commercial_building', 'light_building'))

# 2. Bi-variate collocation for commercial buildings and single houses
plot(Kcross(pointpatternbi, 'commercial_building', 'single_house'))

# 3. Bi-variate collocation for commercial buildings and garages
plot(Kcross(pointpatternbi, 'commercial_building', 'garage'))

# 4. Bi-variate collocation for commercial buildings and schools
plot(Kcross(pointpatternbi, 'commercial_building', 'school'))

# 5. Bi-variate collocation for collective houses and light buildings
plot(Kcross(pointpatternbi, 'collective_house', 'light_building'))

# 6. Bi-variate collocation for collective houses and single houses
plot(Kcross(pointpatternbi, 'collective_house', 'single_house'))

# 7. Bi-variate collocation for collective houses and garages
plot(Kcross(pointpatternbi, 'collective_house', 'garage'))

# 8. Bi-variate collocation for collective houses and schools
plot(Kcross(pointpatternbi, 'collective_house', 'school'))

# 9. Bi-variate collocation for collective houses and commercial buildings
plot(Kcross(pointpatternbi, 'collective_house', 'commercial_building'))
