library(ggplot2)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(corrplot)
library(reshape2)

#import dataset
dataset <- read.csv("D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/ICS/2023/Report-1/census2003_2023.csv")
head(dataset)
dataset
nrow(dataset)
ncol(dataset)
summary(dataset)
#................1: Frequency distribution of the variables

#Histogram of Median.age..both.sexes in 2023
Median.age.both_2023 <- dataset$Median.age..both.sexes[dataset$Year == 2023]
class(Median.age.both_2023)
Median.age.both_2023 <- as.numeric(Median.age.both_2023)
class(Median.age.both_2023)
hist(Median.age.both_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,100) , ylim = c(0,35), xlab = "Median age for both sexes in 2023",  breaks = 15 )
mean_value <- mean(Median.age.both_2023, na.rm = T)
print(mean_value)
median_value <- median(Median.age.both_2023, na.rm = T)
print(median_value)
abline(v = mean(Median.age.both_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Median.age.both_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=1.0)


#Histogram of Median.age..females in 2023
Median.age.females_2023 <- dataset$Median.age..females[dataset$Year == 2023]
class(Median.age.females_2023)
Median.age.females_2023 <- as.numeric(Median.age.females_2023)
class(Median.age.females_2023)
hist(Median.age.females_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,100) , ylim = c(0,50), xlab = "Median age for females in 2023",  breaks = 15 )
mean_value1 <- mean(Median.age.females_2023, na.rm = T)
print(mean_value1)
median_value1 <- median(Median.age.females_2023, na.rm = T)
print(median_value1)
abline(v = mean(Median.age.females_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Median.age.females_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=1.0)


#Histogram of Median.age..males in 2023
Median.age.males_2023 <- dataset$Median.age..males[dataset$Year == 2023]
class(Median.age.males_2023)
Median.age.males_2023 <- as.numeric(Median.age.males_2023)
class(Median.age.males_2023)
hist(Median.age.males_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,70) , ylim = c(0,25), xlab = "Median age for males in 2023",  breaks = 15 )
mean_value2 <- mean(Median.age.males_2023, na.rm = T)
print(mean_value2)
median_value2 <- median(Median.age.males_2023, na.rm = T)
print(median_value2)
abline(v = mean(Median.age.males_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Median.age.males_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)


#Histogram of Total.Fertility.Rate in 2023
Total.Fertility.Rate_2023 <- dataset$Total.Fertility.Rate[dataset$Year == 2023]
class(Total.Fertility.Rate_2023)
Total.Fertility.Rate_2023 <- as.numeric(Total.Fertility.Rate_2023)
class(Total.Fertility.Rate_2023)
hist(Total.Fertility.Rate_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,10) , ylim = c(0,120), xlab = "Total fertility rate in 2023",  breaks = 15 )
mean_value3 <- mean(Total.Fertility.Rate_2023, na.rm = T)
print(mean_value3)
median_value3 <- median(Total.Fertility.Rate_2023, na.rm = T)
print(median_value3)
abline(v = mean(Total.Fertility.Rate_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Total.Fertility.Rate_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.8)


#Histogram of Infant.Mortality.Rate..Both.Sexes in 2023
Infant.Mortality.Rate.Both.Sexes_2023 <- dataset$Infant.Mortality.Rate..Both.Sexes[dataset$Year == 2023]
class(Infant.Mortality.Rate.Both.Sexes_2023)
Infant.Mortality.Rate.Both.Sexes_2023 <- as.numeric(Infant.Mortality.Rate.Both.Sexes_2023)
class(Infant.Mortality.Rate.Both.Sexes_2023)
hist(Infant.Mortality.Rate.Both.Sexes_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,110) , ylim = c(0,70), xlab = "Infant mortality rate for both sexes in 2023",  breaks = 15 )
mean_value4 <- mean(Infant.Mortality.Rate.Both.Sexes_2023, na.rm = T)
print(mean_value4)
median_value4 <- median(Infant.Mortality.Rate.Both.Sexes_2023, na.rm = T)
print(median_value4)
abline(v = mean(Infant.Mortality.Rate.Both.Sexes_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Infant.Mortality.Rate.Both.Sexes_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)


#Histogram of Infant.Mortality.Rate..Males in 2023
Infant.Mortality.Rate.Males_2023 <- dataset$Infant.Mortality.Rate..Males[dataset$Year == 2023]
class(Infant.Mortality.Rate.Males_2023)
Infant.Mortality.Rate.Males_2023 <- as.numeric(Infant.Mortality.Rate.Males_2023)
class(Infant.Mortality.Rate.Males_2023)
hist(Infant.Mortality.Rate.Males_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,110) , ylim = c(0,55), xlab = "Total fertility rate in 2023",  breaks = 15 )
mean_value5 <- mean(Infant.Mortality.Rate.Males_2023, na.rm = T)
print(mean_value5)
median_value5 <- median(Infant.Mortality.Rate.Males_2023, na.rm = T)
print(median_value5)
abline(v = mean(Infant.Mortality.Rate.Males_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Infant.Mortality.Rate.Males_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)


#Histogram of Infant.Mortality.Rate..Females in 2023
Infant.Mortality.Rate.Females_2023 <- dataset$Infant.Mortality.Rate..Females[dataset$Year == 2023]
class(Infant.Mortality.Rate.Females_2023)
Infant.Mortality.Rate.Females_2023 <- as.numeric(Infant.Mortality.Rate.Females_2023)
class(Infant.Mortality.Rate.Females_2023)
hist(Infant.Mortality.Rate.Females_2023, col = 'green', main = "", freq = TRUE, xlim =c(0,90) , ylim = c(0,75), xlab = "Total fertility rate in 2023",  breaks = 15 )
mean_value6 <- mean(Infant.Mortality.Rate.Females_2023, na.rm = T)
print(mean_value6)
median_value6 <- median(Infant.Mortality.Rate.Females_2023, na.rm = T)
print(median_value6)
abline(v = mean(Infant.Mortality.Rate.Females_2023, na.rm = T), col="red", lwd=2)
abline(v = median(Infant.Mortality.Rate.Females_2023, na.rm = T), col="blue", lwd=2)
legend(x="topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=1:1, cex=0.9)


#----- 1: Continue: Represented region wise boxplot

# Boxplot for Median.age..both.sexes
Median.age.both_2023 <- dataset$Median.age..both.sexes[dataset$Year == 2023]
class(Median.age.both_2023)
Median.age.both_2023_1 <- as.integer(Median.age.both_2023, na.rm = TRUE)
class(Median.age.both_2023_1)

Median.age.both_2023_11 <- data.frame(
  Region = dataset$Region[dataset$Year == 2023],
  life.exp = Median.age.both_2023_1
)

Median.age.both_2023_11 <- Median.age.both_2023_11[order(Median.age.both_2023_11$Region), ]

plot8 <- ggplot(Median.age.both_2023_11, aes(x = life.exp, y = Region, fill = Region)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Regions", x = "Life expectancy at birth in both sexes (new: Median.age..both.sexes)")

# Boxplot for Infant.Mortality.Rate..Both.Sexes
Infant.Mortality.Rate_2023 <- dataset$Infant.Mortality.Rate..Both.Sexes[dataset$Year == 2023]
class(Infant.Mortality.Rate_2023)
Infant.Mortality.Rate_2023 <- as.integer(Infant.Mortality.Rate_2023, na.rm = TRUE)
class(Infant.Mortality.Rate_2023)

mor.both <- data.frame(
  Region = dataset$Region[dataset$Year == 2023],
  Mortality = Infant.Mortality.Rate_2023
)

mor.both <- mor.both[order(mor.both$Region), ]

plot7 <- ggplot(mor.both, aes(x = Mortality, y = Region, fill = Region)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Regions", x = "Under age 5 mortality in both sexes (new: Infant.Mortality.Rate..Both.Sexes)")

library(gridExtra)
grid.arrange(plot8, plot7, nrow = 1, ncol = 2)
#---------------------------------------------------------------

# Boxplot for Median.age..males
Median.age..males_2023 <- dataset$Median.age..males[dataset$Year == 2023]
class(Median.age..males_2023)
Median.age.males_2023_11 <- as.integer(Median.age..males_2023, na.rm = TRUE)
class(Median.age.males_2023_11)

Median.age.males_2023_11 <- data.frame(
  Region = dataset$Region[dataset$Year == 2023],
  life.exp1 = Median.age.males_2023_11
)

Median.age.males_2023_11 <- Median.age.males_2023_11[order(Median.age.males_2023_11$Region), ]

plot1 <- ggplot(Median.age.males_2023_11, aes(x = life.exp1, y = Region, fill = Region)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Regions", x = "Life expectancy at birth in males (New: Median.age..males)")

# Boxplot for Median.age..females
Median.age..females_2023 <- dataset$Median.age..females[dataset$Year == 2023]
class(Median.age..females_2023)
Median.age.females_2023_11 <- as.integer(Median.age..females_2023, na.rm = TRUE)
class(Median.age.females_2023_11)

mor.both1 <- data.frame(
  Region = dataset$Region[dataset$Year == 2023],
  Mortality = Median.age.females_2023_11
)

mor.both1 <- mor.both1[order(mor.both1$Region), ]

plot2 <- ggplot(mor.both1, aes(x = Mortality, y = Region, fill = Region)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Regions", x = "Life expectancy at birth in females (New:Median.age..females)")

library(gridExtra)
grid.arrange(plot1, plot2, nrow = 1, ncol = 2)
#---------------------------------------------------------------
#--------------------------
#Boxplot for Infant.Mortality.Rate..Males
Infant.Mortality.Rate.Males_2023 <- dataset$Infant.Mortality.Rate..Males[dataset$Year == 2023]
class(Infant.Mortality.Rate.Males_2023)
Infant.Mortality.Rate.Males_2023_1 <- as.integer(Infant.Mortality.Rate.Males_2023, na.rm = TRUE)
class(Infant.Mortality.Rate.Males_2023_1)

Infant.Mortality.Rate.Males_2023_11 <- data.frame(
  Region = dataset$Region[dataset$Year == 2023],
  life.exp1 = Infant.Mortality.Rate.Males_2023_1
)

Infant.Mortality.Rate.Males_2023_11 <- Infant.Mortality.Rate.Males_2023_11[order(Infant.Mortality.Rate.Males_2023_11$Region), ]

plot3 <- ggplot(Infant.Mortality.Rate.Males_2023_11, aes(x = life.exp1, y = Region, fill = Region)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Regions", x = "Under age 5 mortality in males(New:Infant.Mortality.Rate..Males)")

#Boxplot for Infant.Mortality.Rate..Females
Infant.Mortality.Rate..Females_2023 <- dataset$Infant.Mortality.Rate..Females[dataset$Year == 2023]
class(Infant.Mortality.Rate..Females_2023)
Infant.Mortality.Rate..Females_2023_1 <- as.integer(Infant.Mortality.Rate..Females_2023, na.rm = TRUE)
class(Infant.Mortality.Rate..Females_2023_1)

mor.both11 <- data.frame(
  Region = dataset$Region[dataset$Year == 2023],
  Mortality = Infant.Mortality.Rate..Females_2023_1
)

mor.both11 <- mor.both11[order(mor.both11$Region), ]

plot4 <- ggplot(mor.both11, aes(x = Mortality, y = Region, fill = Region)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Regions", x = "Under age 5 mortality in females (New:Infant.Mortality.Rate..Females)")

library(gridExtra)
grid.arrange(plot3, plot4, nrow = 1, ncol = 2)

#.....................................................................................................................
#...................................................................................................................
#................2: Compare the detailed difference between the regions Europe and Africa. 
#Are the values of the individual variables comparatively homogeneous within the individual subregions and heterogeneous between different subregions? 
#To answer this question, first analyse the variability of the values within the individual subregions and then compare the measures of central tendency of the individual variables between different subregions.
#Compare the detailed difference between the regions Europe and Africa.

europe_data <- data[data$Region == 'Europe',]
unique_subregions <- unique(europe_data$Subregion)
print(unique_subregions)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary1 <- aggregate(Median.age..both.sexes ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary1)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary2 <- aggregate(Median.age..females ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary2)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary3 <- aggregate(Median.age..males ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary3)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary4 <- aggregate(Total.Fertility.Rate ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary4)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary5 <- aggregate(Infant.Mortality.Rate..Both.Sexes ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary5)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary6 <- aggregate(Infant.Mortality.Rate..Males ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary6)

europe_subregions <- c('Southern Europe', 'Western Europe', 'Eastern Europe', 'Northern Europe')
europe_data <- data[data$Region == 'Europe' & data$Subregion %in% europe_subregions, ]
mean_median_stddev_minimum_maximum_summary7 <- aggregate(Infant.Mortality.Rate..Females ~ Subregion, data = europe_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary7)



africa_data <- data[data$Region == 'Africa',]
unique_subregions <- unique(africa_data$Subregion)
print(unique_subregions)


africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary1 <- aggregate(Median.age..both.sexes ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary1)

africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary2 <- aggregate(Median.age..females ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary2)

africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary3 <- aggregate(Median.age..males ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary3)

africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary4 <- aggregate(Total.Fertility.Rate ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary4)

africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary5 <- aggregate(Infant.Mortality.Rate..Both.Sexes ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary5)

africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary6 <- aggregate(Infant.Mortality.Rate..Males ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary6)

africa_subregions <- c('Northern Africa', 'Middle Africa',   'Western Africa',  'Southern Africa', 'Eastern Africa')
africa_data <- data[data$Region == 'Africa' & data$Subregion %in% africa_subregions, ]
mean_median_stddev_minimum_maximum_summary7 <- aggregate(Infant.Mortality.Rate..Females ~ Subregion, data = africa_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), stddev = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE)))
print(mean_median_stddev_minimum_maximum_summary7)
#----------------------------------------------------------------------------------------------------------------------------------------





#Boxplot for Median.age..both.sexes
data_mor_1 <- data.frame(Region = data[ data$Year == 2023 & data$Region == 'Europe', 'Region'],
                       Subregion = data[ data$Year == 2023 & data$Region == 'Europe', 'Subregion'],
                       Median.age..both.sexes = data[ data$Year == 2023 & data$Region == 'Europe', 'Median.age..both.sexes'])

data_mor_1 <- data_mor_1[order(data_mor_1$Region, data_mor_1$Subregion), ]
data_mor_1$Subregion <- factor(data_mor_1$Subregion, levels = rev(unique(data_mor_1$Subregion)), ordered = TRUE)
ggplot(data_mor_1, aes(Median.age..both.sexes, Subregion, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Subregions")






#Boxplot for Infant.Mortality.Rate..Both.Sexes
data_exp <- data.frame(Region = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Region'],
                       Subregion = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Subregion'],
                       Life.Expectancy.at.Birth.Both.Sexes = census2002_2022[ census2002_2022$Year == 2022 & census2002_2022$Region == 'Asia', 'Life.Expectancy.at.Birth..Both.Sexes'])

data_exp <- data_exp[order(data_exp$Region, data_exp$Subregion), ]
data_exp$Subregion <- factor(data_exp$Subregion, levels = rev(unique(data_exp$Subregion)), ordered = TRUE)
ggplot(data_exp, aes(Life.Expectancy.at.Birth.Both.Sexes, Subregion, fill=Region)) + geom_boxplot() + coord_flip()+ labs(y= "Subregions")




#............................................................................................................................................
#..................3: Bivariate correlation

# For Integer Data
Median.age.both_2023_num <- as.integer(Median.age.both_2023)
class(Median.age.both_2023_num)
Median.age.females_2023 <- as.integer(Median.age.females_2023)
class(Median.age.females_2023)
Median.age.males_2023 <- as.integer(Median.age.males_2023)
class(Median.age.males_2023)
Infant.Mortality.Rate.Both.Sexes_2023 <- as.integer(Infant.Mortality.Rate.Both.Sexes_2023)
class(Infant.Mortality.Rate.Both.Sexes_2023)
Infant.Mortality.Rate.Males_2023 <- as.integer(Infant.Mortality.Rate.Males_2023)
class(Infant.Mortality.Rate.Males_2023)
Infant.Mortality.Rate.Females_2023 <- as.integer(Infant.Mortality.Rate.Females_2023)
class(Infant.Mortality.Rate.Females_2023)



#install.packages("corrplot")
library(corrplot)
# Create a data frame with your variables
correlation_data <- data.frame(
  Median.Both = Median.age.both_2023_num,
  Median.Females = Median.age.females_2023,
  Median.Males = Median.age.males_2023,
  Infant.Both = Infant.Mortality.Rate.Both.Sexes_2023,
  Infant.Males = Infant.Mortality.Rate.Males_2023,
  Infant.Females = Infant.Mortality.Rate.Females_2023
)

# Calculate the Pearson correlation matrix
cor_matrix <- cor(correlation_data, use = "complete.obs")
# Create the Pearson correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         tl.cex = 0.8, 
         title = "Pearson Correlation Heatmap")


# Load the corrplot library if not already loaded
library(corrplot)

# Create a data frame with your variables
correlation_data <- data.frame(
  Median.Both = Median.age.both_2023_num,
  Median.Females = Median.age.females_2023,
  Median.Males = Median.age.males_2023,
  Infant.Both = Infant.Mortality.Rate.Both.Sexes_2023,
  Infant.Males = Infant.Mortality.Rate.Males_2023,
  Infant.Females = Infant.Mortality.Rate.Females_2023
)

# Calculate the Spearman correlation matrix
cor_matrix_spearman <- cor(correlation_data, method = "spearman", use = "complete.obs")

# Create the Spearman correlation heatmap
corrplot(cor_matrix_spearman, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black",
         tl.cex = 0.8,
         title = "Spearman Correlation Heatmap")

######################################################################################
data <- read.csv("D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/ICS/2023/Report-1/census2003_2023.csv", header = TRUE)
columns_to_convert <- c("Median.age..both.sexes", "Median.age..females", "Median.age..males", "Total.Fertility.Rate", "Infant.Mortality.Rate..Both.Sexes", "Infant.Mortality.Rate..Males", "Infant.Mortality.Rate..Females")
data[columns_to_convert] <- lapply(data[columns_to_convert], as.integer)
write.csv(data, "D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/ICS/2023/Report-1/census2003_2023_1.csv", row.names = FALSE)

#.........4: comparing 2003 with 2023
Country <- data$Country
Subregion <- data$Subregion

#Median.age..both.sexes 2003 vs 2023
data_fert <- dcast(data, Country + Subregion  + Region ~ factor(Year), value.var="Median.age..both.sexes")
data_fert <- data_fert[order(data_fert$Region, data_fert$Subregion), ]
data_fert$Subregion <- factor(data_fert$Subregion, levels = rev(unique(data_fert$Subregion)), ordered = TRUE)
ggplot(data=data_fert, mapping = aes(x=`2003`, y=`2023`)) +
  geom_point() + facet_wrap(. ~ Subregion) + 
  geom_abline()


#Infant.Mortality.Rate..Both.Sexes 2003 vs 2023
data_life_exp <- dcast(data, Country + Subregion + Region ~ factor(Year), value.var="Infant.Mortality.Rate..Both.Sexes")
data_life_exp <- data_life_exp[order(data_life_exp$Region, data_life_exp$Subregion), ]
data_life_exp$Subregion <- factor(data_life_exp$Subregion, levels = rev(unique(data_life_exp$Subregion)), ordered = TRUE)
ggplot(data=data_life_exp, mapping = aes(x=`2003`,y=`2023`)) +
  geom_point() + facet_wrap(. ~ Subregion) + 
  geom_abline()
