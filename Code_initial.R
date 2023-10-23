library(ggplot2)
library(ggpubr)
library(dplyr)
library(gridExtra)
library(corrplot)
library(reshape2)

#import dataset
dataset <- read.csv("D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/ICS/2023/Report-1/census2003_2023.csv")
head(dataset)

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
Median.age.males_2023 <- as.numeric(Median.age.males_2023)
class(Median.age.males_2023)


Infant.Mortality.Rate.Males_2023 <- as.numeric(Infant.Mortality.Rate.Males_2023)
class(Infant.Mortality.Rate.Males_2023)




#Boxplot for Median.age..males
Median.age.males_2023 <- as.numeric(Median.age.males_2023)
Median.age.males_2023 <- data.frame(Region = dataset[ dataset$Year == 2023, 'Region'],
                            median.age = dataset[ dataset$Year == 2023, 'Median.age..males'])

Median.age.males_2023 <- Median.age.males_2023[order(Median.age.males_2023$Region), ]
plot <- ggplot(Median.age.males_2023, aes(median.age, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Median age in both sexes")


#Boxplot for Infant.Mortality.Rate..Males
Infant.Mortality.Rate.Males_2023 <- as.numeric(Infant.Mortality.Rate.Males_2023)
Infant.Mortality.Rate.Males_2023 <- data.frame(Region = dataset[ dataset$Year == 2023, 'Region'],
                       Mortality = dataset[ dataset$Year == 2023, 'Infant.Mortality.Rate..Males'])

Infant.Mortality.Rate.Males_2023 <- Infant.Mortality.Rate.Males_2023[order(Infant.Mortality.Rate.Males_2023$Region), ]
plot1 <- ggplot(Infant.Mortality.Rate.Males_2023, aes(Mortality, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Infant mortality rate for both sexes")

grid.arrange(plot,plot1, nrow=1,ncol=2)




life.exp.both <- data.frame(Region = dataset[ dataset$Year == 2023, 'Region'],
                            life.exp = dataset[ dataset$Year == 2023, 'Infant.Mortality.Rate..Males'])

life.exp.both <- life.exp.both[order(life.exp.both$Region), ]
plot8 <- ggplot(life.exp.both, aes(life.exp, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Life expectancy at birth in both sexes")

#Boxplot for Under.Age.5.Mortality..Both.Sexes
mor.both <- data.frame(Region = dataset[ dataset$Year == 2023, 'Region'],
                       Mortality = dataset[ dataset$Year == 2023, 'Infant.Mortality.Rate..Females'])

mor.both<- mor.both[order(mor.both$Region), ]
plot7 <- ggplot(mor.both, aes(Mortality, Region, fill=Region)) + geom_boxplot() + coord_flip() + labs(y= "Regions", x= "Under age 5 mortality in both sexes")

grid.arrange(plot8,plot7, nrow=1,ncol=2)














