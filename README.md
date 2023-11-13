# Descriptive analysis of demographic data using the U.S. Census Bureau dataset
The International Data Base (IDB) offers demographic data for over 200 countries and regions from the worldwide, encompassing populations of 5,000 or more. The United States Census Bureau manage the demographic and health aspects, including total population size, population distribution by age and sex, fertility rate, mortality rate, and the migration patterns in the year 2023.

# Project Objective
The main goal is to evaluate the whole dataset using a variety of statistical and graphical tools. First, we use plotting histograms to look at how the frequencies of the variables are spread out. The second step is to utilize a boxplot with the mean, median, and interquartile range to calculate the variability of the sub-region that illustrates homogeneity and heterogeneity. Moreover, in this section also detailed comparisons between the regions of Europe and Africa are shown. Next, the heatmap is used to find the variables relationship with each other. Finally, changes that have occurred in the last 20 years, along with the median age and infant mortality rate for both sexes were examined.

# Dataset and Data Quality
The provided dataset represents a limited portion of the International Data Base (IDB) maintained by the United States Census Bureau. It includes the overall median age along with infant mortality rate for both sexes, male and female for 227 countries and countries are divided into five regions i.e. Asia, Africa, Europe, Oceania, and Americas, and 21 subregions between 2003 and 2023. The sample size of the dataset is 453 observations along with 11 variables. The variables of the datasets are ‘country name’, ‘region’, ‘subregion’, ‘year’, ‘median age for both sexes’, ‘median age for males’, ‘median age for females’, ‘total fertility rate’, ‘infant mortality rate for both sexes’, ‘infant mortality rate for males’, and ‘infant mortality rate for females’. From those variables, ‘median age for both sexes’, ‘median age for males’, ‘median age for females’, ‘total fertility rate’, ‘infant mortality rate for both sexes’, ‘infant mortality rate for males’, and ‘infant mortality rate for females’ are numerical (continuous) variables. Moreover, the variable of ‘country name’, ‘year’, ‘region’, and ‘subregion’ are categorical (nominal) variables.

From the year 2003 and 2023, countries are divided into the five regions and twenty-one subregions. While looking through the whole dataset, we find out that there are seven rows that have missing values. Six of them are from 2003, and the last one is from 2023. Missing values are positioned in rows number 118, 163, 190, 193, 197, 215, and 441. Only ‘country name’, ‘region’, ‘subregion’, and ‘year’ values exist in the first six rows, and the remaining variable values are missing. Furthermore, in row number 441, only ‘median age for both sexes’, ‘median age females’, and ‘median age males’ that three variables values are missing. Since there are only a few missing records in the whole dataset, they are not used for analysis.

# Statistical Methods
In this section, several statistical methods are represented. The information was analyzed with R, version 4.1.1, which was made by the R Development Core Team. In addition, R packages, including ggplot2, gridExtra, reshape2, and ggpubr are used for data analysis, calculations, and visualizations.

1. Mean, 2. Median, 3. Pearson Correlation Coefficient, 4. Variance and Standard Deviation, 5. Histogram, 6. Scatterplot, 7. Boxplot

# Statistical Analysis
In this section, all seven variables such as ‘median age for both sexes’, ‘median age for males’, ‘median age for females’, ‘total fertility rate’, ‘infant mortality rate for both sexes’, ‘infant mortality rate for males’, and ‘infant mortality rate for females’ are applied for the year 2023. In the provided Excel dataset, those variables exist in the character mode. For the purpose of data analysis, those variables should be converted from character mode to numeric mode. Finally, in the last subsection, a comparison between the year 2003 and 2023 is shown.  

Frequency Distribution Analysis:
In this subsection, we represent our data analysis for the above-mentioned seven variables using histograms and boxplots. First of all, we observe the frequency distributions of the median age for both sexes, males, and females and their results. From the Table 1, we observe that the mean value of both sexes, males, and females are 32.43, 31.60, and 33.19 respectively where the median age for females contains the higher value. On the other hand, if we observe the other three important variables outcomes (infant mortality for both sexes, males and females), then the infant mortality rate for males holds the highest position in the mean. For the total fertility rate mean result is 2.38. 

# Summary 
In conclusion, the main objective of this paper was to analyze data on median age and mortality rate from the given dataset. This dataset contained 227 countries, which were separated into five regions and 21 subregions between 2003 and 2023. This dataset had a total of 453 observations along with 11 variables. The data utilized in this study was obtained from the International Data Base (IDB) of the United States Census Bureau. The frequency distributions of the variables and the difference between the sexes and regions, the detailed difference between the regions Europe and Africa along with the dependency and variability between the variables for both regions, the relationship between the variables of median age and infant mortality rate, and how the values of the variables changed over the last 20 years (2003 and 2023) were analyzed with the help of the statistical methods and followed by their measurements along with graphical representations. We implemented the data of the 2023 years for the first three tasks, and for the last task, we used the data of 2003 and 2023. By histogram, we saw that the median age for female rate was high, whereas the infant mortality portion females rate were less than male. Moreover, from the boxplot our findings were that the European region had the highest value for the median age whereas the African region had the lowest value. Moreover, the Africa region had the highest mortality rate, and the Europe region had the lowest rate. In the part of data variability analysis, our findings were Western Europe had the highest rate for median ages and the lowest rate for infant mortality. In addition, in Africa region median age rate was highest for Northern Africa and the mortality rate was lowest in the same region. We also observed that there was a positive correlation along with a negative correlation between median age and infant mortality using the Pearson correlation method. Finally, the median age rate increased and the mortality rate decreased in 2023 compared to 2003. Higher lifestyles, education, health care, and medicine may explain the higher median age rate. It could be interesting to look for a regression model and implemented on this dataset in the future that links each subregion's median age and infant mortality.
