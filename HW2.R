install.packages("rvest", repos = "http://cran.r-project.org")
install.packages("gmodels")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("plotly")
install.packages("GGally")
install.packages("mi")
install.packages("betareg")
install.packages("corrplot")
install.packages("xtable")


library(rvest)
library(gmodels)
library(ggplot2)
library(reshape2)
library(plotly)
library(MASS)
library(unbalanced)
library(GGally)
library(mi)
library(betareg)
library(corrplot)
library(xtable)
library(unbalanced)

# Q1
# Load the following two datasets, generate summary statistics for all variables, plot some of the features (e.g., histograms, 
# box plots, density plots, etc.) of some variables, and save the data locally as CSV files

# Load the ALS Testing Data and TRaining Data
setwd("C:/Users/xincli/Desktop/HS650/15_ALS_CaseStudy")
TestingData <- read.csv("ALS_TestingData_78.csv")
TrainingData <- read.csv("ALS_TrainingData_2223.csv")

#Use Summary and plot certain variables with histogram, boxplot, density plot for testing data and save the summary statistics to the text file
summary(TestingData)
hist(TestingData$Age_mean, main = 'Histogram of mean age', xlab = 'Mean age')
boxplot(TestingData[,3:5], main = 'Boxplot for Albumin')
plot(density(TestingData$Albumin_median), main = 'Density plot for median albumin', xlab = 'Median albumin')
write.table(summary(TestingData), file = 'C:/Users/xincli/Desktop/HS650/HW2/ALS_TestingData.txt')

#Use Summary and plot certain variables with histogram, boxplot, density plot for training data and save the summary statistics to the text file
summary(TrainingData)
hist(TrainingData$Age_mean, main = 'Histogram of mean age', xlab = 'Mean age')
boxplot(TrainingData[,3:5], main = 'Boxplot for Albumin')
plot(density(TrainingData$Albumin_median), main = 'Density plot for median albumin', xlab = 'Median albumin')
write.table(summary(TrainingData), file = 'C:/Users/xincli/Desktop/HS650/HW2/ALS_TrainingData.txt')


# Load the SOCR Knee Pain Data
wiki_url = read_html("http://wiki.socr.umich.edu/index.php/SOCR_Data_KneePainData_041409")
html_nodes(wiki_url, "#content")
KneePain = html_table(html_nodes(wiki_url, "table")[[2]])
KneePainData = as.data.frame(KneePain)

#Summarize, plot the dataset and save the summary statistics to the text file
summary(KneePainData)
hist(KneePainData$x, main = 'Histogram of x', xlab = 'x')
boxplot(KneePainData[,1:2], main = 'Boxplot for x and Y')
plot(density(KneePainData$x), main = 'Density plot for x', xlab = 'x')
write.table(summary(KneePainData), file = 'C:/Users/xincli/Desktop/HS650/HW2/SOCR_KneePainData.txt')

# Q2
# Use ALS case-study data and long-format SOCR Parkinsons Disease data(extract rows with Time=0) 
# to explore some bivariate relations (e.g. bivariate plot, correlation, table, crosstable etc.)

plot(x = TestingData$Age_mean, y = TestingData$Albumin_median, 
     main = 'Mean age vs median albumin', xlab = 'Mean age', ylab = 'Median albumin')
cor(TestingData$Age_mean, TestingData$Albumin_median)
head(table(TestingData$Age_mean, TestingData$Albumin_median))

CrossTable(TestingData$Age_mean, TestingData$Albumin_median)

# Use 07_UMich_AnnArbor_MI_TempPrecipitation_HistData_1900_2015 data to show the relations between 
# temperature and time. [Hint: use geom_line and geom_bar]

aa_temp_data <- as.data.frame(read.csv("https://umich.instructure.com/files/706163/download?download_frd=1", header=T, na.strings=c("", ".", "NA", "NR")))
b = seq(1, 111, 5)
aa_temp_data1 = aa_temp_data[b,]
aa_temp_data_new = melt(aa_temp_data1, id.vars = 'Year')
colnames(aa_temp_data_new) = c('Year', 'Month', 'Temperature')
aa_temp_data_new$Month = as.factor(aa_temp_data_new$Month)
aa_temp_data_new$Temperature = as.numeric(aa_temp_data_new$Temperature)

plot = ggplot(aa_temp_data_new, aes(Year, Temperature, group = Month, color = Month)) + geom_line()
plot

bar = ggplot(aa_temp_data_new, aes(x = Year, y = Temperature, fill = Month)) + geom_col() + facet_grid(. ~ Month) + 
  scale_y_continuous(breaks = seq(10, 80, 10))
bar

# Q3
# Introduce (artificially) some missing data, impute the missing values and examine the differences between the original, 
# incomplete and imputed data in statistics.

n = 1000
m = 5
data = matrix(data = rnorm(5000, 10, 1), 1000, 5)
miss = sample(1:5000, 500)
data[miss] = NA
data = as.data.frame(data)
summary(data)

mdf = missing_data.frame(data)
show(mdf)
image(mdf)

imputations = mi(data, n.iter=5, n.chains=3, verbose=TRUE)
hist(imputations)


# Q4 
# Generate a surface plot for the SOCR Knee Pain Data illustrating the 2D distribution of locations of the patient 
# reported knee pain (use plotly and kernel density estimation).

KneePainData$View = as.factor(KneePainData$View)
kernal_density = with(KneePainData, MASS::kde2d(x, Y, n = 50))
with(kernal_density, plot_ly(x=x, y=y, z=z, type="surface"))


# Balancing cases
TrainingData['age_over50'] <- ifelse(TrainingData$Age_mean <= 50, 1, 0)
head(TrainingData)
balanced_ALS <- ubBalance(X = TrainingData[,], Y = as.factor(TrainingData$age_over50), 
                          type = "ubSMOTE", percOver = 100, percUnder = 200, verbose = TRUE)
table(balanced_ALS[[2]])

