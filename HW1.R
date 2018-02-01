
#Problem 1.1 (Long-to-Wide Data format translation):
  
#We demonstrated the wide-to-long conversion in lecture. 
#Now, let¡¯s explore long-to-wide. Load in the long-format SOCR Parkinson's Disease dataLinks to an external site. and export it as wide format. For simplicity, just choose only 3 variables (avoiding case and time variables). Please note that there are several time observations for each subject. You need to transform the features according to the time variable. Try to use reshape function.

library(rvest)
#Load the data from website
site <- "http://wiki.socr.umich.edu/index.php/SOCR_Data_PD_BiomedBigMetadata"
wiki_url <- read_html(site)
html_nodes(wiki_url, "#content")
pd_data <- html_table(html_nodes(wiki_url,"table")[[1]])
head(pd_data); summary(pd_data)

#Choose three variables Sex, Weight and Age other than Cases and Time
pd_data_sub <- pd_data[,c("Cases", "Time", "Sex", "Weight", "Age")]

#Transform the data from long to wide
wide_data <- reshape(pd_data_sub, timevar = "Time", idvar = "Cases",
                     direction = "wide")
str(pd_data_sub)

#Problem 1.2 (Data stratification):
  
#Use the same SOCR Parkinson's Disease dataLinks to an external site. and extract rows satisfying Time=0. Complete the following protocol in R:

#Extract the first 10 subjects
#Find the cases for which L_caudate_ComputeArea<600.
#Sort the subjects based on L_caudate_Volume in descending and ascending order.
#Generate frequency and probability tables for Age and Sex.
#Compute the mean Age and the correlation between Age and Weight.
#Plot Histogram and density of R_fusiform_gyrus_Volume and scatterplot L_fusiform_gyrus_Volume and R_fusiform_gyrus_Volume.

#Extract rows satisfying Time=0
pd_data_timezero <- subset(pd_data, Time == 0)
pd_data_timezero

#Extract the first 10 subjects
pd_data_timezero_10 <- pd_data_timezero[1:10,]
str(pd_data_timezero_10)

#Find the cases for which L_caudate_ComputeArea<600.
Cauareabelow600 <- subset(pd_data_timezero_10, L_caudate_ComputeArea < 600) 
str(Cauareabelow600)

#Generate frequency and probability tables for Age and Sex.
table(pd_data$Sex)
table(pd_data$Age)
prop.table(pd_data$Sex)
prop.table(pd_data$Age)

#Sort the subjects based on L_caudate_Volume in ascending order.
SortAscend <- Cauareabelow600[order(Cauareabelow600$L_caudate_Volume), ]
head(SortAscend$L_caudate_Volume)

#Sort the subjects based on L_caudate_Volume in descending order.
SortDescend <- Cauareabelow600[order(Cauareabelow600$L_caudate_Volume, decreasing = T),]
head(SortDescend$L_caudate_Volume)

#Compute the mean Age.
mean(pd_data_timezero$Age)

#Compute the correlation between Age and Weight.
cor(pd_data_timezero$Age, pd_data_timezero$Weight)

#Plot Histogram and density of R_fusiform_gyrus_Volume.
hist(pd_data_timezero$R_fusiform_gyrus_Volume, freq = F, main = "Histogram of R fusiform gyrus volume at Time 0", xlab = "R fusiform gyrus volume", ylab = "density")

#Plot the scatterplot L_fusiform_gyrus_Volume and R_fusiform_gyrus_Volume.
plot(pd_data_timezero$R_fusiform_gyrus_Volume, pd_data_timezero$L_fusiform_gyrus_Volume, main = "Scatterplot of Right and Left fusiform gyrus volume at Time 0", xlabs = "Right fusiform gyrus volume", ylabs = "Left fusiform gyrus volume")

#Problem 1.3 (Simulation)
#Generate 1,000 standard normal variables and 1,200 student t distributed random variables with df=20. Generate a quantile-quantile (Q-Q) probability plot of the two samples. Then, compare it with qqnorm of student t simulation.

#1 Generate 1,000 standard normal variables
stuvar <- rnorm(1000)

#2 Generate 1,200 student t distributed random variables with df=20
stdist <- rt(1200, df = 20)

#3 Generate a quantile-quantile (Q-Q) probability plot of the two samples.
qqplot(stuvar, stdist)

#4 Generate qqnorm of student t simulation
qqnorm(stdist)

#5 Compare these two plots: they look similar with a bit of difference at the beginning #and end of the lines


#Problem 1.4 (Define an R SD function)
#Generate a function that computes a sample standard deviation function and compare it against the sd function using the simulation data you generate in the last question. Did you cover all possible situations for the input data?

stuvar <- rnorm(1000)
a <- c(stuvar)
b <- sqrt(sum((a-mean(a))**2/(length(a)-1)))
b

c<- sd(a)
c

stdist <- rt(1200, df = 20)
d <- c(stdist)
e <- sqrt(sum((d-mean(d))**2/(length(d)-1)))
e

f <- sd(stdist)
f4

#Yes, it looks like I covered all the possible situations for this input data. And the function I defined for standard deviation generates the same output as the one from the inserted function sd().  


