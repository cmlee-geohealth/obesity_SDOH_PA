# set the working directory
wd = 'C:/EPBI8208_Local(C)/Project/Data Processing_12.07.2023'
setwd(wd)
getwd()

# load packages
library(dplyr)
library(tidyverse)
library(modelr)
library(car)

# Data --------------------------------------------------------------------

# CDC Places data
obesity_data <- read.csv("Data/CDC_Place_Data/CDC_Extract.csv")

# Census Bureau ACS data
dp05_data <- read.csv("Data/Census_ACS/csv_3_not_included/DP05_Gender_Race_Ethnicity.csv")
education_data <- read.csv("Data/Census_ACS/csv_3_not_included/S1501_Education.csv")
poverty_data <- read.csv("Data/Census_ACS/csv_3_not_included/S1701_Poverty.csv")

colnames(obesity_data)
head(obesity_data)
# 3,196 census tract records available with obesity measure
dim(obesity_data)

colnames(dp05_data)
head(dp05_data)
# 3,446 census tract records
dim(dp05_data)

colnames(education_data)
head(education_data)
# 3,446 census tract records
dim(education_data)

colnames(poverty_data)
head(poverty_data)
# 3,446 census tract records
dim(poverty_data)


# Pre-Processing ----------------------------------------------------------

# merge the datasets by using geoid (census tract id)
census_data <- merge(dp05_data, education_data, by = "geoid")
census_data <- merge(census_data, poverty_data, by = "geoid")
dim(census_data)
# 3,446 census tract records retained

# merge the obesity measure file with demographic measures
composite_data <- merge(obesity_data, census_data, by.x = "LocationID",
                        by.y = "geoid")
dim(composite_data)
# only 2,921 common census tract records exist at the same time in both datasets of obesity measure and demographic information

# remove unnecessary columns
composite_data <- subset(composite_data, select = -c(CountyFIPS, TotalPopulation))

# rename columns for better understanding
composite_data = rename(composite_data, "geoid" = "LocationID", "obesity" = "Data_Value")

# Calculate the poverty rate
# the imported data provides the population whose poverty status is determined
# calculate the poverty rate 
composite_data$pov_18over_perc <- composite_data$pop_poor_18over / (
  composite_data$pop_poor_18over + composite_data$pop)

# remove the columns which were used for calculating the poverty rate since they are necessary anymore
composite_data <- subset(composite_data, select = -c(pop, pop_poor_18over))


# Descriptive Statistics --------------------------------------------------

# use summary function to find descriptive statistics
summary(composite_data)
# some columns are character instead of numeric

# convert character columns to numeric
composite_data$female_num <- as.numeric(composite_data$female)
composite_data$black_num <- as.numeric(composite_data$black)
composite_data$hisp_num <- as.numeric(composite_data$hisp)
composite_data$less_educ_num <- as.numeric(composite_data$less_educ)

# run summary again
summary(composite_data)

# create a new data frame which only contains necessary columns 
composite_numeric <- subset(composite_data, select = c(geoid, obesity, female_num, black_num, hisp_num, less_educ_num, pov_18over_perc))

# identify the number of missing values
sapply(composite_numeric, function(x) sum(is.na(x)))


# Data Exploration and Visualization --------------------------------------

# create a scatterplot of female % and male % to see the distribution by the obesity measure
ggplot(data = composite_numeric, aes(x = obesity, y = female_num)) +
  geom_point()
# neighborhorhoods with female percent cluster between the range of approximately 0.4 and 0.6
# they seem to most cluster when the obesity % is between 25% and 45%

ggplot(data = composite_numeric, aes(x = obesity, y = black_num)) +
  geom_point()
# neighborhoods with black population are distributed between 0 to 1
# they are most clustered between 0 to 0.25
# obesity measure of this range is between approximately 25% and 45%

ggplot(data = composite_numeric, aes(x = obesity, y = hisp_num)) +
  geom_point()
# neighborhoods with hispani population are most prevalent bewteen 0 to 0.1
# these have the obeisty measure concentration between 28% and 43%

ggplot(data = composite_numeric, aes(x = obesity, y = less_educ_num)) +
  geom_point()
# the variable less education seem to show cluster presenting a linear relationship with obesity
# as the obesity measure increases, less education % increases as well

ggplot(data = composite_numeric, aes(x = obesity, y = pov_18over_perc)) +
  geom_point()
# the poor % seem to be most prevalent between 0.4 and 0.5
# this cluster of poor % has the range of the obesity measure between 25% and 45% 

# Correlation

# correlation between obesity % and female %
cor(composite_numeric$female_num, composite_numeric$obesity,  method = "pearson", use = "complete.obs")
# 0.001445938

# correlation between obesity % and black %
cor(composite_numeric$black_num, composite_numeric$obesity,  method = "pearson", use = "complete.obs")
# 0.3399277

# correlation between obesity % and hisp %
cor(composite_numeric$hisp_num, composite_numeric$obesity,  method = "pearson", use = "complete.obs")
# 0.2377433

# correlation between obesity % and less education %
cor(composite_numeric$less_educ_num, composite_numeric$obesity,  method = "pearson", use = "complete.obs")
# 0.7036358

# correlation between obesity % and poor %
cor(composite_numeric$pov_18over_perc, composite_numeric$obesity,  method = "pearson", use = "complete.obs")
# -0.06289325


# Regression Model --------------------------------------------------------

lm1 <- lm(obesity ~ female_num + black_num + hisp_num + less_educ_num + pov_18over_perc, data=composite_numeric, na.action=na.exclude)

summary(lm1)

# Regression Diagnositics -------------------------------------------------

# Normality
composite_numeric$fitted <- fitted(lm1)
composite_numeric$resid <- resid(lm1)

ggplot(composite_numeric, aes(resid)) + 
  geom_freqpoly(binwidth = 2)

# QQ Plot
qqnorm(composite_numeric$resid)
qqline(composite_numeric$resid)


# Linearity

ggplot(composite_numeric, aes(fitted, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() + 
  geom_smooth(se=FALSE) +
  xlab("Fitted values") + ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")

# Homoscedasticity
composite_numeric$rstandard <- rstandard(lm1)

ggplot(composite_numeric, aes(fitted, sqrt(abs(rstandard)))) +
  geom_point(na.rm=TRUE) +
  stat_smooth(method="loess", se=FALSE, na.rm = TRUE) +
  xlab("Fitted Value") + 
  ylab(expression(sqrt("|Standardized residuals|"))) +
  ggtitle("Scale-Location") +
  theme_bw()


ncvTest(lm1)


# Outliers

# Leverage
composite_numeric$leverage <- hatvalues(lm1)

ggplot(composite_numeric, aes(leverage, rstandard)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + geom_smooth(se = FALSE) + 
  ggtitle("Standardized Residual vs. Leverage") 

# Cook's distance
composite_numeric$cooksD <- cooks.distance(lm1)
cutoff <- 4/(nrow(composite_numeric)-length(lm1$coefficients)-1)
cutoff

ggplot(composite_numeric, aes(seq_along(cooksD), cooksD)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  ggtitle("Cook's distance") +
  theme_bw() +
  geom_hline(yintercept = cutoff)

influencePlot(lm1, main="Influence Plot", 
              sub="Circle size is proportional to Cook's distance")
