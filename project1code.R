# Load Necessary Packages and set the appropriate global settings 
#| label: Stat 5100 Setup
#| echo: false
# keep the environment clean and predictable between runs
rm(list = ls())
# make printed numbers and plots a bit nicer
options(digits = 4, scipen = 999)
# set the seed for reproducibility
set.seed(123)
# load in packages
library(tidyverse)
library(corrplot)
library(MASS)
library(car)


# Load in the dataset
heat <- read.csv("urban_heat_partA.csv")
head(heat)
summary(heat)


# View each variable's distribution and normality 
# Possible Response Variables 
hist(heat$surface_temp_c,
     main = "Afternoon Land Surface Temperature",
     ylab = "Frequency",
     xlab = "Degrees Celsius",
     col = "#D40000", 
     xlim = c(25, 55))
qqnorm(heat$surface_temp_c)
summary(heat$surface_temp_c)

hist(heat$energy_kwh,
     main = "Residential Electricity Use per Household",
     ylab = "Frequency",
     xlab = "Kilowatt-hour (kWh)",
     col = "#E2C400")
qqnorm(heat$energy_kwh)
summary(heat$energy_kwh)

# Land Cover Variables 
hist(heat$block_id,
     main = "Block Identifier Variable (Not a Predictor)",
     ylab = "Frequency",
     xlab = "Block ID (Random Block Identifier)",
     col = "red3")
qqnorm(heat$block_id)
summary(heat$block_id)

hist(heat$canopy_pct,
     main = "Percent of Block Area Covered by Canopy",
     ylab = "Frequency",
     xlab = "Percent Block Area Covered by Canopy", 
     col = "#009150",
     ylim = c(0, 80))
qqnorm(heat$canopy_pct)
summary(heat$canopy_pct)

hist(heat$impervious_pct,
     main = "Percent of Block Area Covered by Impervious Surfaces",
     ylab = "Frequency",
     xlab = "Percent Block Area Covered by Impervious Surfaces", 
     col = "black")
qqnorm(heat$impervious_pct)
summary(heat$impervious_pct)

hist(heat$albedo,
     main = "Albedo",
     ylab = "Frequency",
     xlab = "Average Surface Reflectance", 
     col = "orange", 
     xlim = c(0.05, 0.3))
qqnorm(heat$albedo)
summary(heat$albedo)

#Location Variables 
hist(heat$park_dist_m,
     main = "Distance from the Center of the Block to Nearest Park",
     ylab = "Frequency",
     xlab = "Distance to Nearest Park (m)", 
     col = "purple4")
qqnorm(heat$park_dist_m)
summary(heat$park_dist_m)

hist(heat$water_dist_m,
     main = "Distance from the Center of the Block to the Nearest Body of Water",
     ylab = "Frequency",
     xlab = "Distance to Nearest Body of Water (m)", 
     col = "blue4")
qqnorm(heat$water_dist_m)
summary(heat$water_dist_m)

hist(heat$elevation_m,
     main = "Average Block Elevation Above Sea Level",
     ylab = "Frequency",
     xlab = "Average Elevation Above Sea Level", 
     col = "magenta3")
qqnorm(heat$elevation_m)
summary(heat$elevation_m)

hist(heat$traffic_aadt,
     main = "Annual Average Daily Traffic on Busiest Street on the Block",
     ylab = "Frequency",
     xlab = "Vehicles per Day", 
     col = "gray52")
qqnorm(heat$traffic_aadt)
summary(heat$traffic_aadt)

# People and Housing
hist(heat$pop_density_km2,
     main = "Residents per Square Kilometer",
     ylab = "Frequency",
     xlab = "People per Square Kilometer", 
     col = "#80471C", 
     ylim = c(0, 80))
qqnorm(heat$pop_density_km2)
summary(heat$pop_density_km2)

hist(heat$household_size,
     main = "Average People per Household",
     ylab = "Frequency",
     xlab = "People", 
     col = "#645", 
     xlim = c(1, 4), 
     ylim = c(0, 60))
qqnorm(heat$household_size)
summary(heat$household_size)

hist(heat$home_size_m2,
     main = "Median Floor Area of Homes",
     ylab = "Frequency",
     xlab = "Square Meters", 
     col = "#E30B5D")
qqnorm(heat$home_size_m2)
summary(heat$home_size_m2)

hist(heat$building_age_yr,
     main = "Median Age of Buildings on the Block",
     ylab = "Frequency",
     xlab = "Years", 
     col = "#777696")
qqnorm(heat$building_age_yr)
summary(heat$building_age_yr)

hist(heat$median_income,
     main = "Median Income in US Dollars", 
     ylab = "Frequency",
     xlab = "US Dollars", 
     col = "green")
qqnorm(heat$median_income)
summary(heat$median_income)



# Look at How the Variables are Related with a Correlation Plot 
pairs(heat)
corr_matrix <- cor(heat %>% select_if(is.numeric), use = "complete.obs") 
corrplot(corr_matrix, method = "circle")

# make this plot a little nicer 
pairs(~ surface_temp_c + canopy_pct + impervious_pct + home_size_m2, 
      data = heat)

# Pairwise Scatterplots 
plot(heat$surface_temp_c ~ heat$canopy_pct)
plot(heat$surface_temp_c ~ heat$impervious_pct)
plot(heat$surface_temp_c ~ heat$home_size_m2)
plot(heat$surface_temp_c ~ heat$median_income)
plot(heat$surface_temp_c ~ heat$pop_density_km2)
plot(heat$surface_temp_c ~ heat$traffic_aadt)


# Look at Canopy more in Depth
hist(heat$canopy_pct,
     main = "Percent of Block Area Covered by Canopy",
     ylab = "Frequency",
     xlab = "Percent Block Area Covered by Canopy", 
     col = "#009150")
summary(heat$canopy_pct)
qqnorm(heat$canopy_pct)
# numerical test of normality:
shapiro.test(heat$canopy_pct) # DATA IS NOT NORMAL
boxplot(heat$canopy_pct,
        boxwex = 0.5,
        col = "#009150")
# Appears to have an outlier 


# Look at Percent of Impervious Surfaces more in Depth
hist(heat$impervious_pct,
     main = "Percent of Block Area Covered by Impervious Surfaces",
     ylab = "Frequency",
     xlab = "Percent Block Area Covered by Impervious Surfaces", 
     col = "black")
qqnorm(heat$impervious_pct)
summary(heat$impervious_pct)
boxplot(heat$impervious_pct,
        boxwex = 0.5,
        col = "black")
# Appears to only have 1 outlier


# Look at the home_size_m2 Variable more in Depth
hist(heat$home_size_m2,
     main = "Median Floor Area of Homes",
     ylab = "Frequency",
     xlab = "Floor Area (Square Meters)", 
     col = "#E30B5D")
summary(heat$home_size_m2)
qqnorm(heat$home_size_m2)
# numerical test of normality:
shapiro.test(heat$home_size_m2) # DATA IS NOT NORMAL

boxplot(heat$home_size_m2,
        boxwex = 0.5, 
        col = "#E30B5D")
# Appears to have around 4 outliers in this set. 

# First model and checks
first_lm <- lm(surface_temp_c ~ canopy_pct + impervious_pct + home_size_m2,
               data = heat)
resids <- first_lm$residuals

summary(first_lm)
plot(first_lm, which = 1)
plot(first_lm, which = 2)
plot(first_lm, which = 3)
plot(first_lm, which = 4)
plot(first_lm, which = 5)

# Check Normality Specifically
plot(first_lm, which = 2)
shapiro.test(resids)
# Does not appear that the residuals are normal

# Try to log transform the response to become more normal
log_surface <- log(heat$surface_temp_c)
log_model <- lm(log_surface ~ canopy_pct + impervious_pct + home_size_m2, 
                data = heat)

# Recheck Normality
shapiro.test(log_model$residuals)
plot(log_model, which = 2)

#check constant variance with levene test and plot
plot(log_model, which = 1)
plot(x = X, y = resids,
     xlab = "lot size",
     ylab = "residuals")
abline(h = 0, lty = 2)
resids <- log_model$residuals #fit is the stored model in this case, extract the residuals from this
Y_hat <- log_model$fitted # extract the fitted model y hat values

# identify the median fitted value
med_Y_hat <- median(Y_hat)

# add Y_hat and the residuals to our heat dataframe
heat$Y_hat <- Y_hat
heat$resids <- resids

# define a group variable that divides our data in half using Y_hat: Creates two groups 
heat$group <- factor(ifelse(heat$Y_hat > med_Y_hat, "High", "Low"))

# Do a levene test on the two groups:
car::leveneTest(y = heat$resids,
                group = heat$group,
                data = heat)
#assumption of levene test = homogeneity of variance/ constant variance 

  
# Check independence (sequence plot)
# create an index variable by making a sequence of numbers from 1 to n:
index <- seq(1, nrow(heat)) 

plot(x = index, y = resids,
     type = 'b',
     xlab = "Index",
     ylab = "Residuals", 
     main = "Sequence Plot")
abline(h = 0, lty = 2)


# Check for Influential Points 
#1 - Leverage 
n <- nrow(heat)
p <- length(coef(log_model))

lev <- hatvalues(log_model) #Grab leverage out of a fitted model

plot(lev, type = "h", ylab = "Leverage", xlab = "Observation")
abline(h = 2 * p / n, lty = 2, col = "red")
text(which(lev > 2 * p / n) #This is the threshold for leverage 
     , lev[lev > 2 * p / n],
     labels = names(lev)[lev > 2 * p / n], pos = 2, cex = 0.7)

sort(lev, decreasing = TRUE)[1:5]

#2 - Outliers
# studentize the residuals (standardize the residuals)
stud <- rstudent(log_model)
plot(fitted(log_model), stud,
     xlab = "Fitted values", ylab = "Studentized residual")
abline(h = c(-2, 0, 2), lty = c(2, 1, 2), col = c("red", "black", "red"))
text(fitted(log_model)[abs(stud) > 2], stud[abs(stud) > 2],
     labels = names(stud)[abs(stud) > 2], pos = 4, cex = 0.7)

sort(abs(stud), decreasing = TRUE)[1:5]


#3 - Influence 
cd <- cooks.distance(log_model)

plot(cd, type = "h", ylab = "Cook's distance", xlab = "Observation")
abline(h = 4 / n, lty = 2, col = "red") #4/n is threshold
abline(h = qf(0.5, p, n - p), lty = 3, col = "blue") #qf is another threshold 
text(which(cd > 4 / n), cd[cd > 4 / n],
     labels = names(cd)[cd > 4 / n], pos = 2, cex = 0.7)

sort(cd, decreasing = TRUE)[1:5]

# Influence using residual vs. leverage plot 
plot(log_model, which = 5)

# Checking other possible parameters
in_case_lm <- lm(surface_temp_c ~ canopy_pct + impervious_pct + pop_density_km2
                 + traffic_aadt + median_income + home_size_m2, 
                 data = heat)
confint(in_case_lm)

# Check for Multicollinearity 
vif(log_model)

# ANOVA
anova(log_model)

# Perform subset F-tests
#canopy predictor
log_model
only_canopy_log <- lm(log_surface ~ canopy_pct,
                      data = heat)
anova(log_model, only_canopy_log)

# impervious predictor
only_imperv_log <- lm(log_surface ~ impervious_pct, 
                      data = heat)
anova(log_model, only_imperv_log)

# house size predictor
only_homes_log <- lm(log_surface ~ home_size_m2, 
                     data = heat)
anova(log_model, only_homes_log)

# Confidence Intervals
confint(log_model)

# Point estimates 
# Beta-not
(3.6389203 + 3.72862262)/2

# canopy and canopy interpretation
(-0.0027163 + -0.00173090) / 2
exp(-0.002224)
exp(1.3)

# impervious surfaces and interpretation
(0.00182225 + 0.0008533)/2
exp(0.001338)

# average home size and interpretation
(-0.0004001 + -0.00007542)/2
exp(-0.0002378)
