# Import necessary libraries
library(readr)
# Import custom R functions
source("Rfunctions.R")

# Read data from data file (SampCountries.csv) and store in R in a data frame (countries)
countries <- read_csv("SampCountries.csv")
# View dataframe
View(countries)
# Create a correlation matrix of variables
pairs(countries, lower.panel=panel.smooth, upper.panel=panel.cor, main="Scatterplot Matrix of Country Variables", cex=0.9)

# Create a multiple linear regression model to estimate life expectancy
model <- lm(LifeExp ~ Population + Health + Internet + BirthRate, data=countries)
# Output summary
print(summary(model))
# Output ANOVA
print(anova(model))

# Create an optimised linear regression model by dropping non-useful predictor variables 
final_model <- lm(LifeExp ~ Internet + BirthRate, data=countries)
# Output summary
print(summary(final_model))
# Output ANOVA
print(anova(final_model))

# Find 95% CI for intercept and predictor variables
betaCI(final_model)

# Residuals vs fitted and normal Q-Q of initial model
par(mfrow=c(2,2))
plot(model, which=1:2) 


# Predict life expectancy with birth rate of 20.5 and internet of 39.2
BirthRate <- c(20.5) 
Internet <- c(39.2)
newdf <- data.frame(BirthRate,Internet)
# Predict life expectancy using a confidence interval
pre <- predict(final_model, new=newdf,interval="confidence", se=T)
print(pre)

# Predict life expectancy with birth rate of 45.6 and internet of 39.2
BirthRate <- c(45.6) 
newdf <- data.frame(BirthRate,Internet)
# Predict life expectancy using a confidence interval
pre <- predict(final_model, new=newdf,interval="confidence", se=T)
print(pre)


# Scatterplot of birth rate per 1000 vs percentage internet
par(mfrow=c(1,1))
plot(BirthRate~Internet, data=countries, main = "Scatterplot of Birth Rate vs Percentage Internet")
# Calculate maximum value for the birth rate variable
max(countries$BirthRate)


# Residuals vs fitted and normal Q-Q of Final Model
par(mfrow=c(1,2))
plot(final_model, which=1:2) 

# Test normally distributed residuals of Final Model
shapiro.test(final_model$residuals)





