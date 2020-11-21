# Brady Lange
# R Script 3
# Course: CSIS 239
# 4/5/18
# This program explores crime data and implements linear regression to manipulate the data and sees what predictors influence it.

graphics.off()
rm(list = ls())
setwd("C:/Users/brady/Documents/CSIS 239/Assignment 3")

# 1.) 
# Loading crimeData2.csv 
d <- read.csv("crimeData2.csv", header = T)

# 2.)
# Examining the data
typeof(d)
class(d)
attributes(d)

# 3.) 
# Displaying the first and last 6 rows
head(d)
tail(d)

# 4.)
# Visualing data with a histogram
hist(d$crime, xlab = "Amount of Crime", ylab = "Amount of States", main = "Histogram of Crime in the US")
hist(d$murder, xlab = "Amount of Murder", ylab = "Amount of States", main = "Histogram of Murder in the US")

# 5.)
# Testing if crimes rates and murder are related
dStats <- cor.test(d$crime, d$murder)

# 6.)
# Exploring the fields of the test object 
attributes(dStats)
# The hypothesis that crime are related is false due to the 
# t and p values. P is not < 0.05
dStats$statistic
dStats$p.value

# 7.)
# Creating a scatterplot of the data
plot(x = d$murder, y = d$crime, xlab = "Murders", ylab = "Crime Rate", main = "Scatterplot of Murders and Crime Rates")
abline(lm(formula = d$crime ~ d$murder), col = "green")

# 8.)
# Examining if the other variables can help predict crime rates.
dLM <- lm(formula = d$crime ~ d$murder + d$pctmetro + d$pcths + d$poverty + d$single)

# 9.)
# Determining the predictors that significantly affect crimes rates.
summary(dLM)
# Significant values:
# Metro areas: t value: 6.042
# Single: t value: 2.741

# 10.)
# Explaining how these predictors affect crime rates.
plot(d$single, d$crime, xlab = "Percent Single", ylab = "Crime", main = "Crime in Single")
abline(lm(formula = d$crime ~ d$single), col = "yellow")
plot(d$pctmetro, d$crime, xlab = "Percent Metro Areas", ylab = "Crime", main = "Crime in Metro Areas")
abline(lm(formula = d$crime ~ d$pctmetro), col = "purple")
# As single percentage goes up the crime goes up as well.
# As the metro area percentage goes up the crime goes up as well.

# 11.)
# Plotting the normalized residuals.
plot(residuals(dLM))

# 12.)
# Calculating influence measures for the model.
abline(h = 0)
dRStu <- rstudent(dLM)
plot(dRStu, cex = 1, ylim = c(-max(3, max(abs(dRStu))), max(3, max(abs(dRStu)))))
abline(h = c(-2, 0, 2))
abline(h = c(-3, 3), col = "red")

# 13.)
# Summarizing the influence measures.
dInfluM <- influence.measures(dLM)
plot(dLM)
summary(dInfluM)

# 14.)
# The actions these influence tests suggest to do.
# Remove the data points 9, 25, and 51 as they influence the model as outliers.

# 15.)
# Dropping outliers that would be problematic.
dDropOut <- d
dDropOut <- dDropOut[-c(9, 25, 51), ]
dDropOut

# 16.)
# Running the new statistical model.
dDropOutLM <- lm(formula = dDropOut$crime ~ dDropOut$murder + dDropOut$pctmetro + dDropOut$pcths + dDropOut$poverty + dDropOut$single)

# 17.)
# Evaluating if the model or predictors have changed.
plot(dDropOutLM)
plot(dDropOut$murder, dDropOut$crime, ylab = "Crime", xlab = "Murder", main = "Updated Plot of Crime and Murder")
# Old fit line (d variable)
abline(lm(formula = d$crime ~ d$murder), col = "green")
# New fit line (dDropOut variable)
abline(lm(formula = dDropOut$crime ~ dDropOut$murder), col = "blue")
summary(dDropOutLM)
# The predictors that were significant before are still significant.
# The model has been altered a little bit as the outliers were removed making the linear line more accurate.

