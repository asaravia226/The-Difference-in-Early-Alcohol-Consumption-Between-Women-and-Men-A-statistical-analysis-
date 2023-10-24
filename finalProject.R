
# 
# The title Page 
# 
# Introduction 
#
# The aim of this study is to observe any correlation with gender 
# and the age that people have had their first alcoholic beverage. 
# This observation would be of interest because we could focus on 
# alcohol consumption prevention specific to a gender if we do happen to 
# find that a particular gender begins consumption at an earlier age. 
#
# Hypothesis: 
#             On average, males have their first alcoholic beverage at a much 
#              younger age than females. 
#
#########################   Method Section  #########################
# Participants:
# Gather this information from the MIDUS 2 study online. 
#
# Procedure: 
# How did you collect the variables( in exact detail). 
#
# Data analysis:
# 
# Outline all statistical procedures. 
# why use an independent samples t-test? Argue why: 
# 
# TO ADRESS: 
# 1) Normality 
# 2) Homogeneity of Variance 
# 3) independence of the groups
# 
# Refer the audience to your histograms and qqplots, and results of the shapiro -Wilk test. 
# 
# We should have two demographic variables. 
# 

library(lmtest)

midusdata = read.csv("datasetW2022.xlsx", header = T, sep=",")

midusdata = datasetW2022

# Variable 1: Marriage Status
midusdata$B1PB19

# Variable 2: Life Satisfaction
midusdata$B1SSATIS

# Variable 3: Age 
midusdata$B1PAGE_M2

FinalData = midusdata[ ,c("B1PB19", "B1SSATIS", "B1PAGE_M2")]
head(FinalData)

colnames(FinalData) = c("RlpStatus", "LifeSatisfaction", "Age")

FinalData = FinalData[FinalData$RlpStatus == 1 | FinalData$RlpStatus == 3, ]

table(FinalData$RlpStatus)

##############################################################################
##############################################################################

# Variable 1 (continuous)
midusdata$B1PA49

# Variable 2 (categorical)
midusdata$B1PGENDER


data = midusdata[ ,c("B1PA49", "B1PGENDER")]
colnames(data) = c("firstDrink", "Gender")



# Let u1 = mu of the male population. 
# Let u2 = mu of the female population. 
# Hypotheses: 

# Ho: u1 = u2
# H1: u1 < u2

# t.test definitions 
# omitting "less" runs a test that will be hypothesizing 
# that males have a lower mean score.
# 

t.test(firstDrink ~ Gender, data = data, var.equal = T)

hist(data$firstDrink, breaks = 30)
mean(data$firstDrink[data$Gender == "1"])
# Mean for males = 24.80645
sd(maleDrink)
# sd = 23.77173

mean(data$firstDrink[data$Gender == "2"])
# Mean for females = 30.52632
sd(femaleDrink)
# 27.94636

# Test for Normality

# Normality for male group: 
hist(data$firstDrink[data$Gender == "1"])
shapiro.test(data$firstDrink[data$Gender == "1"])
# W = 0.47996, p-value < 2.2e-16 
# Our p-value is less than .or so we retain the null, this 
# did not come from a normally distributed population. 

# Normality for Female Group: 
hist(data$firstDrink[data$Gender == "2"])
shapiro.test(data$firstDrink[data$Gender == "2"])
# W = 0.55571, p-value < 2.2e-16
# Our p-value is < .05 so we retain the null. 
# This sample did not come from a normally distributed 
# population. 

# Equality of Variance:
# 
maleDrink = data$firstDrink[data$Gender == "1"]
femaleDrink = data$firstDrink[data$Gender == "2"]

bptest(firstDrink ~ Gender, data = data)

# p-value = .05821 is greater than .05 
# we can reject the null: the two populations 
# have equal variances.

par(mfrow = c(1, 2))
qqnorm(maleDrink, main = "Age of First Alcoholic Beverage Consumption for Males",
                  ylab = "Age")
qqline(maleDrink)
qqnorm(femaleDrink, main = "Age of First Alcoholic Beverage Consumption for Females",
       ylab = "Age")
qqline(femaleDrink)


# Transformations: 
sqrtMale = sqrt(maleDrink)
sqrtFemale = sqrt(femaleDrink)
recipmale = 1/maleDrink
recipFemale = 1/femaleDrink
logMale = log(maleDrink)
logFemale = log(femaleDrink)



hist(sqrtMale)
qqnorm(sqrtMale, main = "Square Root Transformation of Male Drinking Data",
                  ylab = "Age")
qqline(sqrtMale)
shapiro.test(sqrtMale)
# still not normal 

qqnorm(sqrtFemale, main = "Square Root Transformation of Female Drinking Data",
       ylab = "Age")
qqline(sqrtFemale)


qqnorm(recipmale, main = "Reciprocal Transformation of Male Drinking Data", ylab = "Age")
qqline(recipmale)

qqnorm(recipFemale, main = "Reciprocal Transformation of Female Drinking Data", ylab = "Age")
qqline(recipFemale)

qqnorm(logMale, main = "Logarithmic Transformation of Male Drinking Data", ylab = "Age")
qqline(logMale)

qqnorm(logFemale, main = "Logarithmic Transformation of Female Drinking Data", ylab = "Age")
qqline(logFemale)

hist(recipMale, breaks = 50)
qqnorm(recipmale)
qqline(recipmale)
shapiro.test(recipMale)
# still not normal

hist(logMale)
qqnorm(logMale)
qqline(logMale)
shapiro.test(logMale)
# not normal

###########################################
sqrtfirstDrink = sqrt(data$firstDrink)
recipfirstDrink = 1/data$firstDrink
logfirstDrink = log(data$firstDrink)



hist(sqrtfirstDrink)
qqnorm(sqrtfirstDrink)
qqline(sqrtfirstDrink)
shapiro.test(sqrtfirstDrink)
# still not normal 

hist(recipMale, breaks = 50)
qqnorm(recipmale)
qqline(recipmale)
shapiro.test(recipMale)
# still not normal

hist(logMale)
qqnorm(logMale)
qqline(logMale)
shapiro.test(logMale)
# not normal
