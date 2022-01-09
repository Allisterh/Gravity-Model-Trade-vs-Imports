# to save raw table data as a r script
save(gravity, file = "gravity.RData")

# we can view the data:
View(gravity)

# We can add labels for each variable to give more information on it.
attach(gravity)
gravity <- apply_labels (gravity,
                        country_home = "Home country name",
                        country_host = "Trading partner country name",
                        import2009 = "UK imports trading partner (million US Dollars)",
                        GDP2009 = "Trading partner GDP (billion US Dollars)",
                        dist = "Distance measured in km",
                        landlocked_host = "1 if trading partner is a landlocked country, 0 otherwise",
                        colony = " 1 if UK and trading partner have ever had a colonial link, 0 otherwise",
                        )
                        
# generate summary for all columns on gravity data.
summary(gravity)


# generate sd for specific column on gravity data

sd(gravity$import2009) #produces sd of imports in 2009
sd(gravity$GDP2009)    #produces sd of GDP in 2009
sd(gravity$dist)        #produces sd of distance between home country and host.
sd(gravity$landlocked_host)  #produces sd of landlocked
sd(gravity$colony)      #produces sd of colony


# generate variance for specific column on gravity data

var(gravity$import2009)  #produces variance of imports in 2009
var(gravity$GDP2009)     #produces variance of GDP in 2009
var(gravity$dist)        #produces variance of distance between home country and host.
var(gravity$landlocked_host)  #produces variance of landlocked
var(gravity$colony)      #produces variance of colony


#produce a boxplot to see the dispersion of data for each column in gravity

boxplot(gravity$import2009)  #produces boxplot of imports in 2009
boxplot(gravity$GDP2009)     #produces boxplot of GDP in 2009
boxplot(gravity$dist)        #produces boxplot of distance between home country and host.
boxplot(gravity$landlocked_host)  #produces boxplot of landlocked
boxplot(gravity$colony)      #produces boxplot of colony


boxplot(import2009~colony,data=gravity, main="Import 2009 by Colony",
        xlab="1:Colony - 0:No Colony", ylab="Import 2009")

boxplot(import2009~landlocked_host,data=gravity, main="Import 2009 by Landlocked",
        xlab="1:Landlocked - 0:No Landlocked", ylab="Import 2009")


# produce a SCATTER PLOT (1) of two variables
# distance on x-axis and exports on y-axis (these variables need to be numerical)

plot(log(gravity$import2009)~gravity$dist , xlab = 'Distance (Km)' , ylab = 'Imports In 2009 (Million US Dollars)' , main = 'Impact Of Distance On Imports' , pch=20 , col='black')

# now we need to add a regression line (1) to better understand the trend

abline(lm(log(import2009)~dist,data=gravity), col="red")

# Mention that we are looking at the log of imports 

# To test this relationship we use as follows; this tells us whether its a negative correlation or positive correlation
cor(gravity$import2009, gravity$dist) #generate correlation coefficient

# we can use covariance: This covariance tells us how closely two variables are moving together.But we cant tell how weak the relationship actually is.
# cov(gravity$dist, gravity$import2009)

cor.test(gravity$dist, gravity$import2009) 


# produce a SCATTER PLOT (2) of two variables
# GDP2009 on x-axis and exports on y-axis (these variables need to be numerical)

# this plot is in log-log scale

plot(log(gravity$import2009)~log(gravity$GDP2009) , xlab = 'GDP 2009 (Billion US Dollars)' , ylab = 'Imports In 2009 (Million US Dollars)' , main = 'Impact Of GDP On Imports' , pch=20 , col='black')

# now we need to add a regression line (2) to better understand the trend

abline(lm(log(import2009)~log(GDP2009),data=gravity), col="red")

# To test this relationship we use as follows; this tells us whether its a negative correlation or positive correlation
cor((gravity$GDP2009), (gravity$import2009)) #generate correlation coefficient

# we can use covariance: This covariance tells us how closely two variables are moving together.But we cant tell how weak the relationship actually is.
#cov(gravity$GDP2009, gravity$import2009)

# confidence interval
cor.test(gravity$GDP2009, gravity$import2009) 




######### REGRESSION ANALYSIS ############ 

### LINEAR MODEL

model1 <- lm(import2009~GDP2009+dist, data=gravity)
options(scipen=0) # get rid off the scientific form SET scipen=0 to normal form
summary(model1) 
model1$fitted
confint(model1)  # we can also produce confidence intervals for this model
anova(model1)
par(mfrow=c(2,2))
plot(model1)      # we can also produce the anova table for linear regression model
termplot(model1) # diagnostic plot of residuals
mean(model1$residuals)  # the mean of residuals from the simple regression
cor.test(gravity$dist, model1$residuals)      # To check if X variables and residuals are uncorrelated.
bptest(model1)

# Perform an F test
# Run a first, RESTRICTED model (because it has less parameters than
# the one we will run later)
# b4 = 0
e1 = model1$residuals
RSS1 = sum(e1^2)

# Run another regression (the UNRESTRICTED model)
# b4 = anything
e3 = model3$residuals # Collect residuals
RSS3 = sum(e3^2) # Compute the sum of squares
# Get teh degrees of freedom
df1 = 1 # the difference between the number of paramaters in the first
# and the second model
df2 = 153 # this the df of the first model, which you get in the last line of
# summary

# Compute the test statistic
Fstat = ((RSS1 - RSS3)/(df1))/(RSS1/(df2)) 
# Now we need to compare this number with the critical value
# at level of significance level we care about
# need to use the quantile function for the F distribution

alpha = 0.05
qf(1-alpha, df1, df2)

# since Fstat > qf() then we reject the null hypothesis that there is no
# difference between the restricted and unrestricted model

# and in particular, the unrestricted one is "better", in the sense
# that it explains more variance than the other one, even when taking into account
# that it has more variables

##
## Estimate of both the role of GDP and distance are statistically significant
# at any level of confidence.

# 0.0003473 one more point of GDP increase import to UK by this much
# turn this into an actual number

### SEMI LOG MODEL 

model2 <- lm(log(import2009)~GDP2009+dist, data=gravity)
options(scipen=999)# log of dependent variable
summary(model2)
model2$fitted
confint(model2)
anova(model2)
plot(model2)
termplot(model2)
mean(model2$residuals)  # the mean of residuals from the simple regression
cor.test(gravity$dist, model2$residuals)      # To check if X variables and residuals are uncorrelated.
bptest(model2)

### INTERACTIVE MODEL

model3 <- lm(import2009~GDP2009+dist+GDP2009*dist, data=gravity)
options(scipen=8)
summary(model3)
model3$fitted
confint(model3)
anova(model3)
plot(model3)
termplot(model3)
mean(model3$residuals)  
cor.test(gravity$dist, model3$residuals)     
bptest(model3)

### BASIC GRAVITY MODEL

model4 <- lm(log(import2009)~log(GDP2009)+log(dist), data=gravity)
options(scipen=999)
summary(model4)
model4$fitted
confint(model4)
anova(model4)
plot(model4)
termplot(model4)
mean(model4$residuals)  
cor.test(gravity$dist, model4$residuals)     
bptest(model4)


### AUGMENTED GRAVITY MODEL

model5 <- lm(log(import2009)~log(GDP2009)+log(dist)+landlocked_host+colony, data = gravity)
options(scipen=0)
summary(model5)
model5$fitted
confint(model5)
anova(model5)
plot(model5)
termplot(model5)
mean(model5$residuals)  
cor.test(gravity$dist, model5$residuals)     
bptest(model5)




### BASIC GRAVITY MODEL VS AUGMENTED GRAVITY MODEL COMPARISON (F-Test)
# which is statistically better

#R^2=0.6961
#R^2=0.7382

# Now compute F-TEST
((0.7382-0.6961)/2)/((1-0.7382)/(151))
#F-Test (computed) = 12.14114

#F-Test critical value with (2, 151) dof
qf(0.95,2,151)  #F-Test (critical) = 3.055959

#Since F-Test (computed) > F-Test (critical)
             #12.14114 > 3.055959
# we can reject the null hypothesis and infer that unrestricted model AGM is better

anova(model4, model5)  # comparing RSS of different models

### Another attempt at F-Test model 4 vs model 5 ########

# Perform an F test
# Run a first, RESTRICTED model (because it has less parameters than
# the one we will run later)
# b4 = 0
e4 = model4$residuals
RSS4 = sum(e4^2)

# Run another regression (the UNRESTRICTED model)
# b4 = anything
e5 = model5$residuals # Collect residuals
RSS5 = sum(e5^2) # Compute the sum of squares
# Get teh degrees of freedom
df1 = 1 # the difference between the number of paramaters in the first
# and the second model
df2 = 153 # this the df of the first model, which you get in the last line of
# summary

# Compute the test statistic
Fstat = ((RSS4 - RSS5)/(df1))/(RSS4/(df2)) 
# Now we need to compare this number with the critical value
# at level of significance level we care about
# need to use the quantile function for the F distribution

alpha = 0.05
qf(1-alpha, df1, df2)









### SEMI-LOG MODEL VS AUGMENTED GRAVITY MODEL COMPARISON (F-Test)
# which is statistically better

#R^2=0.1939
#R^2=0.7382

# Now compute F-TEST
((0.7382-0.1939)/2)/((1-0.7382)/(151))
#F-Test (computed) = 156.9696

#F-Test critical value with (2, 151) dof
qf(0.95,2,151)  #F-Test (critical) = 3.055959

#Since F-Test (computed) > F-Test (critical)
#156.9696 > 3.055959
# we can reject the null hypothesis and infer that unrestricted model AGM is better

anova(model2, model5)




### A summary of our comparison

## Within the linear model formulation we have concluded through an F test that
## interactive model is a better fit for the data. This also has a sensible 
## interpretation in that one would think that there is a role for the
## interaction of distance and GDP (in the sense that one would imagine 
## larger GDP would be less 
## affected by distance and viceversa)

## Overall, both these two models have a decent value of adjusted R2 
## explaining about 52% of the variation.

## The semi-log model instead performs very poorly in term of fit to the data
## and there is no particular reason to prefer it from a theoretical standpoint.

### Both the basic and augmented gravity model perform very strongly in fitting
## the data with an adjusted R square that is very high.
## It makes sense to introduce the distinction between trading with former colonies 
## with landlocked countries (since that is going to affect the cost
## of transport) and (perform another F test and report conclusions here)


# In its simplest, most basic format, the following code will produce a
# regression table with the five specified models

stargazer(model1, model2, model3, model4, model5,
          title="Regression Model Results",
          align =TRUE,
          dep.var.caption = "DV: Imports in 2009 (UK Imports From Trading Partner)",
          dep.var.labels = c("Linear Model","Semi-log Model","Interactive Model","Basic Gravity Model","Augmented Gravity Model"),
          covariate.labels = c("GDP 2009", "Distance", "Landlocked", "log(GDP2009)"),
          notes.label = "Significance Levels:",
          no.space=TRUE,
          type="html",
          out="regression/regressionoutputtable.htm")

































                        