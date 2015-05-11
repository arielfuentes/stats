### From the webpage salemmafari http://www.salemmarafi.com/code/price-elasticity-with-r/

###################################
#   Warm-up ... data and stuff    #
###################################
 
# Load data and output summary stats
sales.data<-read.csv('supermarket.csv')
 
#Look at column names and their classes
sapply(sales.data,class)
 
# Change Ad.Type to factor and print Summary Stats
sales.data$Ad.Type<-as.factor(sales.data$Ad.Type)
summary(sales.data)
 
####################
#  Create Models   #
####################
 
# Load required library
library(memisc) 
 
# Create models
m1<-lm(formula=Sales~Price.Eggs,data=sales.data)
m2<-update(m1,.~.+Ad.Type)
m3<-update(m2,.~.+Price.Cookies)
mtable(m1,m2,m3)
 
####################
#   DIAGNOSTICS    #
####################
 
# Linearity Plots
par(mfrow=c(1,2))
plot(m3)
par(mfrow=c(1,1))
 
# Multi-collinearity
library(car)
vif(m3) # variance inflation factors 
sqrt(vif(m3)) > 2 # problem?
 
# Diagnosis: Nonlinearity
crPlots(m3)
 
# Diagnosis: Non-independence of Errors 
# We want a D-W Statistic close to 2
durbinWatsonTest(m3)
 
#########################
#   Price Elasticity    #
#########################
 
# Calculate Price Elasticity
PE<-as.numeric(m3$coefficients["Price.Eggs"] * mean(sales.data$Price.Eggs)/mean(sales.data$Sales))
CPEcookies<-as.numeric(m3$coefficients["Price.Cookies"] * mean(sales.data$Price.Cookies)/mean(sales.data$Sales))
 
# Print Results 
PE
CPEcookies
 
 
####################################
#   BONUS - What about the ads?    #
####################################
 
# Subset the data
sales.adEggs <- subset(sales.data,Ad.Type==0)
sales.adCookies <- subset(sales.data,Ad.Type==1)
 
# Diagnostic on subsets' means and if they are different ... they are. 
wilcox.test(x=sales.adCookies$Sales,y=sales.adEggs$Sales,exact=F,paired=T)
 
# On average, does the advert for eggs generate higher sales for eggs?
mean(sales.adEggs$Sales) >= mean(sales.adCookies$Sales)
