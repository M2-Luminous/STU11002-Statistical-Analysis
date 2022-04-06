head(pressure)#inspect the start of the data set
cor(pressure$temperature, pressure$pressure)
#check the correlation between the variables

plot(pressure$temperature, pressure$pressure)
#visualize a scatter plot of the data set

scatter.smooth(x=pressure$temperature, y=pressure$pressure, main="Pressure ~ Temperature")
#generate a smoothed line of the points

par(mfrow=c(1, 2))  
# divide graph area in 2 columns

boxplot(pressure$temperature, main="Temperature") 
# box plot for 'temperature'
boxplot.stats(pressure$temperature)$out 
# display outliers

boxplot(pressure$pressure, main="Pressure")  
# box plot for 'distance'
boxplot.stats(pressure$pressure)$out 
# display outliers

plot(density(pressure$temperature), main="Density Plot: Temperature")  # density plot for 'Temperature'
plot(density(pressure$pressure), main="Density Plot: Pressure") # density plot for 'Pressure'
par(mfrow=c(1, 1))  #change the two side by side graph into individual graphs

pressure.lm <- lm(temperature ~ pressure, data=pressure)  # build linear regression model on full data
print(pressure.lm)

summary(pressure.lm)#inspect the results

plot(pressure$temperature, pressure$pressure)
#visualize the regression line on a scatter plot of the pressure data.
abline(pressure.lm)

pressure.res <- resid(pressure.lm)#compute the residuals

plot(pressure$pressure, pressure.res, ylab="Residuals", xlab="Pressure", main="Pressure Linear Model") 
abline(0, 0)#generate the horizon

plot(density(pressure.res), main="Pressure Density Plot: residuals")

plot(pressure.lm) # each plot individually
par(mfrow=c(2,2)) #generate a 2x2 grid of plots format
plot(pressure.lm)
par(mfrow=c(1, 1))  #change the two side by side graph into individual graphs

pressure.stdres = rstandard(pressure.lm)
qqnorm(pressure.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Pressure dataset") #create the standardized residuals
qqline(pressure.stdres)#create a normal probability plot (Q-Q plot)

mod<-lm(temperature ~ pressure, data=pressure)
mean(mod$residuals)

mod.lm <- lm(temperature ~ pressure, data=pressure)
cor.test(pressure$temperature, mod.lm$residuals)#do the correlation test

#a.Write an equation that describes the linear model you have fitted
#Fine:y = pressure(0.3797)*x + intercept(132.7907)
#b.Explain why the p-values for the variable temperature and the overall F test are so similar for this model.
#F test means two-sample t test, and p-value stands two-sample p test, and they are all in needs of an enormous amount of sample(volume of sample), so they all
#have a similar data for that module
#c.comment on what you saw from the previous parts. Is the model appropriate?
#(1)For the correlation test, we can see the p-value = 0.002461 < 0.05, which means 
#there is an evidence of correlation and they are correlated, the model is appropriate
#(2)For the QQ plot I found that the smooth line fit the scatter point graph properly, which means the 
#residuals are normally distributed, the model is appropriate 
#(3)For the graph:Scale-Location and Residuals vs Leverage , we can draw a conclusion that
#the scatter points are mostly fit in a straight line, means the residuals are homoscedastic,
#the model is appropriate
#(4)According to the data in multiple R squared and adjusted R square, it is clear to see
#that they have a small difference between each other, that means the model is appropriate
#(5)judging the cook distance in the graph:Scale-Location and Residuals vs Leverage, we can
#see that the distances between each points are close

