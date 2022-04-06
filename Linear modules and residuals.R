head(cars)
cor(cars$speed, cars$dist)#相关性
plot(cars$speed, cars$dist)#散点图

#A smoothed line of the points might help to visualize a linear relationship.
#画一条近似直线来观察数据变化
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

#A box plot can help us check for outliers

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed") # box plot for 'speed'
boxplot.stats(cars$speed)$out # display outliers
boxplot(cars$dist, main="Distance")  # box plot for 'distance'
boxplot.stats(cars$dist)$out # display outliers

#We can also look at the density of the variables. 

plot(density(cars$speed), main="Density Plot: Speed")  # density plot for 'speed'
plot(density(cars$dist), main="Density Plot: Distance")
par(mfrow=c(1, 1))  # back to 1 plot

cars.lm <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(cars.lm)
#creating a linear model of the distance traveled by car in terms of its speed

#And now we can inspect the results
summary(cars.lm)

#Next, we can visualize our regression line on a scatter plot of our data.
plot(cars$speed, cars$dist)
abline(cars.lm)

#Next, we can compute the residuals
cars.res <- resid(cars.lm)

#We can plot the residuals against the observed values
plot(cars$dist, cars.res, ylab="Residuals", xlab="Distance", main="Cars Linear Model") 
abline(0, 0)# the horizon

#We can inspect the residuals density
plot(density(cars.res), main="Density Plot: residuals")

#We can also see a number of other plots with a single command
plot(cars.lm) # each plot individually
par(mfrow=c(2,2)) # 2x2 grid of plots
plot(cars.lm)
par(mfrow=c(1, 1))  # back to 1 plot

#If we wanted to, we could create the standardized residuals and create a normal probability plot (Q-Q plot) manually.
cars.stdres = rstandard(cars.lm)
qqnorm(cars.stdres, 
ylab="Standardized Residuals", 
xlab="Normal Scores", 
main="Cars dataset") 
qqline(cars.stdres)