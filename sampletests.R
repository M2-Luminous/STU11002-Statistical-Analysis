#1)Use R Studio to conduct one-sample hypothesis tests using the Z distribution
#2)Use R Studio to conduct one-sample hypothesis tests using the t distribution
#3)Use R Studio to conduct one-sample hypothesis tests for proportions
#4)Use R Studio to conduct chi square tests. 

#One-sample Z-tests
# for a one-sample Z test we need a null hypothesis, 
#a significance level, and a test statistic
Sample <-rnorm(100,0,1) #simulate some data(normal distribution)

#null hypothesis = sample++

#significance value = check the z table, which is already done in the program

n <- length(Sample) #size of the sample
x_bar <- mean(Sample) #mean of the sample
sd <- sd(Sample) #this is the sample SD, in this case we know the population SD is 1 
                 #since we simulated our data

# lets define mu (the mean of the population under the null hypothesis
mu <- 0

#calculate the test statistic for a Z-distribution
test_stat <- (x_bar-mu)/(sd/sqrt(n))

#get the p-value for this test statistic
p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) # for two tailed
p_val <-  pnorm(test_stat , lower.tail=TRUE) # for left-tailed
p_val <-  pnorm(test_stat , lower.tail=FALSE) # for right tailed


#One-Sample Z-test for proportions
#The only change needed to use the code above for a proportion is
#in your calculation of standard error.

Propdat <- rbinom(100, 1, 0.3) # simulate some data(binomial distribution)
n <- length(Propdat) #size of the sample
prop <- mean(Propdat) #proportion of 1s in the sample
prop <- sum( ( Propdat == 1 ) / n ) #could also do it this way
#calculate sd for a z-test of proportions
sd = sqrt((prop)*(1-prop)/n)

#One-Sample t-tests
#Similar code can be used for a one-sample t-test, 
#the difference being that the df must be specified, 
#and the t-distribution is used rather than the Z-distribution.

df <- n-1 #define df
p_val <- 2*pt(abs(test_stat), df, lower.tail=FALSE) #two-tailed
p_val <- pt(test_stat , df,lower.tail=TRUE) #left-tailed
p_val <- pt(test_stat , df, lower.tail=FALSE) # right-tailed

#We could also use the t.test function in the package {stats}
t.test(Sample) #two-tailed
t.test(Sample, alternative = "less") #left-tailed
t.test(Sample, alternative = "greater") #right-tailed

#Chi Square tests
#The following code will make a new dataset ¡°dat¡±
#which contains the standard iris flower data and a new variable size, 
#which records if the sepal length of each observation is above or below the median.

dat <- iris
dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), "small", "big")
#Next, we can create a contingency table from size and species
Tbl <- table(dat$size, dat$Species)
Tbl #look at the table
#Finally, we can conduct a chi square test.
chisq.test(Tbl) 
