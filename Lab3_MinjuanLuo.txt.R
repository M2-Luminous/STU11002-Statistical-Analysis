#1)
#a.Simulate a population of 5,000 with a variable from a Normal
#  (5,4) distribution and a population of 5,000 with a variable 
#  from an Exponential(1.2) distribution [use the command rexp(n,lambda) ]
#b.Draw samples of size 40 and size 100 from each population.
#2)
#a.Choose a confidence level and calculate confidence intervals 
#  for the mean of the sample of size 40 from each distribution, using a) 
#  the Z-score and the known population SD, and b) the t-distribution and the sample SD.
#3)
#a.Use R to plot graphs to help you assess the normality of each population,
#  as well as each of the samples of size 40. 
#b.Would you trust/rely on the confidence intervals you calculated in part 2? 
#  Why/Why not?
x0 <- rnorm(5000, 5, 4)#a population of 5,000 with a variable from a Normal(5,4) distribution
x1 <- rexp(5000, 1.2)# variable from an Exponential(1.2) distribution

x2 <- sample(x0,40)
x3 <- sample(x0,100)#Draw samples of size 40 and size 100 from normal distribution population
x4 <- sample(x1,40)
x5 <- sample(x1,100)#Draw samples of size 40 and size 100 from exponential distribution population

n2 <- length(x2)
mean2 <- mean(x2)
sd2 <- sd(x2)# record the size, mean and sd of x2

n4 <- length(x4)
mean4 <- mean(x4)
sd4 <- sd(x4)# record the size, mean and sd of x4

Z_score <- qnorm(0.975)#Calculate Z-score for 95% CI
t_score2 <- qt(0.975, n2-1)#Calculate critical t-value for 95% CI and x2
t_score4 <- qt(0.975, n4-1)#Calculate critical t-value for 95% CI and x4

se_kn2 <- sd2/sqrt(n2)
se_kn4 <- sd4/sqrt(n4)

left_Z95_kn2 <- mean2-Z_score*se_kn2
right_z95_kn2 <- mean2+Z_score*se_kn2#confidence interval for the z score for x0
left_Z95_kn4 <- mean4-Z_score*se_kn4
right_z95_kn4 <- mean4+Z_score*se_kn4#confidence interval for the z score for x1

left_t95_kn2 <- mean2-t_score2*se_kn2
right_t95_kn2 <- mean2+t_score2*se_kn2#confidence interval for the t score for x0
left_t95_kn4 <- mean4-t_score4*se_kn4
right_t95_kn4 <- mean4+t_score4*se_kn4#confidence interval for the t score for x0

paste("Z-distribution2 w/known pop sd:", left_Z95_kn2, right_z95_kn2)
paste("Z-distribution4 w/known pop sd:", left_Z95_kn4, right_z95_kn4)
paste("t-distribution2 w/known pop sd:", left_t95_kn2, right_t95_kn2)
paste("t-distribution4 w/known pop sd:", left_t95_kn4, right_t95_kn4)

qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit0 <- seq(min(x0), max(x0), length = 40) 
yfit0 <- dnorm(xfit0, mean = mean(x0), sd = sd(x0))
lines(xfit0, yfit0)#original plot

qqnorm(x2)
qqline(x2)
hist(x2, freq = FALSE)
xfit2 <- seq(min(x2), max(x2), length = 40) 
yfit2 <- dnorm(xfit2, mean = mean(x2), sd = sd(x2))
lines(xfit2, yfit2)#make a qqplot and a histogram with normal density curve for x0

qqnorm(x4)
qqline(x4)
hist(x4, freq = FALSE)
xfit4 <- seq(min(x4), max(x4), length = 40) 
yfit4 <- dnorm(xfit4, mean = mean(x4), sd = sd(x4))
lines(xfit4, yfit4)#make a qqplot and a histogram with normal density curve for x1

#I think I won't trust the confidence intervals that I calculated in part 2 because they
#only include 40 samples out of 5000 objects, which means there will be a large difference
#appear if I choose 100 samples instead or 1000 samples instead. The samples are not
#big enough and according to the normal table and t table, a small amount of sample
#may cause an incompleteness.What's more, there exists a huge difference between original 
#plot and sampled plot, which even make me more confidence about my view