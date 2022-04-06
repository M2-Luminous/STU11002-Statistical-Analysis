#Part A
Sample1 <-rnorm(100,4,5)
n1 <- length(Sample1)#get the length for the first sample
x_bar1 <- mean(Sample1)#get the mean
sd1 <- sd(Sample1)#get the standard deviation

Sample2 <-rnorm(80,3.5,2)
n2 <- length(Sample2)#get the length for the second sample
x_bar2 <- mean(Sample2)#get the mean
sd2 <- sd(Sample2)#get the standard deviation

df <- (n1 + n2)-2
sd <- sqrt(((n1-1)*(sd1^2) + (n2-1)*(sd2^2)) / df)#calculate the standard deviation
se <- sd* sqrt(1/n1 + 1/n2)#calculate the standard error
test_stat <- (abs(x_bar1 - x_bar2) - 0 / se) #calculate the test statistic
p_val <- 2*pt(abs(test_stat), df, lower.tail=FALSE) #two-tailed
p_val <- pt(test_stat , df,lower.tail=TRUE) #left-tailed
p_val <- pt(test_stat , df, lower.tail=FALSE) # right-tailed

t.test(Sample1, Sample2) #two-tailed

#Part B
Prop_1 <- 0.4#load the data:40%
n_1 <- 100#load the size: 100
Prop_2 <- 0.5#load the data:60%
n_2 <- 50#load the size: 50
H0 <- 0
Prop_0 <- ((Prop_1*n_1) + (Prop_2*n_2))/(n_1 + n_2)
se1 <- sqrt(Prop_0*(1-Prop_0)*(1/n_1 + 1/n_2))#calculate the standard error
test_stat1 <- ((abs(Prop_1 - Prop_2) - H0) / se1)#calculate the test statistic
#if failed the null hypothesis, confidence intervals equals the difference in means
CI1 <- (Prop_1 - Prop_2 + 1.96*se1)#right boundary
CI2 <- (Prop_1 - Prop_2 - 1.96*se1)#left boundary
p_val <- 2*pnorm(abs(test_stat1), lower.tail=FALSE) #two-tailed

#Part C
Lab4 <- read.csv(file="survey.csv", header=TRUE)
Tbl <- table(Lab4$Smoke, Lab4$Exer)
Tb1
chisq.test(Tbl) 
#Q1:chi-square is highly sensitive to sample size.As sample size increases
#, absolute differences become a smaller and smaller proportion of the expected
#value.A reasonably strong association may not come up as significant if
#the sample size is small. In large samples, we may find statistical significance
#when the findings are small and uninteresting
#Q2:H0 = exercise has the relation with smoking, HA =/= exercise has no relation with smoking
#Q3:using the significance level of 0.05, comparing to the p-value which is 
#0.4828, I can then draw the conclusion that the chi-square have a large differences
#with the correct number