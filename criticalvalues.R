x0 <- rnorm(1000,3,5)                   #create a vector x0 of length 1000, 
                                        #drawn from a normal distribution 
                                        #with mean 3, sd 5
N0 <- length(x0)                        # record the size, mean and sd of x0
mean0 <- mean(x0)
sd0 <- sd(x0)

x1 <- sample(x0,30)                     #Take a random sample of size 30 from x0
n1 <- length(x1)
mean1 <- mean(x1)
sd1 <- sd(x1)

se_kn <- sd0/sqrt(n1)                   #calculate the standard error of mean1 
se_unkn <- sd1/sqrt(n1)                 #using known and estimated sd0

Z1 <- qnorm(0.975)
Z2 <- qnorm(0.995)
Z1
Z2

Z_score <- qnorm(0.975)                 #Calculate Z-score for 95% CI
t_score <- qt(0.975, n1-1)              #Calculate critical t-value for 95% CI and x1

left_Z95_kn <- mean1-Z_score*se_kn      #z distribution, known population sd
right_z95_kn <- mean1+Z_score*se_kn     
left_Z95_unkn <- mean1-Z_score*se_unkn  #z distribution, unknown pupulation sd
right_z95_unkn <- mean1+Z_score*se_unkn

left_t95_kn <- mean1-t_score*se_kn      #t distribution, known population sd
right_t95_kn <- mean1+t_score*se_kn
left_t95_unkn <- mean1-t_score*se_unkn  #t distribution, unknown population sd
right_t95_unkn <- mean1+t_score*se_unkn

paste("var: x0", "mean:", mean0, "sd:", sd0)
paste("var: x1", "mean:", mean1, "sd:", sd1, "se w/known pop sd:", se_kn, "se w/unknown pop sd:", se_unkn)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn, right_z95_kn)
paste("Z-distribution w/unknown pop sd:", left_Z95_unkn, right_z95_unkn)
paste("t-distribution w/known pop sd:", left_t95_kn, right_t95_kn)
paste("t-distribution w/unknown pop sd:", left_t95_kn, right_t95_unkn)          #print all the calculated result

qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit <- seq(min(x0), max(x0), length = 40) 
yfit <- dnorm(xfit, mean = mean(x0), sd = sd(x0))
lines(xfit, yfit)                                                               ##make a qqplot and a histogram with normal density curve for x0


