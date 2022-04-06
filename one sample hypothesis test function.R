#We can make a function where we enter the values needed 
#and get a printout of our results for our desired test.

#code for a one-sample hypothesis test function
OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), 
              input=paste("alpha=", alpha, "; mu=", mu, "; n=", n, "; x-bar=", x_bar, "; sd=", sd),    
              calculations=paste("se=", se, "; test statistic", test_stat),  
              conclusion=paste("At a significance level of", alpha, "the p-value of", p_val, "is", sig))
  #return the list
  return( ret )
}
OneSampTest("z", "right", 0.05, 0, 10, 3, 2)