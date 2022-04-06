Lab1 <- read.csv(file="Lab1.csv", header=TRUE)                  #read in the Lab1.csv file
summary(Lab1$EARN)                                              #Display summary statistics for the variable EARN
table(Lab1$Job.class)                                           #Display frequencies of the variable Job.class
table.prop <- table(Lab1$EDUC, Lab1$Gender, Lab1$Job.class)     #create a proportional table for a three-way cross-tabulation
prop.table(table.prop)                                          #Display a three-way cross-tabulation of the proportions of variables Educ, Gender and Job.Class
hist(Lab1$EARN)                                                 #Create a basic histogram of the variable EARN
boxplot(Lab1$EARN~Lab1$Job.class)                               #Create a basic boxplot of the variable EARN by Job Class
Lab1$EARN_10000 = Lab1$EARN/10000                               #Create a new variable EARN_10000 that is equal to Earnings divided by 10,000
plot(Lab1$EARN_10000, Lab1$AGE)                                 #Create a scatterplot with EARN_10000 on the x axis and AGE on the Y axis
