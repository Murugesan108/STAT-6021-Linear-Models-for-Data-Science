#========================================================#
# STAT 6021: Linear Models for Data Science              |
# Hands-on team laboratory exercise for unit 1           |
#========================================================#
#                                                        |
# Submit one solution per team. Use the template         |
# provided in the file "STAT 6021 unit 02 team xx.R" to  |
# code your solution and write your answers in the       |
# spaces that are provided. Answers are to be written as |
# comments, and clearly indicated so the grader can      |
# easily find them. Be sure to include all coding        |
# statements that are necessary to run your code and     |
# either reproduce your solution or obtain a close       |
# approximation to it (e.g., in the case of a            |
# simulation). To submit your solution, change the       |
# filename by replacing the characters "xx" with your    |
# team label (but make no other changes to the           |
# filename), and submit the file to the course web page  |
# in Collab, using the "Assignments" tool.               |
#                                                        |
# In this exercise you will use simulation to explore    |
# the sampling distribution of the sample median,        |
# compared with that of the sample mean.                 |
#                                                        |
# In each problem listed below, complete the task based  |
# on 500 simulations, which is to be carried out under   |
# one of the first two sampling configurations explored  |
# in the tutorial.                                       |
#                                                        |
# -------------------------------------------------------+
# Problem 1: Bell-shaped distribution, small sample size |
# -------------------------------------------------------+
#                                                        |
# Here, each sample is to be simulated using the         |
# following code                                         |


# of the confidence interval           |


# At each repetition of the simulation, calculate and    |
# record the values of the sample mean and sample        |
# median. Once you have completed 500 simulations, use   |
# the results of your simulation to answer the questions |
# in the parts below.                                    |


#                                                        |
# Part A: Is the mean of the sampling distribution of    |
# the sample median about the same as the population     |
# median?
mu <- 3           # population mean                      |
sig2 <- 6         # population variance                  |
n.rep <- 500      # stores the number of repetitions of  |
# the simulation.                      |
alpha <- 0.05  
M<-3

n <- 5
t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
x.bar.sample <- numeric(length=n.rep)
ci.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
  x <- rnorm(n, mean=3, sd=sqrt(6))
  x.bar <- median(x)
  m.err <- t.crit*sd(x)/sqrt(n)
  x.bar.sample[rep] <- median(x)
  ci.sample[rep] <- (abs(x.bar - M) <= m.err)
}


mean.x.bar.sim <- mean(x.bar.sample)
mean.x.bar.sim
#[1] 2.982338

pop.median <- qnorm(0.5, mean=3, sd=sqrt(6))
pop.median
# [1] 3

#                                                        |
# Note: The median of a N(mu=3, sig2=6) is M = 3, as is  |
# confirmed by the following code                        |


# Part B: How does the variance of the the sampling      |
# distribution of the sample median compare to that of   |
# the sample mean?             

n <- 5
t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
y.bar.sample <- numeric(length=n.rep)
c.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
  y <- rnorm(n, mean=3, sd=sqrt(6))
  y.bar <- mean(y)
  n.err <- t.crit*sd(y)/sqrt(n)
  y.bar.sample[rep] <- mean(y)
}


var.x.bar.sim <- var(x.bar.sample)
var.x.bar.sim
# [1] 1.598399  
# The variance of the sampling distribution of the smaple median is 1.598399 

var.y.bar.sim <- var(y.bar.sample)
var.y.bar.sim
# [1] 1.281967
# The variance of the sampling distribution of the sample mean is 1.281967 


#                                                        |
# Part C: What value for margin of error is such that    |
# the relative frequency at which the absolute           |
# difference between the sample median and population    |
# median is smaller than that margin of error is about   |
# 0.95?                                                  |
#  
n <- 5
x.bar.sample <- numeric(length=n.rep)
abs.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
  x <- rnorm(n, mean=3, sd=sqrt(6))
  x.bar <- median(x)
  x.bar.sample[rep] <- median(x)
  abs.sample[rep] <- (abs(x.bar - M))
}
mr = quantile(abs.sample, .95)  
mr
# 95% 
# 2.414505 
#The margin of error is 2.414505 
# -------------------------------------------------------+
# Problem 2: Skewed distribution, small sample size      |
# -------------------------------------------------------+
#                                                        |
# Here, each sample is to be simulated using the         |
# following code                                         |

n <- 5
p <- rchisq(n, df=3)
m <- qchisq(0.5, df=3)
t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
p.bar.sample <- numeric(length=n.rep)
p.bar2.sample <- numeric(length=n.rep)
p.sample1 <- logical(length=n.rep)
p.sample2 <- logical(length=n.rep)
for (rep in 1:n.rep) {
  p <- rchisq(n, df=3)
  p.bar <- mean(p)
  p.bar2 <- median(p)
  mp.err <- t.crit*sd(x)/sqrt(n)
  p.bar.sample[rep] <- mean(p)
  p.bar2.sample[rep] <- median(p)
  p.sample1[rep] <- (abs(p.bar - mu) <= mp.err)
  p.sample2[rep] <- (abs(p.bar2 - m) <= mp.err)
}

# At each repetition of the simulation, calculate and    |
# record the values of the sample mean and sample        |
# median. Once you have completed 500 simulations, use   |
# the results of your simulation to answer the questions |
# in the parts below.                                    |
#                                                        |
# Part A: Is the mean of the sampling distribution of    |
# the sample median about the same as the population     |
# median?                                                |
#                                                        |
# Note: The median of a chi-square distribution with     |
# df=3 degrees of freedom is M = 2.37, as is confirmed   |
# by the following code                                  |

pop.median <- qchisq(0.5, df=3)
pop.median
# [1] 2.365974
mean(p.bar2.sample)
# [1] 2.61018


# Part B: How does the variance of the the sampling      |
# distribution of the sample median compare to that of   |
# the sample mean?

var.sample.medain <- var(p.bar2.sample)
var.sample.medain
# [1] 1.383803
var.sample.mean <- var(p.bar.sample)
var.sample.mean 
#[1] 1.27811  

# Part C: What value for margin of error is such that    |
# the relative frequency at which the absolute           |
# difference between the sample median and population    |
# median is smaller than that margin of error is about   |
# 0.95?                                                  |

p.bar.sample <- numeric(length=n.rep)
abs.p.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
  p <- rchisq(n, df=3)
  p.bar <- median(x)
  p.bar.sample[rep] <- median(p)
  abs.p.sample[rep] <- (abs(p.bar - m))
}
p.mr = quantile(abs.p.sample, .95)  
p.mr
# 95% 
# 1.023792 