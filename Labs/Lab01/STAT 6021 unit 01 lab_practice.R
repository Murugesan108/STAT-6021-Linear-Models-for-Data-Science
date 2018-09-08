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

n <- 5
mu <- 3
n.rep <- 500
alpha <- 0.05


t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
x.bar.sample <- numeric(length=n.rep)
ci.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
  x <- rnorm(n, mean=3, sd=sqrt(6))
  x.bar <- mean(x)
  m.err <- t.crit*sd(x)/sqrt(n)
  x.bar.sample[rep] <- median(x)
  ci.sample[rep] <- (abs(x.bar - mu) <= m.err)
}

# Q1: Are the mean and variance of the sampling          |
#     distribution of the sample mean consistent with    |
#     the theoretical formulas of the central limit      |
#     theorem?                                           |
#                                                        |
# Theoretical and simulated mean of the sampling         |
# distribution                                           |

mean.x.bar.theory <- mu
mean.x.bar.theory

mean.x.bar.sim <- mean(x.bar.sample)
mean.x.bar.sim


t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
x.bar.sample <- numeric(length=n.rep)
ci.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
  x <- rnorm(n, mean=3, sd=sqrt(6))
  x.bar <- mean(x)
  m.err <- t.crit*sd(x)/sqrt(n)
  x.bar.sample[rep] <- mean(x)
  ci.sample[rep] <- (abs(x.bar - mu) <= m.err)
}


var.x.bar.sim <- var(x.bar.sample)
var.x.bar.sim


## PART C ##



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
# Note: The median of a N(mu=3, sig2=6) is M = 3, as is  |
# confirmed by the following code                        |

pop.median <- qnorm(0.5, mean=3, sd=sqrt(6))
pop.median

# Part B: How does the variance of the the sampling      |
# distribution of the sample median compare to that of   |
# the sample mean?                                       |
#                                                        |
# Part C: What value for margin of error is such that    |
# the relative frequency at which the absolute           |
# difference between the sample median and population    |
# median is smaller than that margin of error is about   |
# 0.95?                                                  |
#                                                        |
# -------------------------------------------------------+
# Problem 2: Skewed distribution, small sample size      |
# -------------------------------------------------------+
#                                                        |
# Here, each sample is to be simulated using the         |
# following code                                         |

n <- 5
x <- rchisq(n, df=3)

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

# Part B: How does the variance of the the sampling      |
# distribution of the sample median compare to that of   |
# the sample mean?                                       |
#                                                        |
# Part C: What value for margin of error is such that    |
# the relative frequency at which the absolute           |
# difference between the sample median and population    |
# median is smaller than that margin of error is about   |
# 0.95?                                                  |

