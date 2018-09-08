#========================================================#
# STAT 6021: Linear Models for Data Science              |
# Hands-on team laboratory exercise for unit 2           |
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
# -------------------------------------------------------+
# Problem 1:                                             |
# -------------------------------------------------------+
#                                                        |
# Use the generic simple linear regression data given    |
# below to complete the following parts.                 |

x <- c(81, 92, 64, 38, 49, 61, 59, 48, 81, 33, 25, 28, 45, 44, 75, 62, 61, 37, 50, 23, 55, 50, 13, 69, 31)
y <- c(40, 25, 22, 22, 18, 35, 33, 18, 46, 13, 22, 19, 21, 5, 40, 15, 10, 15, 18, 29, 21, 24, 23, 15, 20)

# Part A: Fit a simple linear regression model to these  |
# data. Obtain the fitted values and residuals.          |




# Part B: Verify that the residuals satisfy Property 1   |
# listed on p. 20 of the textbook.                       |


# Part C: Verify that the fitted values satisfy Property |
# 2 listed on p. 20 of the textbook.                     |
#                                                        |
# Part D: Verify that the fitted regression line         |
# satisfies Property 3 listed on p. 20 of the textbook.  |
#                                                        |
# Part E: Verify that the residuals satisfy Property 4   |
# listed on p. 20 of the textbook.                       |
#                                                        |
# Part F: Verify that the fitted values and residuals    |
# satisfy Property 5 listed on p. 20 of the textbook.    |
#                                                        |
# -------------------------------------------------------+
# Problem 2:                                             |
# -------------------------------------------------------+
#                                                        |
# This problem will use simulation to explore the        |
# theoretical properties of linear regression methods.   |
#                                                        |
# Start by defining a generic regressor with values      |
# specified at 1.0, 1.5, 2.0, ..., 9.5, 10.0.            |

x <- seq(from=1, to=10, by=0.5)

# Next, randomly generate response values from the model | 
#   y = 50 + 10 x + epsilon where epsilon ~ NID(0, 16)   |
# This may be achieved using the "rnorm" function, as in |
# the following code:                                    |

n <- length(x)
beta0 <- 50
beta1 <- 10
sig2 <- 16
y <- beta0 + beta1*x + rnorm(n, mean=0, sd = sqrt(sig2))

# By repeatedly simulating regression data this way, we  |
# realize a process of *hypthetical repeated sampling*,  |
# which allows us to make sense of the theoretical       |
# properties of statistical procedures.                  |

# Complete the task in each part listed below based on   |
# 500 simulations of the regression model.               |
#                                                        |
# Part A: At each repetition of the simulation,          |
# calculate and record the values of the fitted          |
# regression coefficients for intercept and slope. Once  |
# you have completed 500 simulations, calculate the      |
# sample mean and sample variance of each set of values  |
# that you collected. Are the numerical values of those  |
# sample means and sample variances consistent with the  |
# numerical values that you would obtain from the        |
# expected values and variance formulas of the least-    |
# squares estimates for intercept and slope?             |
#                                                        |
# Part B: At each repetition of the simulation,          |
# calculate and record the values of the upper and lower |
# bounds of a 95% confidence interval for slope; record  |
# whether the confidence interval surrounds the true     |
# value of the slope, beta1 = 10. Once you have          |
# completed 500 simulations, calculate the relative      |
# frequency that the confidence interval surrounds the   |
# true value of the slope. Is the numerical value of     |
# that relative frequency consistent with what you would |
# expect? Explain.                                       |
#                                                        |
# Part C: At each repetition of the simulation,          |
# calculate and record the values of the upper and lower |
# bounds of a 95% confidence interval for error          |
# variance; record whether the confidence interval       |
# surrounds the true value of the error variance,        |
# sig2 = 16. Once you have completed 500 simulations,    |
# calculate the relative frequency that the confidence   |
# interval surrounds the true value of the error         |
# variance. Is the numerical value of that relative      |
# frequency consistent with what you would expect?       |
# Explain.                                               |
#                                                        |
# Part D: At each repetition of the simulation, carry    |
# out a t test for the hypotheses                        |
#             H0: beta1 = 10 vs H0: beta1 ≠ 10           |
# controlling for type I error at level 0.05; record     |
# whether the test rejects or fails to reject H0. Once   |
# you have completed 500 simulations, calculate the      |
# relative frequency that the test rejects H0.           |
#                                                        |
# Part E: At each repetition of the simulation, carry    |
# out a t test for the hypotheses                        |
#             H0: beta1 = 9 vs H0: beta1 ≠ 9             |
# controlling for type I error at level 0.05; record     |
# whether the test rejects or fails to reject H0. Once   |
# you have completed 500 simulations, propose an         |
# approximate value of the power of the test when the    |
# true value of the slope is beta1=10.                   |
#                                                        |
# Part F: Repeat Parts A-E, but this time specify the    |
# regressor values at 1.0, 2.0, ..., 10.0.               |

x <- seq(from=1, to=10, by=1.0)

# Then, answer whether each of the following sumulation  |
# summaries                                              |
#    APPRECIABLY CHANGED or DID NOT APPRECIABLY CHANGE   |
# from the original version of simulation and explain    |
# why you could have expected that to be the case.       |
# (i.) The sample means of the simulated regression      |
# coefficients.                                          |
# (ii.) The sample variances of the simulated regression |
# coefficients.                                          |
# (iii.) The relative frequency that the confidence      |
# interval surrounds the true value of the slope.        |
# (iv.) The relative frequency that the confidence       |
# interval surrounds the true value of the error         |
# variance.                                              |
# (v.) The relative frequency that the test rejects H0.  |
# (vi.) The power of the test when the true value of the |
# slope is beta1=10.                                     |