#========================================================#
# STAT 6021: Linear Models for Data Science              |
# R tutorial for learning unit 1                         |
#========================================================#
#                                                        |
# In previous classes you learned about the central      |
# limit theorem, which establishes that the sample mean  |
# of a large sample follows a normal distribution. This  |
# demonstration will use simulation to explore the       |
# central limit theorem.                                 |
#                                                        |
# The following are two population distributions with    |
# the same mean and variance but very different shapes   |
#                                                        |
# -- A N(mu=3, sig2=6) distribution has mean mu=3 three  |
#    and variance sig2=6. It is symmetric and            |
#    bell-shaped.
#                                                        |
# -- A chi-square distribution with df=3 degrees of      |
#    freedom has mean mu=3 three and variance sig2=6. It |
#    is right-skewed                                     |
#                                                        | 
# By repeatedly simulating samples drawn from these      |
# populations, we realize a process of *hypthetical      |
# repeated sampling*, which allows us to understand the  |
# central limit theorem and make sense of the            |
# theoretical properties of statistical procedures       |
# derived from it.                                       |
#                                                        |
# The following code simulates a sample of size n=10,    |
# drawn from a N(mu=3, sig2=6) distribution              |

n <- 10
x <- rnorm(n, mean=3, sd=sqrt(6))

# The following code simulates a sample of size n=10,    |
# drawn from a chi-square distribution with df=3 degrees |
# of freedom                                             |

n <- 10
x <- rchisq(n, df=3)

# In what follows, these samples are simulated 500       |
# times. Each time, statistical summarization and other  |
# procedures are applied to the sample. After the        |
# simulation is complete, the results of the procedures  |
# are analyzed to understand their properties.           |
#                                                        |
# Here we explore the following properties of            |
# statistical procedures:                                |
#                                                        |
# -- The mean, variance, and shape of the sampling       |
#    distribution of the sample mean                     |
# -- The coverage probability of a t confidence interval |
#                                                        |
# We explore the following configurations of the         |
# sampling context:                                      |
#                                                        |
# -- Bell-shaped vs skewed distributions                 |
# -- Small sample size (n=5) vs large sample size (n=25) |
#                                                        |
# The following variables are referenced in all          |
# configurations:                                        |

mu <- 3           # population mean                      |
sig2 <- 6         # population variance                  |
n.rep <- 500      # stores the number of repetitions of  |
                  # the simulation.                      |
alpha <- 0.05     # defines the stated confidence level  |
                  # of the confidence interval           |

# -------------------------------------------------------+
# Configuration 1: Bell-shaped distribution, small       |
# sample size                                            |
# -------------------------------------------------------+

n <- 5
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

# Theoretical and simulated variance of the sampling     |
# distribution                                           |

var.x.bar.theory <- sig2/n
var.x.bar.theory

var.x.bar.sim <- var(x.bar.sample)
var.x.bar.sim

# Q2: Does the sampling distribtion of the sample mean   |
#     have an approximate bell shape?                    |

hist(x.bar.sample, breaks=15, xlab="sample mean", main="Simulated sampling distribution of the sample mean")

# Q3: Does the t confidence interval cover the true mean |
#     at the stated confidence level?                    |

table(ci.sample) / n.rep

# -------------------------------------------------------+
# Configuration 2: Skewed distribution, small sample     |
# size                                                   |
# -------------------------------------------------------+

n <- 5
t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
x.bar.sample <- numeric(length=n.rep)
ci.sample <- logical(length=n.rep)

for (rep in 1:n.rep) {
	x <- rchisq(n, df=3)
	x.bar <- mean(x)
	m.err <- t.crit*sd(x)/sqrt(n)
	x.bar.sample[rep] <- mean(x)
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

# Theoretical and simulated variance of the sampling     |
# distribution                                           |

var.x.bar.theory <- sig2/n
var.x.bar.theory

var.x.bar.sim <- var(x.bar.sample)
var.x.bar.sim

# Q2: Does the sampling distribtion of the sample mean   |
#     have an approximate bell shape?                    |

hist(x.bar.sample, breaks=15, xlab="sample mean", main="Simulated sampling distribution of the sample mean")

# Q3: Does the t confidence interval cover the true mean |
#     at the stated confidence level?                    |

table(ci.sample) / n.rep

# -------------------------------------------------------+
# Configuration 3: Bell-shaped distribution, large       |
# sample size                                            |
# -------------------------------------------------------+

n <- 25
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

# Theoretical and simulated variance of the sampling     |
# distribution                                           |

var.x.bar.theory <- sig2/n
var.x.bar.theory

var.x.bar.sim <- var(x.bar.sample)
var.x.bar.sim

# Q2: Does the sampling distribtion of the sample mean   |
#     have an approximate bell shape?                    |

hist(x.bar.sample, breaks=15, xlab="sample mean", main="Simulated sampling distribution of the sample mean")

# Q3: Does the t confidence interval cover the true mean |
#     at the stated confidence level?                    |

table(ci.sample) / n.rep

# -------------------------------------------------------+
# Configuration 4: Skewed distribution, large sample     |
# size                                                   |
# -------------------------------------------------------+

n <- 25
t.crit <- qt(alpha/2, df=n-1, lower.tail=FALSE)
x.bar.sample <- numeric(length=n.rep)
ci.sample <- logical(length=n.rep)
for (rep in 1:n.rep) {
	x <- rchisq(n, df=3)
	x.bar <- mean(x)
	m.err <- t.crit*sd(x)/sqrt(n)
	x.bar.sample[rep] <- mean(x)
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

# Theoretical and simulated variance of the sampling     |
# distribution                                           |

var.x.bar.theory <- sig2/n
var.x.bar.theory

var.x.bar.sim <- var(x.bar.sample)
var.x.bar.sim

# Q2: Does the sampling distribtion of the sample mean   |
#     have an approximate bell shape?                    |

hist(x.bar.sample, breaks=15, xlab="sample mean", main="Simulated sampling distribution of the sample mean")

# Q3: Does the t confidence interval cover the true mean |
#     at the stated confidence level?                    |

table(ci.sample) / n.rep


