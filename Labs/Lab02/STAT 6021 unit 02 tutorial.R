#========================================================#
# STAT 6021: Linear Models for Data Science              |
# R tutorial for learning unit 2                         |
#========================================================#
#                                                        |
#========================================================#
# The rocket propellant data                             |
#========================================================#
#                                                        |
# The following code is used to read in the data         |

DataDirectory <- "C:\\Users\\arvra\\Documents\\UVa files\\Classes\\Fall_18\\STAT Intro to linear models\\Labs\\Lab02\\Data"
setwd(DataDirectory)
rocket.data <- read.table(file = "rocket.txt")
names(rocket.data) <- c("strength", "age")

# -------------------------------------------------------+
# Example 2.1: Rocket propellant data, simple linear     |
# regression analysis                                    |
# -------------------------------------------------------+
#                                                        |
# Start by assigning the response, y, and regressor, x,  |
# variables:                                             |

y <- rocket.data$strength
x <- rocket.data$age

# The "plot" function may be used to generate a scatter  |
# diagram.                                               |

plot(x, y, pch=16, cex=1, xlab="age of propellant", ylab="shear strength", main = "Scatter diagram of y by x")

# The "mean" function may be used to calculate x-bar and |
# y-bar. The "sum" function, combined with standard      |
# arithmetic operators, may be used to calculate Sxx and |
# Sxy.                                                   |

y.bar <- mean(y)
x.bar <- mean(x)
S.xy <- sum((y-mean(y))*(x-mean(x)))
S.xx <- sum((x-mean(x))^2)

# The following code checks the above calculations using |
# alternative formulas for Sxx and Sxy. Note the use of  |
# the "length" function to find, n, the number of        |
# observations.                                          |

n <- length(y)
sum.x <- sum(x)
sum.x2 <- sum(x^2)
sum.y <- sum(y)
sum.y2 <- sum(y^2)
sum.xy <- sum(x*y)

y.bar
sum.y / n

x.bar
sum.x / n

S.xy
sum.xy - sum.x*sum.y / n

S.xx
sum.x2 - sum.x^2 / n

# The "lm" function may be used to fit a linear          |
# regression model. The code below only creates an       |
# output object to the "lm" function (which is stored in |
# the variable "rocket.lm"), but does not display any    |
# numerical results.                                     |

rocket.lm <- lm(strength ~ age, data=rocket.data)

# The "summary" function may be used to display a        |
# summary of the numerical output stored in "rocket.lm"  |

summary(rocket.lm)

# The "coefficients" function may be used to display the |
# least-squares estimates.                               |

coefficients(rocket.lm)

# Note that these values are also listed in the output   |
# to the "summary" function.                             |
#                                                        |
# The following code checks the calculation of the       |
# least-squares estimates.                               |

b1.hat <- coefficients(rocket.lm)[[2]]
b1.hat
S.xy / S.xx

b0.hat <- coefficients(rocket.lm)[[1]]
b0.hat
y.bar - b1.hat*x.bar

# The following code uses the values of the coefficients |
# to draw the fitted regression line in the scatterplot. |

min.x <- min(x)
max.x <- max(x)
plot(x, y, pch=16, cex=1, xlab="age of propellant", ylab="shear strength", main = "Scatter diagram of y by x")
lines(c(min.x, max.x), b0.hat + b1.hat*c(min.x, max.x), type="l", lty=1, lwd=3)

# The "fitted" and "resid" functions may be used to      |
# display the fitted values, y-hat, and residuals, e. In |
# the code below, the "cbind" functions puts these and   |
# the observed response values, y, together for tabular  |
# display.                                               |

cbind(y, fitted(rocket.lm), resid(rocket.lm))

# The five-number summary of quantiles for the residuals |
# is                                                     |

quantile(resid(rocket.lm))

# Note that these quantiles of the residuals also listed |
# in the output to the "summary" function.               |
#                                                        |
# -------------------------------------------------------+
# Example 2.2: Rocket propellant data, unbiased          |
# estimator of error variance                            |
# -------------------------------------------------------+
#                                                        |
# The corrected sum of squares is                        |

SS.T <- sum((y-mean(y))^2)
SS.T

# The residual sum of squares is                         |
#                                                        |

SS.Res <- SS.T - b1.hat*S.xy
SS.Res

# The unbiased estimator of error variance is            |

sig2.hat <- SS.Res / (n-2)
sig2.hat

# -------------------------------------------------------+
# Example 2.3: Rocket propellant data, t test for        |
# significance of regression                             |
# -------------------------------------------------------+
#                                                        |
# The residual mean square is the same as the unbiased   |
# estimator of error variance                            |

MS.Res <- sig2.hat

# The standard error of b1.hat is                        |

se.b1.hat <- sqrt(MS.Res / S.xx)
se.b1.hat

# The t test statistic for testing significance of       |
# regression is                                          |

t0 <- b1.hat / se.b1.hat
t0

# The t critical value is                                |

alpha <- 0.05
t.crit <- qt(alpha/2, df=n-2, lower.tail=FALSE)
t.crit

# The p-value is                                         |

p.val <- 2*pt(abs(t0), df=n-2, lower.tail=FALSE)
p.val

# Note that these values are also listed in the output   |
# to the "summary" function.                             |
#                                                        |
# -------------------------------------------------------+
# Example 2.4: Rocket propellant data, F test for        |
# significance of regression                             |
# -------------------------------------------------------+
#                                                        |
# The regression sum of squares is                       |

SS.R <- b1.hat*S.xy
SS.R

# Its associated degrees of freedom is                   |

df.R <- 1
df.R

# The regression mean square is                          |

MS.R <- SS.R / df.R
MS.R

# The F test statistic for testing significance of       |
# regression is                                          |

F0 <- MS.R / MS.Res
F0

# The F critical value is                                |

alpha <- 0.05
F.crit <- qf(alpha, df1=1, df2=n-2, lower.tail=FALSE)
F.crit

# The P-value is                                         |

p.val <- pf(F0, df1=1, df2=n-2, lower.tail=FALSE)
p.val

# Observe that this produces the exact same P-value as   |
# the t test for significance of regression. The known   |
# relationship between the t test statistic and F test   |
# statistic is verified below.                           |

t0^2
F0

# The "anova" function, applied to the "lm" output       |
# object stored in "rocket.lm" may be used to display    |
# most of the analysis-of-variance table.                |

anova(rocket.lm)

# -------------------------------------------------------+
# Example 2.5: Rocket propellant data, CI for slope and  |
# error variance                                         |
# -------------------------------------------------------+
#                                                        |
# The required t upper percentage point to calculate a   |
# CI for slope is                                        |

alpha <- 0.05
t.crit <- qt(alpha/2, df=n-2, lower.tail=FALSE)
t.crit

# The margin of error of the CI for slope is             |

m.err <- t.crit*se.b1.hat
m.err

# The lower bound, point estimate, and upper bound in    |
# the CI for slope are                                   |

c(b1.hat - m.err, b1.hat, b1.hat + m.err)

# This result may also be produced using the "confint"   |
# function, which, when applied to the "lm" output       |
# object stored in "rocket.lm", displays the upper and   |
# lower bounds of the CIs for the intercept and slope.   |

confint(rocket.lm, level=0.95)

# To calculate a CI for error variance, the required     |
# chi-square upper percentage points to are              |

alpha <- 0.05
chisq.crit.lo <- qchisq(1-alpha/2, df=n-2, lower.tail=FALSE)
chisq.crit.hi <- qchisq(alpha/2, df=n-2, lower.tail=FALSE)
c(chisq.crit.lo, chisq.crit.hi)

# The lower bound, point estimate, and upper bound in    |
# the CI for error variance are                          |

c((n-2)*MS.Res / chisq.crit.hi, MS.Res, (n-2)*MS.Res / chisq.crit.lo)

# Note that these values are also listed in the output   |
# to the "summary" function.                             |
