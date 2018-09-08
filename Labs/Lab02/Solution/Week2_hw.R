library(MPV)
library(dplyr)

directory_path <- "C:\\Users\\arvra\\Documents\\UVa files\\Classes\\Fall_18\\STAT Intro to linear models\\Labs\\Lab02\\Data\\"
setwd(directory_path)

## PROBLEM 2.1
#(a) FITTING A MODEL

#football_data <- read.csv("football.txt", header = FALSE, sep = "\t")
#Model with x8 as the independent variable as y as the response variable
model_2a <- lm( y ~ x8, data = table.b1)

#Summary of the model
summary(model_2a)


# Analysis of Varaiance
#Calculating the contents of ANOVA table using the formula
SSt = sum((table.b1$y - mean(table.b1$y))^2)
SSr = sum(table.b1$y * (table.b1$x8 - mean(table.b1$x8))) * model_2a$coefficients[['x8']]
SSres = SSt - SSr

MSr =   SSr/1
MSres = SSres/(nrow(table.b1)-2)
  
F_score =   MSr / MSres
F_score
#[1] 31.10324

#Test of significant

#p-value
#7.381e-06

# Problem (c)
sig_level = 0.05
n_minus_2 = nrow(table.b1) - 2
crit_t_value <- abs(qt(sig_level/2, n_minus_2))


#Standard error of beta

std_error <- summary(model_2a)$coefficients["x8","Std. Error"]

x8_coefficient <- summary(model_2a)$coefficients["x8","Estimate"]
CI_95 <- c(x8_coefficient - std_error * crit_t_value,
        x8_coefficient,
        x8_coefficient + std_error * crit_t_value)

CI_95
#[1] -0.009614347 -0.007025100 -0.004435854


# Prob 2.4 ########################################



#Model with x1 as the independent variable as y as the response variable
model_2.4a <- lm( y ~ x1, data = table.b3)

#Summary of the model
summary(model_2.4a)


# Analysis of Varaiance
#Calculating the contents of ANOVA table using the formula
SSt = sum((table.b3$y - mean(table.b3$y))^2)
SSr = sum(table.b3$y * (table.b3$x1 - mean(table.b3$x1))) * model_2a$coefficients[['x1']]
SSres = SSt - SSr

MSr =   SSr/1
MSres = SSres/(nrow(table.b3)-2)

F_score =   MSr / MSres
F_score
#[1] 101.7357

#Test of significant

#p-value
#3.743e-11


#############################################


oxygen <- oxygen
## PROBLEM 2.7
#(a) FITTING A MODEL

#Model with 'hydro' as the independent variable as 'purity' as the response variable
model_2.7a <- lm( purity ~ hydro, data = oxygen)

#Summary of the model
summary(model_2.7a)


# Analysis of Varaiance
#Calculating the contents of ANOVA table using the formula
SSt = sum((oxygen$purity - mean(oxygen$purity))^2)
SSr = sum(oxygen$purity * (oxygen$hydro - mean(oxygen$hydro))) * model_2.7a$coefficients[['hydro']]
SSres = SSt - SSr

MSr =   SSr/1
MSres = SSres/(nrow(oxygen)-2)

F_score =   MSr / MSres
F_score
#[1] 11.4658

#Test of significant

#p-value
#0.003291

########################################################

steam_data <- read.csv("steam.txt",header = FALSE,sep = " ")
names(steam_data) <- c("Temp","Steam")


#Building a simple linear model to find the relation of temperature with steam
steam_model <- lm(Steam ~ Temp, data = steam_data)
summary(steam_model)


#ANOVA - Test for Significance
SSt = sum((steam_data$Steam - mean(steam_data$Steam))^2)
SSr = sum(steam_data$Steam * (steam_data$Temp - mean(steam_data$Temp))) * steam_model$coefficients[['Temp']]
SSres = SSt - SSr

MSr =   SSr/1
MSres = SSres/(nrow(steam_data)-2)

F_score =   MSr / MSres
F_score
#[1] 74122.78

#Test of significant

#p-value
#< 2.2e-16

# part c
#Answer
#No, as per the model with 1 degree rise in temperature, we see the steam would increase by
# 9208 lb as opposed to 10,000 lb


# QUESTION 2.19 ################################################

#PART A
satisfaction <- data.table::fread("patient.txt", sep = " ", header = FALSE) %>% data.frame()

## satisfaction vs serverity

#Building a simple linear model to find the relation of severity with satisfaction
satisfaction_sev_model <- lm(V1 ~ V3, data = satisfaction)
summary(satisfaction_sev_model)

## satisfaction vs Age

#Building a simple linear model to find the relation of age with satisfaction
satisfaction_age_model <- lm(V1 ~ V2, data = satisfaction)
summary(satisfaction_age_model)

#PART B

plot(satisfaction$V2,satisfaction$V1)

#Red color - Severity
abline(lm(V1 ~ V3, data = satisfaction), col = "red")

#Blue color - Age
abline(lm(V1 ~ V2, data = satisfaction), col = "blue")

# PART C

#Calculating the parameters for severity model

SSt = sum((satisfaction$V1 - mean(satisfaction$V1))^2)
SSr = sum(satisfaction$V1 * (satisfaction$V3 - mean(satisfaction$V3))) * satisfaction_sev_model$coefficients[['V3']]
SSres = SSt - SSr
SSres

#Calculating the parameters for age model

SSt = sum((satisfaction$V1 - mean(satisfaction$V1))^2)
SSr = sum(satisfaction$V1 * (satisfaction$V2 - mean(satisfaction$V2))) * satisfaction_age_model$coefficients[['V2']]
SSres = SSt - SSr
SSres

# PART D

'
The model with Age with more tightly fit than the model with Severity
'

# PART E

'
SSRes shows the proof for the observation in Part D. It is seen that SSRes for age is 2619 while
that for the severity model is 6210 which is very high compared to the age model.  This is because 
of higher residual value for severity model.
'

# ADDITIONAL PROBLEM # PROBLEM 6




