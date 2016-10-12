# Multilevel Modeling of Educational Data using R
# A. Gutierrez
# Taken from: http://hagutierrezro.blogspot.com.co/2016/10/multilevel-modeling-of-educational-data.html?utm_source=feedburner&utm_medium=email&utm_campaign=Feed:+ApuntesDeEstadistica+(Apuntes+de+Estadística)

rm(list = ls())

library(ggplot2)
library(gridExtra)
library(lme4)
library(sjPlot)
library(dplyr)

set.seed(123)

#####

N <- 100 # Number of students per school
sigma <- 200
x1 <- runif(N, 10, 40)
x2 <- runif(N, 25, 55)
x3 <- runif(N, 40, 70)
x4 <- runif(N, 55, 85)
x5 <- runif(N, 70, 100)
y1 <- 20 + 0 * x1 + rnorm(N, 0, sigma)
y2 <- 40 + 10 * x2 + rnorm(N, 0, sigma)
y3 <- 60 + 20 * x3 + rnorm(N, 0, sigma)
y4 <- 80 + 30 * x4 + rnorm(N, 0, sigma)
y5 <- 100 + 40 * x5 + rnorm(N, 0, sigma)
ID <- rep(LETTERS[1:5], each = N)

test <- data.frame(SES = c(x1, x2, x3, x4, x5), 
                   Score = c(y1, y2, y3, y4, y5), ID = ID)

HLM0 <- lmer(Score ~ (1 | ID), data = test)
coef(HLM0)
summary(HLM0)

HLM1 <- lmer(Score ~ SES + (SES | ID), data = test)
coef(HLM1)
summary(HLM1)

# 1% - BS variance
# 99% - WS variance
100 * 40400.24 / (40400.24 + 257.09 + 1.65)
# Percentage of variation explained by SES between schools
1 - ((257.09 + 1.65) / 1931757)
# Percentage of variation explained by SES within schools
1 - (40400.24 / 87346)
