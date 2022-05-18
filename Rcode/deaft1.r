# Libraries

library(lattice)
library(gplots)
library(nparLD)
library(tidyverse)
library(nlme)

# Load Data

math <- read.csv("Data/mathscore.csv", head = TRUE)

dim(math)
str(math)
head(math)
View(math)

# Model A (effective effects) Q1

# Which variable talking about?
a.model.math <- lme(test ~ effective*time, data = math, random = ~ time | id, method = "ML")
summary(a.model.math)

a.fixef.math <- fixef(a.model.math)

a0.fit.math <- a.fixef.math[[1]] + math$time[1:3]*a.fixef.math[[3]]

a1.fit.math <- a.fixef.math[[1]] + a.fixef.math[[2]] + math$time[1:3]*a.fixef.math[[3]] + math$time[1:3]*a.fixef.math[[4]]

plot(math$time[1:3], a0.fit.math,  ylim=c(45, 65), type="b", 
     ylab="predicted test", xlab="year")

lines(math$time[1:3], a1.fit.math, type="b", pch=17)   

title("Model A") 

legend(14, 2, c("effective=0", "effective=1"))

VarCorr(a.model.math)

# Model B (unconditional means model) Q2

b.model.math <- lme(test ~ 1, math, random = ~1 | id)

summary(b.model.math)

b.var.math <- VarCorr(b.model.math)

b.icc.math <- as.numeric(b.var.math[1,1]) / (as.numeric(b.var.math[1,1]) + as.numeric(b.var.math[2,1]))

# Model C (unconditional growth model) Q3
c.model.math <- lme(test ~ time, data = math, random = ~ time | id, method = "ML")

summary(c.model.math)

# Model D (effective teachers effects) Q4

d.model.math <- lme(test ~ effective*time, data = math, random = ~ time | id, method = "ML")

summary(d.model.math)

# Model E (effective teacher & ses effects) Q5,6

e.model.math <- lme(test ~ effective*time + ses*time, data = math, random = ~ time | id, method = "ML")

summary(e.model.math)
 
