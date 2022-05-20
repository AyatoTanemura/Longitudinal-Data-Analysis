# Library----

library(lattice)
library(gplots)
library(nparLD)
library(tidyverse)
library(nlme)

# Question 1 ----
# Data Load----

gpa <- read.csv("Data/gpa.csv", header = TRUE) %>% 
  mutate(sex = as.factor(sex))

dim(gpa)
str(gpa)
View(gpa)

## First 10 students Q1

gpa10 <- gpa[gpa$student %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), ]

# Data Viz----

xyplot(gpa ~ occas | student, data = gpa10,
       panel = function(x, y){
  panel.xyplot(x, y)
  panel.lmline(x, y)
}, ylim = c(-2, 5), as.table = T)

gpa.male <- gpa[gpa$sex == 1,]
gpa.female <- gpa[gpa$sex == 0,]


warnings()
## Interaction plot

interaction.plot(gpa10$occas, gpa10$student, gpa10$gpa)

plotmeans(gpa.male$gpa ~ gpa.male$occas, data = gpa.male, lwd = 10, barwidth = 5, n.label = FALSE)
plotmeans(gpa.female$gpa ~ gpa.female$occas, data = gpa.female, lwd = 10, barwidth = 5, n.label = FALSE)

myplot <- nparLD(gpa ~ occas * sex, data = gpa, subject = "student", description = FALSE)

plot(myplot)

# Model ----

## Unconditional means Model Q2 

model.gpa <- lme(gpa ~ 1, gpa, random = ~1 |student)

summary(model.gpa)

VC.gpa <- VarCorr(model.gpa)

icc.gpa <- as.numeric(VC.gpa[1,1]) / (as.numeric(VC.gpa[1,1]) + as.numeric(VC.gpa[2,1]))

## Model A (Unconditional growth model) Q2 

a.model.gpa <- lme(gpa ~ occas , data = gpa, random = ~ occas | student, method = "ML")

summary(a.model.gpa)

VarCorr(a.model.gpa)

a.fixef <- fixef(a.model.gpa)

a.fit <- a.fixef[[1]] + gpa$occas[1:6]*a.fixef[[2]]
a.fit

## Model B (gender effects) Q3

b.model.gpa <- lme(gpa ~ sex*occas , data = gpa, random = ~ occas | student, method = "ML")

summary(b.model.gpa)

    ### Pesudo R2 & Variance component
VarCorr(a.model.gpa)
VarCorr(b.model.gpa)

b.fixef <- fixef(b.model.gpa)

b1.fit <- b.fixef[[1]] + gpa$occas[1:6]*b.fixef[[3]]

b0.fit <- b.fixef[[1]] + b.fixef[[2]] + gpa$occas[1:6]*b.fixef[[3]] + gpa$occas[1:6]*b.fixef[[4]]

# Model C (high school gpa & gender effects) Q4
head(gpa)

c.model.gpa <- lme(gpa ~ sex*occas +  highgpa*occas, data = gpa, random = ~ occas | student, method = "ML")

summary(c.model.gpa)



# ===============================================================================================================


# Question 2
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

legend(0.0, 65,c("effective=o", "effective=^"))

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

VarCorr(d.model.math)

# Model E (effective teacher & ses effects) Q5,6

e.model.math <- lme(test ~ effective*time + ses*time, data = math, random = ~ time | id, method = "ML")

summary(e.model.math)
 
