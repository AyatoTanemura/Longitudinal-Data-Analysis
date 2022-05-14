# Library----
library(lattice)
library(gplots)
library(nparLD)
library(tidyverse)
library(nlme)

# Data Load----

gpa <- read.csv("Data/gpa.csv", header = TRUE) %>% 
  mutate(sex = as.factor(sex))

dim(gpa)
str(gpa)
View(gpa)

## First 10 students

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

model.gpa <- lme(gpa ~ 1, gpa, random = ~1 |student)

summary(model.gpa)

VC.gpa <- VarCorr(model.gpa)

icc.gpa <- as.numeric(VC.gpa[1,1]) / (as.numeric(VC.gpa[1,1]) + as.numeric(VC.gpa[2,1]))

