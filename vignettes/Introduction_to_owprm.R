## ---- echo = FALSE-------------------------------------------------------
library(nlme)
library(MASS)
library(ggplot2)
library(devtools)

## ---- echo = FALSE-------------------------------------------------------
## Read in ascorbic data set
## load owprm package
library(owprm)
data(ascorbic)
## summary(ascorbic)
## str(ascorbic)
##

## ---- echo = FALSE-------------------------------------------------------
## names(ascorbic)
## with(ascorbic, levels(Phase))

## ---- echo = FALSE, fig.width = 5, fig.height = 3.5, fig.cap = "Reaction of twelve `patient`s to treatment (`Rx`) across seven Occasions (`occ`) in three `Phase`s."----
##
ascorbic.gD.df <-
groupedData(ascorbic.acid ~ week | patient,
data = ascorbic)
## plot ascorbic.gD.df
plot(ascorbic.gD.df, outer = ~ 1, key = FALSE,
ylab = 'Ascorbic Acid', xlab = 'Week', aspect = 0.4, main = 'Reaction of Patients to Rx')
##

## ---- echo = FALSE, fig.height = 4.5, fig.width = 6.5, fig.cap = "Reaction of twelve `patient`s to treatment (`Rx`) across seven Occasions (`occ`) in three `Phase`s."----
##
## ***** Get ggplot
##
## Create ggplot2 plot object
library(ggplot2)
##
occ.p0 <- ggplot(ascorbic, aes(occ, ascorbic.acid)) ##
set.seed(11) ## for jittered points to stay fixed
occ.p1 <- occ.p0 +
geom_boxplot(stat = 'boxplot', outlier.shape = 3) + labs(y = 'Ascorbic Acid', x = 'Occasion',
title = 'Ascorbic Acid Responses for 12 Patients
Across 7 Occasions in 3 Phases')
## occ.p1 ## basic boxplot
##
set.seed(11) ## for jittered points to stay fixed
occ.p2 <- occ.p1 +
geom_point(position = position_jitter(width = 0.2), aes(colour = patient))
## occ.p2 # add points color-coded by patient
##
set.seed(11) ## for jittered points to stay fixed
occ.p3 <- occ.p2 +
geom_vline(xintercept = c(2.5, 5.5), col = 'blue', lwd = 1.2, linetype = 'longdash')
## occ.p3 # add vertical lines to separate Phases
##
set.seed(11) ## for jittered points to stay fixed
occ.p4 <- occ.p3 +
annotate('text', x = 1.5, y = 1.6, label = 'pre Rx') +
annotate('text', x = 4, y = 0.3, label = 'Rx') +
annotate('text', x = 4, y = 0.2, label = '(treatment)') +
annotate('text', x = 6.5, y = 1.6, label = 'post Rx')
## occ.p4 # label Phases
##
set.seed(11) ## for jittered points to stay fixed
occ.p5 <- occ.p4 +
annotate('text', x = 1.5, y = 1.7, label = 'Phase 1') +
annotate('text', x = 4, y = 0.4, label = 'Phase 2') +
annotate('text', x = 6.5, y = 1.7, label = 'Phase 3')
occ.p5 # further label Phases
##
## ***** End ggplot
##

## ---- echo = FALSE-------------------------------------------------------
##
aovObj <- aov(ascorbic.acid ~ patient*Phase*occ, data = ascorbic)
## (alternatively) aovObj <- aov(ascorbic.acid ~ patient + Phase + Phase:occ + patient:Phase + patient:Phase:occ, data = ascorbic)
## aovObj
## To avoid having warning messages sent to console
## suppressWarnings(owprm(aovObj))
owprm_aovObj <- suppressWarnings(owprm(aovObj))
owprm_aovObj$'Summary Table of aov object'
## summary(owprm_aovObj)
## display p value in prettier form
## options(digits = 4, scipen = 2)
## owprm_aovObj
## summary(owprm_aovObj)
##

## ---- echo = FALSE-------------------------------------------------------
##
options(digits = 4, scipen = 2)
## owprm_aovObj
summary(owprm_aovObj)
##

## ---- echo = FALSE-------------------------------------------------------
## Reset options to original digits and scipen
options(digits = 7, scipen = 0)
##

## ---- echo = TRUE--------------------------------------------------------
library(owprm)
data(ascorbic)
str(ascorbic)

## ---- echo = TRUE--------------------------------------------------------
aovObj <- aov(ascorbic.acid ~ patient*Phase*occ, data = ascorbic)
aovObj

## ---- echo = TRUE--------------------------------------------------------
summary(aovObj)

## ---- echo = TRUE--------------------------------------------------------
owprm_aovObj <- owprm(aovObj)

## ---- echo = TRUE--------------------------------------------------------
owprm_aovObj <- suppressWarnings(owprm(aovObj))
owprm_aovObj

## ---- echo = TRUE--------------------------------------------------------
summary(owprm_aovObj)

## ---- echo = TRUE--------------------------------------------------------
options(digits = 4, scipen = 2)
summary(owprm_aovObj)

