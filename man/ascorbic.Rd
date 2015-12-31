#' ascorbic data from Crowder and Hand
#'
#' A dataset containing ascorbic acid measures on 12 patients
#' The data come from Table 3.3 in Crowder and Hand.
\name{ascorbic}
\alias{ascorbic}
\docType{data}
\title{
ascorbic / data frame
}
\description{
Twelve hospital patients underwent a dietary regime treatment. Over the course of seven occasions, measurements were taken on ascorbic acid for each patient. The seven occasions divided into three treatment phases - twice before (`pre`), thrice during (`Rx`), and twice after the treatment regime (`post`). The seven occasions occurred at weeks 1, 2, 6, 10, 14, 15, & 16.
}
\usage{data("ascorbic")}
\format{
  A data frame with 84 observations on the following 7 variables.
  \describe{
    \item{\code{patient}}{a factor with levels \code{Pat.01} \code{Pat.02} \code{Pat.03} \code{Pat.04} \code{Pat.05} \code{Pat.06} \code{Pat.07} \code{Pat.08} \code{Pat.09} \code{Pat.10} \code{Pat.11} \code{Pat.12}}
    \item{\code{week}}{a numeric vector}
    \item{\code{occ}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7}}
    \item{\code{phase.num}}{a numeric vector}
    \item{\code{Phase}}{a factor with levels \code{post} \code{pre} \code{Rx}}
    \item{\code{ascorbic.acid}}{a numeric vector}
    \item{\code{Occasion}}{a numeric vector}
  }
}
\details{
Originally, the data were taken from Table 3.3 in Crowder and Hand (1991). The variable \code{patient} is a factor variable, which should be used in fitting the correct model. The variable \code{phase.num} is a numeric variable. The variable \code{Phase} is a factor variable, which should be used instead of \code{phase.num} the fitting of the correct model. The variable \code{Occasion} is a numeric variable.  The variable \code{occ} is a factor variable,which should be used instead of \code{Occasion} in the fitting of the correct model.
}
\source{
Crowder MJ and Hand DJ (1990) Analysis of Repeated Measures, Chapman and Hall.
}
\references{
Crowder MJ and Hand DJ (1990) Analysis of Repeated Measures, Chapman and Hall.
}
\examples{
##
## ***** Get Simple BoxPlots with Points
##
data(ascorbic)
with(ascorbic, plot(ascorbic.acid ~ occ,
xlab = 'Occasions [occ]',
ylab = 'Ascorbic Acid [ascorbic.acid]',
main = 'Ascorbic Acid responses for 12
patients\nacross 7 Occasions'))
with(ascorbic,
points(ascorbic.acid ~ occ, pch = 16,
col = c('black', 'brown', 'blue', 'purple',
'red', 'green', 'orange', 'yellow', 'pink',
'turquoise', 'gray', 'violet')))
##
## ***** End Simple BoxPlots with Points
##
## ***** Plot groupedData data frame with nlme
##
## Create and plot ascorbic.gD.df,
## a groupedData data frame
##
library(nlme)
ascorbic.gD.df <-
groupedData(ascorbic.acid ~ week | patient,
data = ascorbic)
## plot ascorbic.gD.df
plot(ascorbic.gD.df, outer = ~ 1, key = FALSE,
ylab = 'Ascorbic Acid', xlab = 'week',
aspect = 0.6,
main = 'Reaction of Patients to Rx')
##
## ***** End plot groupedData data frame
##
## ***** Produce Traditional DotPlot
##
with(ascorbic,
plot(jitter(Occasion), ascorbic.acid,
ylab = 'Ascorbic Acid', ylim = c(0,5),
xlab = 'Occasions (occ)',
main = 'Ascorbic Acid - Occasions and Phases',
col = c('black', 'brown', 'blue', 'purple',
'red', 'green', 'orange', 'yellow', 'pink',
'turquoise', 'gray', 'violet'), pch = 16,
xaxt = 'n'))
##
## Note: jitter() takes a numeric value, not a factor.
## In data the 'ascorbic' data frame
## 'Occasion' is a numeric variable
## and 'occ' is a factor variable.
##
axis(side = 1, at = c(1:7),
labels = c(1:7),
tick = TRUE, las = 2, cex.axis = .7)
##
abline(v = c(2.5, 5.5), lty = 1, col = 'blue',
lwd = 1)
##
## Add text above data - ascorbic.acid
##
text( 1.0, 4.4, "Phase 1: 'pre'",  pos = 4, offset = 0, cex = 1.0)
text( 3.4, 4.4, "Phase 2: 'Rx'",   pos = 4, offset = 0, cex = 1.0)
text( 5.8, 4.4, "Phase 3: 'post'", pos = 4, offset = 0, cex = 1.0)
##
## Add legend to the dot plot
##
legend.vec <- c('01', '02', '03', '04', '05',
'06', '07', '08', '09', '10', '11', '12')
## Phase 1
legend(1.2, 3.8, legend = legend.vec[1:4],
col = c('black', 'brown', 'blue', 'purple'),
pch = 16, cex = .8, pt.cex = .8 )
## Phase 2
legend(3.8, 3.8, legend = legend.vec[5:8],
col = c('red', 'green', 'orange', 'yellow'),
pch = 16, cex = .8, pt.cex = .8 )
## Phase 3
legend(6.2, 3.8, legend = legend.vec[9:12],
col = c('pink', 'turquoise', 'gray', 'violet'),
pch = 16, cex = .8, pt.cex = .8 )
##
##
## ***** End Traditional DotPlot
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
occ.p1 ## basic boxplot
##
set.seed(11) ## for jittered points to stay fixed
occ.p2 <- occ.p1 +
geom_point(position = position_jitter(width = 0.2), aes(colour = patient))
occ.p2 # add points color-coded by patient
##
set.seed(11) ## for jittered points to stay fixed
occ.p3 <- occ.p2 +
geom_vline(xintercept = c(2.5, 5.5), col = 'blue', lwd = 1.2, linetype = 'longdash')
occ.p3 # add vertical lines to separate Phases
##
set.seed(11) ## for jittered points to stay fixed
occ.p4 <- occ.p3 +
annotate('text', x = 1.5, y = 1.6, label = 'pre Rx') +
annotate('text', x = 4, y = 0.3, label = 'Rx') +
annotate('text', x = 4, y = 0.2, label = '(treatment)') +
annotate('text', x = 6.5, y = 1.6, label = 'post Rx')
occ.p4 # label Phases
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
}
\keyword{datasets}
