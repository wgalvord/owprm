#' Occasions Within Phases Repeated Measures
#'
#' @param aovObj object returned from \code{aov}
#' @param ... other arguments passed to function (not currently in use)
#'
#' @export
##
owprm <-
  function(aovObj, ...)
  {
    UseMethod('owprm')
  }
##
#' Perform Occasions Within Phases repeated measures analysis.
#'
#' @export
#'
#' @param aovObj object returned from \code{aov}
#' @param ... other arguments passed to function (not currently in use)
#'
#' The \emph{fully saturated} \code{aov} object has the form \deqn{y_{ijk} = \pi_{i} + \Phi_{j} + o_{k(j)} + \pi \Phi_{ij} + \pi o_{ik(j)}}
#'
#' @return \code{owp_aovObj} A list containing an anova summary table
#'     of one line and a summary of the \code{aovObj}
#' @examples data(ascorbic)
#' aovObj <- aov(ascorbic.acid ~ patient*Phase*occ, data = ascorbic)
#' aovObj
#' ## To avoid having warning messages sent to console
#' ## suppressWarnings(owprm(aovObj))
#' owprm_aovObj <- owprm(aovObj)
#' owprm_aovObj
#' summary(owprm_aovObj)
#' ## display p value in prettier form
#' options(digits = 4, scipen = 2)
#' owprm_aovObj
#' summary(owprm_aovObj)
owprm.default <- function(aovObj, ...) {
  .call <- match.call()
  ms.for.phase.ch <- anova(aovObj)[2,3]
  ms.for.patient.by.phase.ch <- anova(aovObj)[4,3]
  F.for.null.phase <- ms.for.phase.ch/ms.for.patient.by.phase.ch
  df.for.num <- round((anova(aovObj)[2,1]), 0)
  df.for.den <- as.integer(anova(aovObj)[4,1])
  pval.for.F.null.phase <- 1 - pf(F.for.null.phase, df.for.num, df.for.den)
  out.table.1 <- c(F.for.null.phase, as.integer(df.for.num), as.integer(df.for.den), pval.for.F.null.phase)
  out.table.1.mat <- matrix(out.table.1, nrow = 1)
  dimnames(out.table.1.mat) <- list('Value', c('F (null)', 'Num df', 'Den df', 'p.val'))
  out.table.2 <- summary(aovObj)
  owpObj <- list(Call = .call, 'Anova Table with Probability' = out.table.1.mat, 'Summary Table of aov object' = out.table.2)
  ## IMPORTANT: assign class to owpObj
  class(owpObj) <- 'owprm'
  owpObj
}
##
#' Print function
#'
#' @param x \code{owprm} object
#' @param ... other arguments to function
#'
#' @export
#'
print.owprm <- function(x, ...) {
  cat('Call: \n')
  print(x$Call)
  cat('\n')
  cat('Anova Summary Table for Occ w/in Phases Rep Meas Analy: \n')
  print(x$Anova)
  cat('\nSummary of Saturated ANOVA Object: \n')
  print(x$Summary)
}

#' Summary
#'
#' @param object \code{owprm} object
#' @param ... other arguments to function
#'
#' @export
#'
summary.owprm <- function(object, ...)
{
  TAB <- object$Anova
  TABout <- list(Call = object$Call, TAB = TAB)
  class(TABout) <- 'summary.owprm'
  TABout
}

#' Print summary
#'
#' @param x \code{owprm} object
#' @param ... other arguments to function
#'
#' @export
#'
print.summary.owprm <- function(x, ...)
{
  ## cat('Call:\n')
  ## print(x$Call)
  cat('\n')
  print(x$TAB)
}

#' ascorbic data from Crowder and Hand
#'
#' A dataset containing ascorbic acid measures on 12 patients
#' The data come from Table 3.3 in Crowder and Hand.
#'
#' @format A data frame with 84 observations on the following 7 variables.
#' \describe{
#'     \item{\code{patient}}{a factor with levels \code{Pat.01}
#'     \code{Pat.02}\code{Pat.03}\code{Pat.04}\code{Pat.05}
#'     \code{Pat.06}\code{Pat.07}\code{Pat.08}\code{Pat.09}
#'     \code{Pat.10}\code{Pat.11}\code{Pat.12}}
#'     \item{\code{week}}{a numeric vector}
#'     \item{\code{occ}}{a factor with levels \code{1} \code{2}
#'     \code{3} \code{4} \code{5} \code{6} \code{7}}
#'     \item{\code{phase.num}}{a numeric vector}
#'     \item{\code{Phase}}{a factor with levels \code{post}
#'     \code{pre} \code{Rx}}
#'     \item{\code{ascorbic.acid}}{a numeric vector}
#'     \item{\code{Occasion}}{a numeric vector}
#'  }
#'
#' @details{
#' The data were taken from Table 3.3 in Crowder and Hand (1991).
#' The variable \code{patient} is a factor variable, which
#' should be used in fitting the correct model. The variable
#' \code{phase.num} is a numeric variable. The variable
#' \code{Phase} is a factor variable, which should be used
#' instead of \code{phase.num} for fitting the correct model.
#' The variable \code{Occasion} is a numeric variable. The
#' variable \code{occ} is a factor variable, which should be
#' used instead of \code{Occasion} in fitting the correct model.
#' }
#'
#' @examples{
#' ##
#' ## ***** Get Simple BoxPlots with Points
#' ##
#' data(ascorbic)
#' with(ascorbic, plot(ascorbic.acid ~ occ,
#' xlab = 'Occasions [occ]',
#' ylab = 'Ascorbic Acid [ascorbic.acid]',
#' main = 'Ascorbic Acid responses for 12
#' patients\nacross 7 Occasions'))
#' with(ascorbic,
#' points(ascorbic.acid ~ occ, pch = 16,
#' col = c('black', 'brown', 'blue', 'purple',
#' 'red', 'green', 'orange', 'yellow', 'pink',
#' 'turquoise', 'gray', 'violet')))
#' ##
#' ## ***** End Simple BoxPlots with Points
#' ##
#' }
#' ##
#' ## ***** Plot groupedData data frame with nlme
#' ##
#' ## Create and plot ascorbic.gD.df,
#' ## a groupedData data frame
#' ##
#' library(nlme)
#' ascorbic.gD.df <-groupedData(ascorbic.acid ~ week | patient,
#'     data = ascorbic)
#'
#' ## plot ascorbic.gD.df
#' plot(ascorbic.gD.df, outer = ~ 1, key = FALSE,
#' ylab = 'Ascorbic Acid', xlab = 'week',
#' aspect = 0.6, main = 'Reaction of Patients to Rx')
#' ##
#' ## ***** End plot of groupedData data frame
#' ##
#' ##
#' ## ***** Get ggplot
#' ##
#' ## Create ggplot2 plot object
#' library(ggplot2)
#' ##
#' occ.p0 <- ggplot(ascorbic, aes(occ, ascorbic.acid))
#' ## set.seed(11) ## for jittered points to stay fixed
#' occ.p1 <- occ.p0 +
#' geom_boxplot(stat = 'boxplot', outlier.shape = 3) +
#' labs(y = 'Ascorbic Acid', x = 'Occasion',
#' title = 'Ascorbic Acid Responses for 12 Patients
#' Across Occasions in 3 Phases')
#' occ.p1 ## basic boxplot
#' ##
#' set.seed(11) ## for jittered points to stay fixed
#' occ.p2 <- occ.p1 +
#' geom_point(position = position_jitter(width = 0.2),
#' aes(colour = patient))
#' occ.p2 # add points color-coded by patient
#' ##
#' set.seed(11) ## for jittered points to stay fixed
#' occ.p3 <- occ.p2 +
#' geom_vline(xintercept = c(2.5, 5.5), col = 'blue', lwd = 1.2,
#' linetype = 'longdash')
#' occ.p3 # add vertical lines to separate Phases
#' ##
#' set.seed(11) ## for jittered points to stay fixed
#' occ.p4 <- occ.p3 +
#' annotate('text', x = 1.5, y = 1.6, label = 'pre Rx') +
#' annotate('text', x = 4, y = 0.3, label = 'Rx') +
#' annotate('text', x = 4, y = 0.2, label = '(treatment)') +
#' annotate('text', x = 6.5, y = 1.6, label = 'post Rx')
#' occ.p4 # label Phases
#' ##
#' set.seed(11) ## for jittered points to stay fixed
#' occ.p5 <- occ.p4 +
#' annotate('text', x = 1.5, y = 1.7, label = 'Phase 1') +
#' annotate('text', x = 4, y = 0.4, label = 'Phase 2') +
#' annotate('text', x = 6.5, y = 1.7, label = 'Phase 3')
#' occ.p5 # further label Phases
#' ##
#' ## ***** End ggplot
#' ##
#' ##
#' ## ***** Produce Traditional DotPlot
#' ##
#' with(ascorbic,
#' plot(jitter(Occasion), ascorbic.acid,
#' ylab = 'Ascorbic Acid', ylim = c(0,5),
#' xlab = 'Occasions (occ)',
#' main = 'Ascorbic Acid - Occasions and Phases',
#' col = c('black', 'brown', 'blue', 'purple',
#' 'red', 'green', 'orange', 'yellow', 'pink',
#' 'turquoise', 'gray', 'violet'), pch = 16,
#' xaxt = 'n'))
#' ##
#' ## Note: jitter() takes a numeric value, not a factor.
#' ## In data the 'ascorbic' data frame
#' ## 'Occasion' is a numeric variable
#' ## and 'occ' is a factor variable.
#' ##
#' axis(side = 1, at = c(1:7),
#' labels = c(1:7), tick = TRUE, las = 2, cex.axis = .7)
#' abline(v = c(2.5, 5.5), lty = 1, col = 'blue', lwd = 1)
#' ##
#' ## Add text above data - ascorbic.acid
#' ##
#' text(1.0, 4.4, "Phase 1: 'pre'",
#'   pos = 4, offset = 0, cex = 1.0)
#' text(3.4, 4.4, "Phase 2: 'Rx'",
#'   pos = 4, offset = 0, cex = 1.0)
#' text(5.8, 4.4, "Phase 3: 'post'",
#'   pos = 4, offset = 0, cex = 1.0)
#' ## Add legend to the dot plot
#' ##
#' legend.vec <- c('01', '02', '03', '04', '05', '06',
#'                 '07', '08', '09', '10', '11', '12')
#' ## Phase 1
#' legend(1.2, 3.8, legend = legend.vec[1:4],
#' col = c('black', 'brown', 'blue', 'purple'),
#' pch = 16, cex = .8, pt.cex = .8 )
#' ## Phase 2
#' legend(3.8, 3.8, legend = legend.vec[5:8],
#' col = c('red', 'green', 'orange', 'yellow'),
#' pch = 16, cex = .8, pt.cex = .8 )
#' ## Phase 3
#' legend(6.2, 3.8, legend = legend.vec[9:12],
#' col = c('pink', 'turquoise', 'gray', 'violet'),
#' pch = 16, cex = .8, pt.cex = .8)
#' ##
#' ## ***** End Traditional DotPlot
#' ##
#'
#' @source{
#' Crowder MJ and Hand DJ (1990) Analysis of Repeated Measures,
#' Chapman and Hall.}
#'
#' @references{
#' Crowder MJ and Hand DJ (1990) Analysis of Repeated Measures,
#' Chapman and Hall.}
#'
#' @keywords{datasets}
#'
"ascorbic"
