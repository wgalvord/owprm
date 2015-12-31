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
#' ## (alternatively) aovObj <- aov(ascorbic.acid ~ patient + Phase + Phase:occ + patient:Phase + patient:Phase:occ, data = ascorbic)
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
#'
"ascorbic"
