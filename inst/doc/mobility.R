## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
  fig.path = "fig/mobility-",
  dev = "png",
  comment = "##"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")

# colorize text
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
      x)
  } else x
}

## ----hauser-data--------------------------------------------------------------
data("Hauser79", package="vcdExtra")
str(Hauser79)
(Hauser_tab <- xtabs(Freq ~ Father + Son, data=Hauser79))

## ----load---------------------------------------------------------------------
library(vcdExtra)
library(gnm)
library(dplyr)

## ----mosaicplot---------------------------------------------------------------
plot(Hauser_tab, shade=TRUE)

## ----mosaic1------------------------------------------------------------------
labels <- list(set_varnames = c(Father="Father's occupation", 
                                Son="Son's occupation"))

mosaic(Freq ~ Father + Son, data=Hauser79,
       labeling_args = labels,
       shade=TRUE,
       legend = FALSE)

## ----indep--------------------------------------------------------------------
hauser.indep <- glm(Freq ~ Father + Son, 
  data=Hauser79, 
  family=poisson)

# the same mosaic, using the fitted model
mosaic(hauser.indep, formula = ~ Father + Son, 
       labeling_args = labels,
       legend = FALSE,
       main="Independence model")

## ----Diag---------------------------------------------------------------------
# with symbols
with(Hauser79, Diag(Father, Son)) |> matrix(nrow=5)

## ----quasi--------------------------------------------------------------------
hauser.quasi <-  update(hauser.indep, 
                        ~ . + Diag(Father, Son))

mosaic(hauser.quasi, ~ Father+Son, 
       labeling_args = labels,
       legend = FALSE,
       main="Quasi-independence model")


## ----symm---------------------------------------------------------------------
with(Hauser79, Symm(Father, Son)) |> matrix(nrow=5)

## ----qsymm--------------------------------------------------------------------
hauser.qsymm <-  update(hauser.indep, 
                        ~ . + Diag(Father,Son) + Symm(Father,Son))

## ----anova1-------------------------------------------------------------------
anova(hauser.indep, hauser.quasi, hauser.qsymm, test="Chisq")

LRstats(hauser.indep, hauser.quasi, hauser.qsymm)

## ----qsymm-mosaic-------------------------------------------------------------
mosaic(hauser.qsymm, ~ Father+Son, 
       labeling_args = labels,
       labeling = labeling_residuals,
       residuals_type ="rstandard",
       legend = FALSE,
       main="Quasi-symmetry model")

## ----topo-levels--------------------------------------------------------------
# Levels for Hauser 5-level model
levels <- matrix(c(
      2,  4,  5,  5,  5,
      3,  4,  5,  5,  5,
      5,  5,  5,  5,  5,
      5,  5,  5,  4,  4,
      5,  5,  5,  4,  1), 
      nrow = 5, ncol = 5, 
      byrow=TRUE)

## ----topo-mosaic--------------------------------------------------------------
hauser.topo <- update(hauser.indep, 
                      ~ . + Topo(Father, Son, spec=levels))

mosaic(hauser.topo, ~Father+Son, 
       labeling_args = labels,
       labeling = labeling_residuals,
       residuals_type ="rstandard",
       legend = FALSE,
       main="Topological model")

## -----------------------------------------------------------------------------
LRstats(hauser.indep, hauser.quasi, hauser.qsymm, hauser.topo, sortby = "AIC")


## ----scores-------------------------------------------------------------------
Sscore <- as.numeric(Hauser79$Son)
Fscore <- as.numeric(Hauser79$Father)

Hauser79 |> cbind(Fscore, Fscore) |> head()

## ----hauser-UAdiag------------------------------------------------------------
hauser.UAdiag <- update(hauser.indep,
                        . ~ . + Fscore : Sscore + Diag(Father, Son))

LRstats(hauser.UAdiag)

## -----------------------------------------------------------------------------
coef(hauser.UAdiag)[["Fscore:Sscore"]]

## ----UAdiag-mosaic------------------------------------------------------------
mosaic(hauser.UAdiag, ~ Father+Son, 
       labeling_args = labels,
       labeling = labeling_residuals,
       residuals_type ="rstandard",
       legend = FALSE,
       main="Uniform association + Diag()")

