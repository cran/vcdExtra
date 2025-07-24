## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
#  fig.path = "fig/tidycat-",
  dev = "png",
  comment = "##"
)

## ----load-pkgs----------------------------------------------------------------
library(MASS)
library(vcdExtra)

## -----------------------------------------------------------------------------
data("HairEyeColor")
hec.df <- as.data.frame(HairEyeColor)
head(hec.df)
# expand to case form
expand.dft(hec.df) |> head()

## -----------------------------------------------------------------------------
structable(Titanic)
structable(Sex + Class ~ Survived + Age, data = Titanic)

## ----glass--------------------------------------------------------------------
data(Glass, package="vcdExtra")
str(Glass)
(glass.tab <- xtabs(Freq ~ father + son, data=Glass))

## ----glass-order--------------------------------------------------------------
# reorder by status
ord <- c(2, 1, 4, 3, 5) 
glass.tab[ord, ord]

## ----housetasks-seriation-----------------------------------------------------
library(seriation)
order <- seriate(glass.tab, method = "CA")
# the permuted row and column labels
rownames(glass.tab)[order[[1]]]

# reorder rows and columns
permute(glass.tab, order)

## -----------------------------------------------------------------------------
hec.indep <- loglm(~Hair+Eye+Sex, data=HairEyeColor)
hec.indep
# extract test statistics
summary(hec.indep)$tests
LRstats(hec.indep)

## -----------------------------------------------------------------------------
coef(hec.indep)

## -----------------------------------------------------------------------------
fitted(hec.indep)
residuals(hec.indep)

## ----error=TRUE---------------------------------------------------------------
try({
hatvalues(hec.indep)
})

