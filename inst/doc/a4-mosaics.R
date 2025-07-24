## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
#  fig.path = "fig/tut04-",
  dev = "png",
  comment = "##"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")


# Load packages
set.seed(1071)
library(vcd)
library(vcdExtra)
library(ggplot2)
library(seriation)

data(HairEyeColor)
data(PreSex)
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
#if(!file.exists("fig")) dir.create("fig")

## -----------------------------------------------------------------------------
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
mosaic(art, gp = shading_max, 
            split_vertical = TRUE, 
            main="Arthritis: [Treatment] [Improved]")

## ----art1---------------------------------------------------------------------
summary(art)

## -----------------------------------------------------------------------------
mosaic(art, gp = shading_Friendly, 
            split_vertical = TRUE, 
            main="Arthritis: gp = shading_Friendly")

## ----glass--------------------------------------------------------------------
data(Glass, package="vcdExtra")
(glass.tab <- xtabs(Freq ~ father + son, data=Glass))

## ----glass-mosaic1------------------------------------------------------------
largs <- list(set_varnames=list(father="Father's Occupation", 
                                son="Son's Occupation"),
              abbreviate=10)
gargs <- list(interpolate=c(1,2,4,8))

mosaic(glass.tab, 
  shade=TRUE, 
  labeling_args=largs, 
  gp_args=gargs,
  main="Alphabetic order", 
  legend=FALSE, 
  rot_labels=c(20,90,0,70))

## ----glass-order--------------------------------------------------------------
# reorder by status
ord <- c(2, 1, 4, 3, 5) 
row.names(glass.tab)[ord]

## ----glass-mosaic2------------------------------------------------------------
mosaic(glass.tab[ord, ord], 
  shade=TRUE, 
  labeling_args=largs,  
  gp_args=gargs,
  main="Effect order", 
  legend=FALSE, 
  rot_labels=c(20,90,0,70))

## ----glass-ord----------------------------------------------------------------
Glass.ord <- Glass
Glass.ord$father <- ordered(Glass.ord$father, levels=levels(Glass$father)[ord])
Glass.ord$son    <- ordered(Glass.ord$son,    levels=levels(Glass$son)[ord])
str(Glass.ord)

## ----diag---------------------------------------------------------------------
rowfac <- gl(4, 4, 16)
colfac <- gl(4, 1, 16)
diag4by4 <- Diag(rowfac, colfac)
matrix(Diag(rowfac, colfac, binary = FALSE), 4, 4)

## ----symm---------------------------------------------------------------------
symm4by4 <- Symm(rowfac, colfac)
matrix(symm4by4, 4, 4)

## ----glass-models-------------------------------------------------------------
library(gnm)
glass.indep <- glm(Freq ~ father + son, 
                   data = Glass.ord, family=poisson)
glass.quasi <- glm(Freq ~ father + son + Diag(father, son),  
                   data = Glass.ord, family=poisson)
glass.symm  <- glm(Freq ~ Symm(father, son),  
                   data = Glass.ord, family=poisson)
glass.qsymm <- glm(Freq ~ father + son + Symm(father, son),  
                   data = Glass.ord, family=poisson)

## ----glass-quasi--------------------------------------------------------------
mosaic(glass.quasi, 
  residuals_type="rstandard", 
  shade=TRUE, 
  labeling_args=largs,  
  gp_args=gargs,
  main="Quasi-Independence",
  legend=FALSE, 
  rot_labels=c(20,90,0,70)
  )

## ----glass-anova--------------------------------------------------------------
# model comparisons: for *nested* models
anova(glass.indep, glass.quasi, glass.qsymm, test="Chisq")

## ----glass-lrstats------------------------------------------------------------
models <- glmlist(glass.indep, glass.quasi, glass.symm, glass.qsymm)
LRstats(models)

## ----glass-qsymm--------------------------------------------------------------
mosaic(glass.qsymm, 
  residuals_type="rstandard", 
  shade=TRUE, 
  labeling_args=largs,  
  gp_args=gargs,
  main = paste("Quasi-Symmetry", modFit(glass.qsymm)),
  legend=FALSE, 
  rot_labels=c(20,90,0,70)
  )

## ----housetasks---------------------------------------------------------------
data("HouseTasks", package = "vcdExtra")
HouseTasks

## ----housetasks-mos1----------------------------------------------------------
require(vcd)
mosaic(HouseTasks, shade = TRUE,
       labeling = labeling_border(rot_labels = c(45,0, 0, 0), 
                                  offset_label =c(.5,5,0, 0),
                                  varnames = c(FALSE, TRUE),
                                  just_labels=c("center","right"),
                                  tl_varnames = FALSE),
       legend = FALSE)


## ----housetasks-ca------------------------------------------------------------
require(ca)
HT.ca <- ca(HouseTasks)
summary(HT.ca, rows=FALSE, columns=FALSE)

## ----housetasks-ca-plot-------------------------------------------------------
plot(HT.ca, lines = TRUE)

## ----housetasks-seriation-----------------------------------------------------
require(seriation)
order <- seriate(HouseTasks, method = "CA")
# the permuted row and column labels
rownames(HouseTasks)[order[[1]]]
colnames(HouseTasks)[order[[2]]]

## ----housetasks-mos2----------------------------------------------------------
# do the permutation
HT_perm <- permute(HouseTasks, order, margin=1)

mosaic(HT_perm, shade = TRUE,
       labeling = labeling_border(rot_labels = c(45,0, 0, 0), 
                                  offset_label =c(.5,5,0, 0),
                                  varnames = c(FALSE, TRUE),
                                  just_labels=c("center","right"),
                                  tl_varnames = FALSE),
       legend = FALSE)

