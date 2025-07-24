## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
#  fig.path = "fig/tut03-",
  dev = "png",
  comment = "##"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")

# preload datasets ???
set.seed(1071)
library(vcd)
library(vcdExtra)
library(ggplot2)
data(HairEyeColor)
data(PreSex)
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
#if(!file.exists("fig")) dir.create("fig")


## ----loglm-hec1---------------------------------------------------------------
library(MASS)
## Independence model of hair and eye color and sex.  
hec.1 <- loglm(~Hair+Eye+Sex, data=HairEyeColor)
hec.1

## ----loglm-hec2---------------------------------------------------------------
## Conditional independence
hec.2 <- loglm(~(Hair + Eye) * Sex, data=HairEyeColor)
hec.2

## ----loglm-hec3---------------------------------------------------------------
## Joint independence model.  
hec.3 <- loglm(~Hair*Eye + Sex, data=HairEyeColor)
hec.3

## ----loglm-anova--------------------------------------------------------------
anova(hec.1, hec.2, hec.3)

## ----mental1------------------------------------------------------------------
data(Mental, package = "vcdExtra")
str(Mental)
xtabs(Freq ~ mental + ses, data=Mental)   # display the frequency table

## ----mental2------------------------------------------------------------------
indep <- glm(Freq ~ mental + ses, family = poisson, data = Mental)  # independence model

## ----mental3------------------------------------------------------------------
# Use integer scores for rows/cols 
Cscore <- as.numeric(Mental$ses)
Rscore <- as.numeric(Mental$mental)	

## ----mental4------------------------------------------------------------------
# column effects model (ses)
coleff <- glm(Freq ~ mental + ses + Rscore:ses, family = poisson, data = Mental)

# row effects model (mental)
roweff <- glm(Freq ~ mental + ses + mental:Cscore, family = poisson, data = Mental)

# linear x linear association
linlin <- glm(Freq ~ mental + ses + Rscore:Cscore, family = poisson, data = Mental)

## ----mental4a-----------------------------------------------------------------
# compare models using AIC, BIC, etc
vcdExtra::LRstats(glmlist(indep, roweff, coleff, linlin))

## ----mental5------------------------------------------------------------------
anova(indep, linlin, coleff, test="Chisq")	
anova(indep, linlin, roweff, test="Chisq")	

## ----mental6------------------------------------------------------------------
CMHtest(xtabs(Freq~ses+mental, data=Mental))

## ----mental7------------------------------------------------------------------
RC1 <- gnm(Freq ~ mental + ses + Mult(mental,ses), data=Mental, 
             family=poisson, verbose=FALSE)
RC2 <- gnm(Freq ~ mental+ses + instances(Mult(mental,ses),2), data=Mental, 
             family=poisson, verbose=FALSE)
anova(indep, RC1, RC2, test="Chisq")

