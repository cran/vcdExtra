## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
  fig.path = "fig/tut02-",
  dev = "png",
  comment = "##"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")

# Old Sweave options
# \SweaveOpts{engine=R,eps=TRUE,height=6,width=7,results=hide,fig=FALSE,echo=TRUE}
# \SweaveOpts{engine=R,height=6,width=7,results=hide,fig=FALSE,echo=TRUE}
# \SweaveOpts{prefix.string=fig/vcd-tut,eps=FALSE}
# \SweaveOpts{keep.source=TRUE}


# preload datasets ???
set.seed(1071)
library(vcd)
library(vcdExtra)
library(ggplot2)
data(HairEyeColor)
data(PreSex)
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
if(!file.exists("fig")) dir.create("fig")


## ---- GSStab------------------------------------------------------------------
# Agresti (2002), table 3.11, p. 106
GSS <- data.frame(
  expand.grid(sex = c("female", "male"), 
              party = c("dem", "indep", "rep")),
  count = c(279,165,73,47,225,191))

(GSStab <- xtabs(count ~ sex + party, data=GSS))

## ---- xtabs-ex2---------------------------------------------------------------
# 2-Way Cross Tabulation
library(gmodels)
CrossTable(GSStab, prop.t=FALSE, prop.r=FALSE, prop.c=FALSE)

## ---- chisq-------------------------------------------------------------------
(HairEye <- margin.table(HairEyeColor, c(1, 2)))

chisq.test(HairEye)

chisq.test(HairEye, simulate.p.value = TRUE)

## ----fisher-------------------------------------------------------------------
fisher.test(GSStab)

## ----fisher-error, error=TRUE-------------------------------------------------
fisher.test(HairEye)

## ---- mantel1-----------------------------------------------------------------
# UC Berkeley Student Admissions
mantelhaen.test(UCBAdmissions)

## ---- mantel2-----------------------------------------------------------------
oddsratio(UCBAdmissions, log=FALSE)

lor <- oddsratio(UCBAdmissions)  # capture log odds ratios
summary(lor)

woolf_test(UCBAdmissions) 

## ---- reorder3----------------------------------------------------------------
UCB <- aperm(UCBAdmissions, c(2, 1, 3))
dimnames(UCB)[[2]] <- c("Yes", "No")
names(dimnames(UCB)) <- c("Sex", "Admit?", "Department")

## -----------------------------------------------------------------------------
col <- c("#99CCFF", "#6699CC", "#F9AFAF", "#6666A0", "#FF0000", "#000080")
fourfold(UCB, mfrow=c(2,3), color=col)

## ----fourfold2, eval=FALSE----------------------------------------------------
#  cotabplot(UCB, panel = cotab_fourfold)

## -----------------------------------------------------------------------------
doubledecker(Admit ~ Dept + Gender, data=UCBAdmissions[2:1,,])

## -----------------------------------------------------------------------------
plot(lor, 
     xlab="Department", 
     ylab="Log Odds Ratio (Admit | Gender)")

## ---- table-form2, include=FALSE----------------------------------------------
## A 4 x 4 table  Agresti (2002, Table 2.8, p. 57) Job Satisfaction
JobSat <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4)
dimnames(JobSat) = list(income=c("< 15k", "15-25k", "25-40k", "> 40k"),
                satisfaction=c("VeryD", "LittleD", "ModerateS", "VeryS"))
JobSat <- as.table(JobSat)

## ---- jobsat------------------------------------------------------------------
JobSat

## ---- cmh1--------------------------------------------------------------------
CMHtest(JobSat, rscores=c(7.5,20,32.5,60))

## ---- assoc1------------------------------------------------------------------
assocstats(GSStab)

## ---- gamma-------------------------------------------------------------------
GKgamma(JobSat)

## ---- kappa-------------------------------------------------------------------
data(SexualFun, package = "vcd")
(K <- Kappa(SexualFun))
confint(K)

## -----------------------------------------------------------------------------
agree <- agreementplot(SexualFun, main="Is sex fun?")
unlist(agree)

## ---- ca1---------------------------------------------------------------------
library(ca)
ca(HairEye)

## ----ca-haireye, cap = "Correspondence analysis plot for the `HairEye` data"----
plot(ca(HairEye), main="Hair Color and Eye Color")

