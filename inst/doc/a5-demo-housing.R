## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
#  fig.path = "fig/demo-housing-",
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

## -----------------------------------------------------------------------------
library(vcdExtra)
library(MASS)
library(effects)

## ----housing------------------------------------------------------------------
data(housing, package="MASS")
str(housing)

## -----------------------------------------------------------------------------
levels(housing$Sat)
levels(housing$Infl)

## ----house.null---------------------------------------------------------------
house.null <- glm(Freq ~ Sat + Infl + Type + Cont, family = poisson,
                  data = housing)


## ----house.glm0---------------------------------------------------------------
house.glm0 <- glm(Freq ~ Sat + Infl*Type*Cont, family = poisson,
                  data = housing)

## ----anova--------------------------------------------------------------------
anova(house.null, house.glm0, test = "Chisq")

## -----------------------------------------------------------------------------
# labeling_args for mosaic()
largs <- list(set_varnames = c(
      Infl="Influence on management", 
			Cont="Contact among residents", 
			Type="Type of dwelling", 
			Sat="Satisfaction"),
	abbreviate=c(Type=3))

mosaic(house.glm0, 
       labeling_args=largs, 
       main='Baseline model: [ITC][Sat]')

## ----mosaic-glm0b-------------------------------------------------------------
mosaic(house.glm0, 
       formula = ~ Type + Infl + Cont + Sat, 
       labeling_args=largs, 
       main=paste('Baseline model: [ITC][Sat],', modFit(house.glm0))
  )

## ----addterm------------------------------------------------------------------
MASS::addterm(house.glm0, 
              ~ . + Sat:(Infl + Type + Cont), 
              test = "Chisq")

## ----house-glm1---------------------------------------------------------------
house.glm1 <- update(house.glm0, 
                     . ~ . + Sat*(Infl + Type + Cont))


## ----house-loglm1-------------------------------------------------------------
(house.loglm1 <- MASS::loglm(Freq ~ Infl * Type * Cont + 
                              Sat*(Infl + Type + Cont), data = housing))


## -----------------------------------------------------------------------------
anova(house.glm0, house.glm1, test="Chisq")

## ----mosaic-glm1--------------------------------------------------------------
mosaic(house.glm1, 
       labeling_args=largs, 
       main=paste('Model [IS][TS][CS],', modFit(house.glm1) ), 
       gp=shading_Friendly)

## ----dropterm-----------------------------------------------------------------
MASS::dropterm(house.glm1, test = "Chisq")


## ----addterm1-----------------------------------------------------------------
MASS::addterm(house.glm1,
               ~. + Sat:(Infl + Type + Cont)^2, 
              test  =  "Chisq")

## -----------------------------------------------------------------------------
house.glm2 <- update(house.glm1,
                     . ~ . + Sat:Infl:Type)


## ----lrstats------------------------------------------------------------------
LRstats(house.glm0, house.glm1, house.glm2)

