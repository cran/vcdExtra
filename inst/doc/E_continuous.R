## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
  fig.path = "fig/tut05-",
  fig.align = "center",
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
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
if(!file.exists("fig")) dir.create("fig")


## ---- spine1------------------------------------------------------------------
(spine(Improved ~ Age, data = Arthritis, breaks = 3))
(spine(Improved ~ Age, data = Arthritis, breaks = "Scott"))

## -----------------------------------------------------------------------------
cdplot(Improved ~ Age, data = Arthritis)

## -----------------------------------------------------------------------------
cdplot(Improved ~ Age, data = Arthritis)
with(Arthritis, rug(jitter(Age), col="white", quiet=TRUE))

## ---- donner1-----------------------------------------------------------------
data(Donner, package="vcdExtra")
str(Donner)

## ---- donner2a, fig=FALSE, eval=FALSE-----------------------------------------
#  # separate linear fits on age for M/F
#  ggplot(Donner, aes(age, survived, color = sex)) +
#    geom_point(position = position_jitter(height = 0.02, width = 0)) +
#    stat_smooth(method = "glm",
#                method.args = list(family = binomial),
#                formula = y ~ x,
#                alpha = 0.2, size=2, aes(fill = sex))

## ---- donner2b, fig=FALSE, eval=FALSE-----------------------------------------
#  # separate quadratics
#  ggplot(Donner, aes(age, survived, color = sex)) +
#    geom_point(position = position_jitter(height = 0.02, width = 0)) +
#    stat_smooth(method = "glm",
#                method.args = list(family = binomial),
#                formula = y ~ poly(x,2),
#                alpha = 0.2, size=2, aes(fill = sex))

## -----------------------------------------------------------------------------
# separate linear fits on age for M/F
ggplot(Donner, aes(age, survived, color = sex)) +
  geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", 
              method.args = list(family = binomial), 
              formula = y ~ x,
              alpha = 0.2, size=2, aes(fill = sex))

# separate quadratics
ggplot(Donner, aes(age, survived, color = sex)) +
  geom_point(position = position_jitter(height = 0.02, width = 0)) +
  stat_smooth(method = "glm", 
              method.args = list(family = binomial), 
              formula = y ~ poly(x,2),
              alpha = 0.2, size=2, aes(fill = sex))

