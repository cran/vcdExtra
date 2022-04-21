## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
  fig.path = "fig/tut04-",
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
if(!file.exists("fig")) dir.create("fig")


## -----------------------------------------------------------------------------
data(Arthritis, package="vcd")
art <- xtabs(~Treatment + Improved, data = Arthritis)
mosaic(art, gp = shading_max, 
            split_vertical = TRUE, 
            main="Arthritis: [Treatment] [Improved]")

## ---- art1--------------------------------------------------------------------
summary(art)

## ----Arthritis2, h=6, w=7-----------------------------------------------------
mosaic(art, gp = shading_Friendly, 
            split_vertical = TRUE, 
            main="Arthritis: gp = shading_Friendly")

