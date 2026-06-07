## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
  dev = "png",
  comment = "##"
)

library(vcdExtra)
library(dplyr)
library(tidyr)

## ----overoll_loadselect-------------------------------------------------------
data("starwars", package = "dplyr")

star_case <- starwars |>
  dplyr::select(c("hair_color", "skin_color", "eye_color")) |> 
  tidyr::drop_na()

str(star_case)

## ----overcoll_hairunique------------------------------------------------------
unique(star_case$hair_color)

## ----overcoll_ex1-------------------------------------------------------------
collapsed.star_case <- collapse_levels(
  star_case,             # The dataset
  hair_color = list(     # Assign the variable to be collapsed to a list
    
    # Format the list as NewLevel = c("old1", "old2", ..., "oldn")
    Blonde = c("blond", "blonde"), 
    Brown = c("brown", "brown, grey"),
    Auburn = c("auburn, white", "auburn, grey", "auburn")
  )
)
str(collapsed.star_case)
unique(collapsed.star_case$hair_color)

## ----overcoll_skinunique------------------------------------------------------
unique(star_case$skin_color)

## ----overcoll_ex2-------------------------------------------------------------
collapsed.star_case <- collapse_levels(
  collapsed.star_case,
  skin_color = list(
    Shades = c(
      "fair", "white", "light", "dark", "grey", "grey, red", 
      "grey, blue", "white, blue", "grey, green, yellow", "fair, green, yellow"
    ), 
    Rainbows = c(
      "green", "pale", "metal", "brown mottle", "brown", "mottled green", 
      "orange", "blue, grey", "red", "blue", "yellow", "tan", "silver, red",
      "green, grey", "red, blue, white", "brown, white"
    )
  )
)
str(collapsed.star_case)
unique(collapsed.star_case$skin_color)

## ----overcoll_eyeunique-------------------------------------------------------
unique(star_case$eye_color)

## ----overcoll_ex3-------------------------------------------------------------
collapsed.star_case <- collapse_levels(
  collapsed.star_case,
  eye_color = list(
    Normal = c("blue", "brown", "blue-gray", "hazel", "dark"), 
    Abnormal = c(
      "yellow", "red", "orange", "black", "pink", "red, blue", "gold", 
      "green, yellow", "white"
    )
  )
)
str(collapsed.star_case)
unique(collapsed.star_case$eye_color)

## ----overcoll_ex4-------------------------------------------------------------
collapsed.star_case <- collapse_levels(
  collapsed.star_case,
  hair_color = list(    # First variable
    Dark = c("Brown", "black", "Auburn"),
    Light = c("Blonde", "white", "grey")
  ),
  skin_color = list(    # Second variable
    Other = c("none", "unknown")
  )
)
unique(collapsed.star_case$hair_color)
unique(collapsed.star_case$skin_color)
str(collapsed.star_case)

## ----overconv-ex1-------------------------------------------------------------
star_freqform <- as_freqform(collapsed.star_case)

str(star_freqform)

## ----overconv-ex2-------------------------------------------------------------
as_freqform(collapsed.star_case, tidy = FALSE) |> str()

## ----overconv-ex3-------------------------------------------------------------
star_tab <- as_table(star_freqform, freq = "Freq")

str(star_tab)

## ----overconv-ex4-------------------------------------------------------------
star_array <- as_array(star_tab)

class(star_array)
str(star_array)

## ----overconv-ex5-------------------------------------------------------------
star_mat <- as_matrix(star_array, dims = c("hair_color", "eye_color"))

class(star_mat)
str(star_mat)

## ----overconv-ex6-------------------------------------------------------------
as_freqform(star_tab, dims = c("hair_color", "eye_color")) |> str()

## ----propconv-ex1-------------------------------------------------------------
star_mat # To view the original

as_table(star_mat, prop = TRUE)

## ----propconv-ex2-------------------------------------------------------------
as_table(star_mat, prop = "hair_color")

## ----propconv-ex3-------------------------------------------------------------
as_table(star_mat, prop = c("hair_color", "eye_color"))

## ----tt-ex1-------------------------------------------------------------------
home_star <- starwars |>
  dplyr::select(c("hair_color", "skin_color", "eye_color", "homeworld")) |> 
  tidyr::drop_na()

# Sort unique levels of homeworld
lvls <- home_star$homeworld |> unique() |> sort()
lvls

# Collapse variable levels
collapsed.home_star <- collapse_levels(
  home_star,
  homeworld = list(
    abc = lvls[1:(length(lvls)/2)],
    xyz = lvls[(length(lvls)/2 + 1):length(lvls)]
  ),
  eye_color = list(
    Normal = c("blue", "brown", "blue-gray", "hazel", "dark"), 
    Abnormal = c(
      "yellow", "red", "orange", "black", "pink", "red, blue", "gold", 
      "green, yellow", "white"
    )
  )
)
# Convert to table of dimensions 'homeworld' and 'eye_color'
tab.home_star <- as_table(collapsed.home_star, dims = c("homeworld", "eye_color"))

# Plot as mosaic display
mosaic(tab.home_star, shading = TRUE, gp = shading_Friendly)

# Convert table into matrix of proportions. Note argument 'dims' was not supplied
# as we already know that there are exactly 2 dimensions.
as_matrix(tab.home_star, prop = TRUE)

## ----ttpipeline, eval=FALSE---------------------------------------------------
# dataset |>                             # Gather the data
#   select(...) |> drop_na() |> ... |>   # Clean the data
#   collapse_levels(...) |>              # Collapse levels as necessary
#   as_form(...)   # Convert forms, select dimensions, take proportions

