## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 7,
#  fig.path = "fig/datasets-",
  dev = "png",
  comment = "##"
)

# save some typing
knitr::set_alias(w = "fig.width",
                 h = "fig.height",
                 cap = "fig.cap")

## ----load---------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)

## ----read-datasets------------------------------------------------------------
dsets_tagged <- read_excel(here::here("inst", "extdata", "vcdExtra-datasets.xlsx"), 
                           sheet="vcdExtra-datasets")

dsets_tagged <- dsets_tagged |>
  dplyr::select(-Title, -dim) |>
  dplyr::rename(dataset = Item)

head(dsets_tagged)

## ----split-tags---------------------------------------------------------------
dset_split <- dsets_tagged |>
  tidyr::separate_longer_delim(tags, delim = ";") |>
  dplyr::mutate(tag = stringr::str_trim(tags)) |>
  dplyr::select(-tags)

#' ## collapse the rows for the same tag
tag_dset <- dset_split |>
  arrange(tag) |>
  dplyr::group_by(tag) |>
  dplyr::summarise(datasets = paste(dataset, collapse = "; ")) |> ungroup()

# get a list of the unique tags
unique(tag_dset$tag)

## ----read-tags----------------------------------------------------------------
tags <- read_excel(here::here("inst", "extdata", "vcdExtra-datasets.xlsx"), 
                   sheet="tags")
head(tags)

## ----join-tags----------------------------------------------------------------
tag_dset <- tag_dset |>
  dplyr::left_join(tags, by = "tag") |>
  dplyr::relocate(topic, .after = tag)

tag_dset |>
  dplyr::select(-tag) |>
  head()

## ----add-links----------------------------------------------------------------
add_links <- function(dsets, 
                      style = c("reference", "help", "rdrr.io"),
                      sep = "; ") {

  style <- match.arg(style)
  names <- stringr::str_split_1(dsets, sep)

  names <- dplyr::case_when(
    style == "help"      ~ glue::glue("[{names}](help({names}))"),
    style == "reference" ~ glue::glue("[{names}](../reference/{names}.html)"),
    style == "rdrr.io"   ~ glue::glue("[{names}](https://rdrr.io/cran/vcdExtra/man/{names}.html)")
  )  
  glue::glue_collapse(names, sep = sep)
}


## ----kable--------------------------------------------------------------------
tag_dset |>
  dplyr::select(-tag) |>
  dplyr::mutate(datasets = purrr::map(datasets, add_links)) |>
  knitr::kable()

