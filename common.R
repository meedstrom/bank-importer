suppressPackageStartupMessages({
  library(rlang)
  library(vctrs)
  library(withr)
  library(slider)
  library(testthat)
  library(magrittr) # mask testthat::not
  library(lubridate)
  library(tidyverse)
})

set.seed(646)
