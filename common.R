suppressPackageStartupMessages({
  library(codetools) # for some Stan auto-checks
  library(rethinking, lib.loc="~/R")
  library(rlang)
  library(vctrs)
  library(withr)
  library(slider)
  library(testthat)
  library(magrittr) # mask testthat::not
  library(bayesplot)
  library(brms)
  library(patchwork)
  library(lubridate)
  library(tidyverse)
  library(tidybayes)
  library(tidymodels)
})

set.seed(646)
