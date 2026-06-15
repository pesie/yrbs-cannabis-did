# 00_setup.R -------------------------------------------------------------
# global setup: libraries, paths, crosswalks
# source this at the top of every other script
# cannabis legalization and youth cannabis use, YRBS 2005-2023
# author: precious esie

# 1. libraries -----------------------------------------------------------

library(haven)
library(tidyverse)
library(survey)
library(did)
library(fixest)
library(scales)

# 2. paths ---------------------------------------------------------------

path_data_cdc <- "~/YRBS/Data/CDC States/"
path_data_states <- "~/YRBS/Data/"
path_panel <- "~/YRBS/Data/Panel Data/"
path_results <- "~/YRBS/Results/"

dir.create(path_panel, showWarnings=FALSE, recursive=TRUE)
dir.create(path_results, showWarnings=FALSE, recursive=TRUE)

# 3. survey year crosswalk -----------------------------------------------
# maps calendar year to YRBS survey cycle number (1=1991, 17=2023)

survey_year_xwalk <- data.frame(
  survyear = 1:17,
  year = seq(1991, 2023, by=2)
)

# 4. cannabis variable crosswalk -----------------------------------------
# cannabis question number varied across survey years

cannabis_var_xwalk <- data.frame(
  year = c(2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023),
  cannabis_var = c("q46","q47","q47","q48","q49","q49","q48","q47","q47","q48"),
  race_var = c("q4", rep("raceeth", 9))
)