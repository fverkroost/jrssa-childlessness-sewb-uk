# Clear workspace and console and install/load packages
rm(list = ls())
clearhistory <- function() {
  write('', file = '.blank')
  loadhistory('.blank')
  unlink('.blank')
}
clearhistory()
cat('\014')  

# Install and load packages
library(rstan)
library(foreign)
library(mice)
library(beepr)
library(stargazer)
library(dummies)
library(VIM)
library(broom)
# library(tidyverse)
library(tidyselect)
library(xtable)
library(rlang)
library(scales)
library(lubridate)
# library(Hmisc)
library(readxl)
library(ggpubr)
library(reshape2)
library(stringr)
library(magrittr)
library(missForest)
library(parallel)
library(fastDummies)
library(zoo)
library(utils)
library(quantreg)
library(MASS)
library(quantreg)
library(gridExtra)
library(plotly)
library(openxlsx)
library(ggplot2)
library(readstata13)
library(lme4)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
# library(brms)
library(xtable)

# Output folder
out_dir = 'model_outputs/'
if (!dir.exists(out_dir)){ dir.create(out_dir) }

# Set seed
set.seed(1234)

# Set graphics device size
dev.new(height = 4000, width = 5142)
