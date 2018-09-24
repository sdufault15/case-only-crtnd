################################
# Suzanne Dufault
# September 21, 2018
# This script sets up the work space to produce each of the reports by 
# loading the packages, user-written functions, and data necessary
################################

# Regularly loaded packages
library(here)
library(dplyr)
library(tidyr)

# Necessary functions
source(here("lib", "txtSetFunction.R"))
source(here("lib", "case-only-function.R"))

load(here("data", "Random10000Allocations.RData"))
dta <- Random10000Allocations %>% select(-X)

library(scales)
library(RColorBrewer)
myColors <- brewer.pal(8, 'Set2')
