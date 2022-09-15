# This script takes in the recovery results from empirical data and makes a plot

library(GGally)
library(dplyr)

pandaa_exp_recovery <- read.csv("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling//outputs/vba_out/ffx/pandaa_vba_input/exp/pandaa_vba_input_exp_ffx_global_statistics.csv")

personEstimates = select(pandaa_exp_recovery, ends_with("transformed"))

ggpairs(personEstimates)
