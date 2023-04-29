# ====================================================================================================== #
# Description
#
#   Initialize experiment tracking
#   Experiments are tracked by appending results to the tibble defined below
#
# Change log:
#   Ver   Date        Comment
#   1.0   20/04/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

df_expTracking <- 
  tibble(
    "time" = NA_POSIXct_,
    "wflow_name" = NA_character_,
    "val.or.test" = NA_character_,
    "recipe" = list(),
    "model.spec" = list(),
    "tuned.par" = list(), # best hp after tuning. Other parameters are registered in the model spec
    "metrics" = list(),
    "description" = NA_character_
  )

# ==== EXPORT ------------------------------------------------------------------------------------------ 

file_name <- "04a_tracking.RData"

# condition to avoid accidentally overriding existing file
if (!(file_name %in% list.files("./Output/"))) {
  
  save(
    df_expTracking,
    file = paste0("./Output/", file_name)
  )  
  
}


