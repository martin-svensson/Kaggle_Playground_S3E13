# ====================================================================================================== #
# Description
#
#   Script for stacking model candidates from 04_modelComparison
#
# Change log:
#   Ver   Date        Comment
#   1.0   11/04/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)
library(bonsai) # lightgbm
library(discrim)
library(baguette)
library(rules)
library(stacks)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/04_output.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

model_stack <- 
  stacks() %>% 
  add_candidates(output_04$cv_results)

set.seed(1681138)

model_stack_ens <- 
  blend_predictions(
    model_stack,
    penalty = 10^seq(-3, 0, length = 10),
    metric = metric_set(roc_auc)
  )

# -- Compare penalty values
autoplot(model_stack_ens)

# -- See member coefficients
autoplot(
  model_stack_ens,
  "weights"
)


# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_04b <- 
  list(
    "model_stack" = model_stack,
    "model_stack_ens" = model_stack_ens
  )

save(
  output_04b,
  file = "./Output/04c_output.RData"
)
