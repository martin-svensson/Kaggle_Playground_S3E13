# ====================================================================================================== #
# Description
#
#   Feature engineering: implementing findings from EDA
#
# Change log:
#   Ver   Date        Comment
#   1.0   20/04/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(data.table)
library(tidyverse)
library(magrittr)

library(tidymodels)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/01_output.RData")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# Since there is so little data, we will not be using the train/test split
df_train <- 
  output_01$df_train %>% 
  rows_append(
    y = output_01$df_train_org
  )

# ---- Feature Engineering -----------------------------------------------------

# -- Recipe - base -------------------------------------------------------------
#    Minimal feature engineering - all following recipes builds on this one

recipe_base <- 
  recipe(
    formula = prognosis ~ .,
    data = df_train
  ) %>% 
  # -- update roles
  update_role(
    id,
    new_role = "id"
  ) 


# -- Recipe - no original data ----------------------------------------------------------------

recipe_noorg <- 
  recipe_base %>% 
  step_filter(original == 0)

# -- Recipe - manual feateng ----------------------------------------------------
#    Manual feature engineering based on domain knowledge




# -- Recipe - minimal --------------------------------------------------------
#    Avoid overfitting by selecting only a few predictors based on EDA

recipe_mini <- 
  recipe(
    formula = 
      prognosis ~ 
      toenail_loss + yellow_skin + ascites + itchiness + 
      coma + stomach_pain + nose_bleed + hypotension + back_pain + 
      light_sensitivity + pleural_effusion + yellow_eyes + loss_of_appetite + ulcers,
    data = df_train
  )

# -- Recipe - interact -------------------------------------------------------

recipe_int <- 
  recipe_base %>% 
  step_interact(
    terms = ~ toenail_loss:itchiness:ulcers + yellow_eyes:urination_loss:yellow_skin:abdominal_pain:loss_of_appetite
  ) 
  
if (FALSE) {
  
  rec_int_test <- 
    recipe_int %>% 
    prep() %>% 
    bake(df_train) %>% 
    as.data.table
  
}

# -- Recipe - nonlinear terms ------------------------------------------------




# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_03 <- 
  list(
    "recipe_base" = recipe_base,
    "recipe_noorg" = recipe_noorg,
    #"recipe_norm" = recipe_norm,
    #"recipe_mfeng" = recipe_mfeng,
    "recipe_mini" = recipe_mini,
    "recipe_int" = recipe_int
    #"recipe_spline" = recipe_spline
  )  

save(
  output_03,
  file = "./Output/03_output.RData"
)
