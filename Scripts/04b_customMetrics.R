# ====================================================================================================== #
# Description
#
#   Custom Metric: Yardstick implementation of MAPk
#   See https://www.kaggle.com/code/nandeshwar/mean-average-precision-map-k-metric-explained-code/notebook
#
# Change log:
#   Ver   Date        Comment
#   1.0   26/03/23    Initial version
#
# ====================================================================================================== #
# ------------------------------------------------------------------------------------------------------ #
# LIBRARIES
# ------------------------------------------------------------------------------------------------------ #

library(tidymodels)
library(rlang)

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# I don't know how to implement mapk in the yardstick framework, so we will have to compute it separately

fun_mapk <- function(data, k = 3) {
  
  # data: Output from collect_predictions, ie. dataframe with the truth (prognosis) and predicted class probabilities (.pred_class)
  
  df_mapk <- 
    data %>% 
    select(
      .row,
      prognosis, 
      starts_with(".pred")
    ) %>% 
    pivot_longer(
      cols = starts_with(".pred")
    ) %>% 
    mutate(
      name = gsub("\\.pred_", "", x = name)
    ) %>% 
    arrange(
      .row, 
      desc(value)
    ) %>% 
    filter(
      row_number() %in% 1:k,
      .by = .row
    )
  
  df_mapk %<>%
    group_by(.row) %>% 
    mutate(
      hit = if_else(prognosis == name, 1, 0),
      precision = hit / row_number(.row)
    )
    
  return(sum(df_mapk$precision) / nrow(data))
  
}


