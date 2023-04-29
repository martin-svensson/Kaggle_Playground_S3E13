# ====================================================================================================== #
# Description
#
#   Model comparison through resampling techniques (cross validation) and
#   workflow sets (combining feature engineering and models)
#   Run initTracking before this script to initialize the tibble for tracking
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
library(finetune) # race approach
library(bonsai) # lightgbm
library(discrim) # MARS multi class
library(baguette) # bagged MARS
library(rules) # C5

library(tictoc)

# ------------------------------------------------------------------------------------------------------ #
# IMPORT AND SOURCES
# ------------------------------------------------------------------------------------------------------ #

load("./Output/01_output.RData")
load("./Output/03_output.RData")
load("./Output/04a_tracking.RData")

# -- Custom metric (mapk)
source("./Scripts/04b_customMetrics.R")

# ------------------------------------------------------------------------------------------------------ #
# PROGRAM
# ------------------------------------------------------------------------------------------------------ #

# we use the entire training because there is so little data overall (better strategy than using test data)

# df_train_split <- training(output_01$data_split)
# 
# df_train_split %<>% 
#   rows_append(
#     y = output_01$df_train_org
#   )

df_train <- 
  output_01$df_train %>% 
  rows_append(
    y = output_01$df_train_org
  )

# ---- Model Specifications ----------------------------------------------------

# -- xgboost: Paramters have been set based on grid search
xgb_spec <- 
  boost_tree(
    tree_depth = 10,
    learn_rate = 0.005, 
    loss_reduction = 10^(-5), 
    min_n = 15,
    sample_size = 0.6, 
    trees = 2000
  ) %>% 
  set_mode("classification") %>% 
  set_engine(
    "xgboost",
    lambda = 0.5,
    alpha = 0.2, 
    colsample_bytree = 0.75,
    counts = FALSE
  )

# -- Light GBM: Paramters have been set based on grid search
lightgbm_spec <- 
  boost_tree(
    tree_depth = 14, 
    learn_rate = 0.001, 
    loss_reduction = 10^(-5),
    min_n = 12,
    sample_size = 0.5,
    trees = 1075
  ) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm") 

# -- GLM: Paramters have been set based on grid search (no multiclass version)
glm_spec <-
  multinom_reg(
    penalty = 0.01,
    mixture = 0.6
  ) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# -- GAM: Paramters have been set based on grid search
# gam_spec <-
#   gen_additive_mod(
#     select_features = FALSE,
#     adjust_deg_free = 3 
#   ) %>% 
#   set_mode("classification")

# -- MARS: Paramters have been set based on grid search
mars_spec <- 
  discrim_flexible(
    num_terms = 60,
    prod_degree = 1,
    prune_method = "backward"
  ) %>% 
  set_mode("classification") %>% 
  set_engine("earth")

# -- Bagged MARS: Paramters have been set based on grid search 
# bagmars_spec <-
#   bag_mars(
#     num_terms = tune(),
#     prod_degree = 1,
#     prune_method = "backward"
#   ) %>%
#   set_mode("classification") %>%
#   set_engine(
#     "earth",
#     base_model = "C5.0"
#   )

# -- C5: Paramters have been set based on grid search
C5_spec <- 
  C5_rules(
    trees = 30,
    min_n = 5
  ) %>% 
  set_mode("classification") %>% 
  set_engine("C5.0")

# -- KNN: Paramters have been set based on grid search
knn_spec <- 
  nearest_neighbor(
    neighbors = 20,
    weight_func = "inv",
    dist_power = 1.5
  ) %>% 
  set_mode("classification") %>% 
  set_engine("kknn")

# ---- Workflow set ------------------------------------------------------------

wflow <- 
  workflow_set(
    preproc = 
      list(
        "recipe_base" = output_03$recipe_base,
        "recipe_noorg" = output_03$recipe_noorg, # performs worse than recipe_base
        "recipe_mini" = output_03$recipe_mini,
        "recipe_int" = output_03$recipe_int
      ),
    models = 
      list(
        "xgboost" = xgb_spec,
        "lightgbm" = lightgbm_spec,
        "glm" = glm_spec,
        "mars" = mars_spec,
        "C5" = C5_spec,
        "knn" = knn_spec
      )
  )

# GAM cannot be paired with the other recipes, so we need to specify a seprate workflowset
check_wflow_contains_gam <- 
  "gen_additive_mod" %in% {
    wflow$info %>% 
      map(
        ~ .x %>% pull(model)
      ) %>% 
      unlist()
  }

if (check_wflow_contains_gam) {
  
  wflow_gam <- 
    workflow_set(
      preproc = 
        list(
          "recipe_base" = output_03$recipe_base
        ),
      models = 
        list(
          "gam" = gam_spec
        )
    )
  
  # -- GAM update formula
  wflow_gam %<>%
    update_workflow_model(
      id = "recipe_base_gam",
      spec = gam_spec,
      formula =
        target ~
        te(calc, urea) +
        s(osmo, k = 10) +
        s(gravity, k = 10) +
        s(ph, k = 10) +
        s(cond, k = 10)
    )
  
  # -- combine workflows
  wflow %<>% 
    bind_rows(
      wflow_gam
    )
  
}


# ---- Tune models -------------------------------------------------------------

my_metric_set <- 
  metric_set(
    roc_auc, # uses "hand_till" method for multiclass
    mn_log_loss
  )
  
tic("tuning/CV estimate")
cv_results <- 
  #wflow %>% 
  wflow %>% 
  workflow_map(
    # fit_resamples is used automatically when no parameters are tagged with tune()
    #"tune_race_anova", # faster, more crude, tuning compared to grid search
    seed = 1387,
    resamples = 
      vfold_cv(
        df_train, 
        v = 5,
        strata = prognosis,
        repeats = 5
      ),
    grid = 6,
    control = 
      control_grid( # use control_race to use the race methodology
        save_pred = TRUE,
        parallel_over = NULL, # automatically chooses betweeen "resamples" and "everything"
        save_workflow = TRUE # needs to be TRUE for stacking
      ),
    metrics = my_metric_set
  )
toc()
 
# ---- Compare models ----------------------------------------------------------

# -- Best model
cv_results %>% 
  autoplot(
    select_best = TRUE
  ) + 
  geom_text(
    aes(
      y = mean,
      label = wflow_id
    ), 
    angle = 90, 
    nudge_x = 0.15,
    color = "black",
    size = 3
  ) +
  theme(legend.position = "none")

# MAPk 
cv_results_mapk <- 
  wflow$wflow_id %>% 
  purrr::set_names() %>% 
  map_dfr(
    ~ cv_results %>% 
      collect_predictions() %>% 
      filter(wflow_id == .x) %>% 
      fun_mapk()
  ) %>% # the following is for compatability with exp tracking
  pivot_longer(
    cols = everything(),
    names_to = "wflow_id",
    values_to = "mean"
  ) %>% 
  mutate(
    .metric = "MAPk",
    std_err = NA
  ) %>% 
  select(
    wflow_id,
    .metric,
    mean,
    std_err
  )

# -- Parameter values for xgboost
tune_switch <- FALSE
if (tune_switch) {
  
  cv_results %>% 
    autoplot(
      id = "recipe_base_knn",
      metric = "roc_auc"
    )
  
}

# -- Analysis of residuals
cv_best_res <- 
  cv_results %>% 
  collect_predictions() %>% 
  filter(
    wflow_id == "recipe_base_xgboost"
  ) %>% 
  mutate(
    res = ifelse(target == 1, -log(.pred_1), -log(.pred_0))
  )

cv_best_res %>% 
  ggplot(aes(x = res)) +
  geom_histogram() +
  facet_grid(~ target)

# Inspect observations which were severely under- or overpredicted
df_large_res <- 
  cv_best_res %>% 
  filter(
    res > quantile(res, probs = 0.99)
  ) %>% 
  arrange(
    desc(res)
  ) 

df_res_inspect <- 
  df_train %>%
  dplyr::slice(df_large_res$.row) %>% 
  unique %>% 
  select(!c(id, target))  

df_res_inspect %>%
  plot_histogram()

# Compare to the EDA


# ---- Track experiment --------------------------------------------------------

exp_results <- 
  tibble(
    "time" = 
      rep_along(
        cv_results$wflow_id, 
        lubridate::now()
      ),
    "wflow_name" = cv_results$wflow_id,
    "val.or.test" = 
      rep_along(
        cv_results$wflow_id, 
        "CV results"
      )
  )

exp_results$recipe <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_preprocessor(
        x = cv_results,
        id = .x
      ) %>% 
      butcher::butcher() # reduce memory demand
  )

exp_results$model.spec <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ extract_spec_parsnip(
        x = cv_results,
        id = .x
      )
  )

exp_results$tuned.par <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>% 
  map(
    ~ cv_results %>% 
      extract_workflow_set_result(.x) %>% 
      select_best(metric = "roc_auc")
  )

exp_results$metrics <- 
  exp_results$wflow_name %>% 
  purrr::set_names() %>%
  map(
    ~ cv_results %>% 
      workflowsets::rank_results(select_best = TRUE) %>% 
      select(
        wflow_id,
        .metric,
        mean,
        std_err
      ) %>% 
      filter(
        wflow_id == .x
      ) # remember to rowbind MAPk 
  )

exp_results$description <- 
  rep_along(
    along = exp_results$wflow_name,
    "Tuning parameters"
  )

# -- Append to tracking data
df_expTracking %<>% 
  dplyr::rows_append(
    y = exp_results
  )

# -- save tracking data
save(
  df_expTracking,
  file = "./Output/04a_tracking.RData"
)

# ==== EXPORT ------------------------------------------------------------------------------------------ 

output_04 <- 
  list(
    "my_metric_set" = my_metric_set,
    "cv_results" = cv_results
  )

save(
  output_04,
  file = "./Output/04_output.RData"
)


