library(cfbfastR)
library(dplyr)
options(tidymodels.dark = TRUE)
library(tidymodels)

max_season <- cfbfastR:::most_recent_cfb_season()
seasons <- 2014:max_season
valid_fbs_teams <- cfbfastR::load_cfb_teams() %>%
    filter(classification == 'fbs') %>%
    select(
        team_id,
        school,
        abbreviation
    )

plays_set <- readRDS("./ep_training_data_2025-07-26.RDS") %>%
    dplyr::mutate(
        game_id = as.character(game_id)
    )



set.seed(2018)

ep_test <- plays_set %>%
    dplyr::select(-game_id) %>%
    dplyr::filter(season >= 2024)

ep_train <- plays_set %>%
    dplyr::select(-game_id) %>%
    dplyr::filter(season < 2024)

train_labels <- ep_train %>%
    dplyr::select(label)

train_weights <- ep_train %>%
    dplyr::select(Total_W_Scaled)


# get rid of extra columns
train_data <- ep_train %>%
    dplyr::select(-season) %>%
    dplyr::mutate(
        Total_W_Scaled = hardhat::importance_weights(Total_W_Scaled),
        # prev_label = label,
        label = as.factor(label),
        is_home = as.integer(is_home)
    )
# check if this is 0 just in case
#nrow(train_data[train_data$prev_label != train_data$label, ])



ep_model <-
    boost_tree(
        mtry = tune(),
        trees = 2000,
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        sample_size = tune(),
        stop_iter = 100,
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")


ep_recipe <- recipe(
    label ~ .,
    data = train_data
)

ep_workflow <- workflow() %>%
    add_recipe(ep_recipe) %>%
    add_case_weights(Total_W_Scaled) %>%
    add_model(ep_model)

xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train_data),
    learn_rate(),
    size = 30
)

future::plan("multisession")

xgb_res <- tune_grid(
    ep_workflow,
    resamples = ep_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE, allow_par = T, verbose = T),
)

xgb_res

future::plan("sequential")

show_best(xgb_res, "roc_auc")

best_auc <- select_best(xgb_res, "roc_auc")
best_auc


final_xgb <- finalize_workflow(
    ep_workflow,
    best_auc
)

final_xgb

#
# final_res <- last_fit(final_xgb, vb_split)
#
# collect_metrics(final_res)
#
#
# dplyr::rename(
# eta = learn_rate,
# gamma = loss_reduction,
# subsample = sample_size,
# colsample_bytree = mtry,
# max_depth = tree_depth,
# min_child_weight = min_n
# )
