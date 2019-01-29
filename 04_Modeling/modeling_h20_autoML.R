# H20 MODELING ----

# 1. Setup ----

# load libraries
library(h2o)
library(tidyverse)
library(recipes)
library(tidyquant)
library(readxl)
library(cowplot)
library(fs)
library(glue)
library(hrbrthemes)

# set theme
theme_set(theme_tq(base_family = "Avenir", base_size = 11))

# load data
path_train <- "00_Data/telco_train.xlsx"
path_test  <- "00_Data/telco_test.xlsx"
path_train_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl  <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_train_definitions, 
                                  sheet = 1, 
                                  col_names = FALSE)

# Processing Pipline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(JobLevel, StockOptionLevel) %>%
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)


# 2. Modeling ----

# iniitalize h2o cluster
h2o.init()

#split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

#train_h2o <- split_h2o[[1]]
#valid_h2o <- split_h2o[[2]]

train_h2o <- as.h2o(train_tbl)
test_h2o  <- as.h2o(test_tbl) 

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  #validation_frame = valid_h2o,
  #leaderboard_frame = test_h2o,
  max_runtime_secs = 60,
  nfolds = 10
)

automl_models_h2o@leaderboard

automl_models_h2o@leader

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20190105_101905") 

automl_models_h2o@leaderboard %>%
  
  as_tibble() %>%
  slice(1) %>%
  pull(model_id) %>%
  
  h2o.getModel()

extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {
  
  model_name <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
}

automl_models_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(2) %>%
  h2o.getModel()

# Saving ad Loading ----

# top GLM
automl_models_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(1) %>%
  h2o.getModel() %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")

# top stacked ensemble
automl_models_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(3) %>%
  h2o.getModel() %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")

#top Deep Learning
automl_models_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(6) %>%
  h2o.getModel() %>%
  h2o.saveModel(path = "04_Modeling/h2o_models/")


# load model
h2o.loadModel(path = "04_Modeling/h2o_models/GLM_grid_0_AutoML_20190105_132902_model_0")

# Make Predictions ----
stacked_ensemble_h2o <- 
  h2o.loadModel(path = "04_Modeling/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190105_101905")

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = test_h2o)
predictions_tbl <- predictions %>% as.tibble()

# 3. Visualizing the Leaderboard ----

data_transformed <- automl_models_h2o@leaderboard %>%
  as.tibble() %>%
  select(model_id,auc,logloss) %>%
  mutate(model_type = str_split(model_id, "_", simplify = TRUE)[,1]) %>%
  slice(1:10) %>%
  rownames_to_column() %>%
  mutate(
    model_id = as_factor(model_id) %>% reorder(auc),
    model_type = as_factor(model_type)
  ) %>%
  gather(key = key, value = value, 
         -c(model_id, model_type, rowname), factor_key = TRUE) %>%
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev()
         )

data_transformed %>%
  ggplot(aes(x = value, y = model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  facet_wrap(~key, scales = "free_x") +
  theme(legend.position = "bottom") +
  labs(title = "h2o Leaderboard Metrics",
       subtitle = "Ordered by AUC",
       caption = "Source: h2o AutoML",
       x = "", y = "") +
  scale_color_tq()


# function to plot h2o leaderboard models
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"),
                                n_max = 10, size = 4, include_lbl = TRUE) {
  
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    select(model_id, auc, logloss) %>%
    mutate(model_type = str_split(model_id, "_", simplify = TRUE)[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(
      model_id = paste0(rowname, ". ", as.character(model_id)) %>% as_factor()
    )

  # Transformation
  
  if (order_by == "auc") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id = as_factor(model_id) %>% reorder(auc),
        model_type = as_factor(model_type)
      ) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname), 
             factor_key = TRUE)
    
  } else if (order_by == "logloss") {
    
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
        model_type = as_factor(model_type)
      ) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname), 
             factor_key = TRUE)
    
  } else {
    
    stop(paste0("order_by = '", order_by, "' is not a permitted option."))
    
  }
  
  g <- data_transformed_tbl %>%
    ggplot(aes(x = value, y = model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~key, scales = "free_x") +
    theme(legend.position = "bottom") +
    labs(title = "h2o Leaderboard Metrics",
         subtitle = glue("Ordered by: {toupper(order_by)}"),
         caption = "Source: h2o AutoML",
         x = "", y = "") +
    scale_color_tq()
  
  if (include_lbl) {
    g <- g + 
      geom_label(aes(label = round(value, 2), hjust = "inward"))
  }
  
  return(g)
  
}

automl_models_h2o@leaderboard %>%
  plot_h2o_leaderboard(order_by = "logloss", n_max = 15)

# Grid Search & CV ----

h2o_deeplearning <- 
  h2o.loadModel("04_Modeling/h2o_models/DeepLearning_0_AutoML_20190105_101905")

h2o.performance(h2o_deeplearning, test_h2o)

deeplearning_grid_01 <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "deeplearning_grid_01",
  
  # h2o.deeplearning() inputs
  x = x,
  y = y,
  training_frame = train_h2o,
  nfolds = 5,
  
  hyper_params = list(
    hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
    epochs = c(10, 50, 100)
  )
)

h2o.getGrid("deeplearning_grid_01", sort_by = "auc", decreasing = T)

deeplearning_grid_01_model_3 <- 
  h2o.getModel("deeplearning_grid_01_model_3")

deeplearning_grid_01_model_3 %>% h2o.auc(train = T, valid = F, xval = T)

deeplearning_grid_01_model_3 %>%
  h2o.performance(newdata = test_h2o)

# 4. Assessing Performance ----

deeplearning_h2o <- 
  h2o.loadModel("04_Modeling/h2o_models/DeepLearning_0_AutoML_20190105_101905")

glm_h2o <- 
  h2o.loadModel("04_Modeling/h2o_models/GLM_grid_0_AutoML_20190105_132902_model_0")

stacked_ensemble_h2o <- 
  h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190105_101905")

performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = test_h2o)

# Classifier Summary Metrics 

h2o.auc(performance_h2o)
h2o.giniCoef(performance_h2o)
h2o.logloss(performance_h2o)
h2o.confusionMatrix(performance_h2o)


# Precision vs. Recall Graph

performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble()

performance_tbl %>%
  filter(f1 == max(f1))

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue") +
  geom_line(aes(y = recall), color = "red") + 
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1"),
             linetype = "dashed", color = "grey") + 
  labs(title = "Precision vs. Recall",
       subtitle = "Stacked Ensemble",
       y = "Value",
       x = "Threshold")

# ROC Plot
path <- "04_Modeling/h2o_models/StackedEnsemble_AllModels_0_AutoML_20190105_101905"

load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  
  perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc)
}

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl %>%
  mutate(
    path = str_split(path, "/", simplify = T)[,3] %>% as_factor(),
    auc = auc %>% round(3),
    model = glue("{path} ({auc})") %>% as.character() %>% as_factor %>% fct_reorder(auc, .desc = T)
    ) %>%
  ggplot(aes(fpr, tpr, color = model, linetype = model)) +
  geom_line() +
  geom_abline(slope = 1, linetype = "dashed", color = "grey") +
  scale_color_tq() + 
  theme(legend.direction = "vertical") +
  labs(title = "ROC Plot",
       subtitle = "Top 3 Model Performance")


load_model_performance_metrics <- function(path, test_tbl) {
  
  model_h2o <- h2o.loadModel(path)
  
  perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, precision, recall, auc)
}

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h2o_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl %>%
  mutate(
    path = str_split(path, "/", simplify = T)[,3] %>% as_factor(),
    auc = auc %>% round(3),
    model = glue("{path} ({auc})") %>% as.character() %>% as_factor %>% fct_reorder(auc, .desc = T)
  ) %>%
  ggplot(aes(recall, precision, color = model, linetype = model)) +
  geom_line() +
  scale_color_tq() + 
  theme(legend.direction = "vertical") +
  labs(title = "Precision vs. Recall",
       subtitle = "Top 3 Model Performance")


# Gain and Lift

predictions_tbl <- h2o.predict(glm_h2o, newdata = as.h2o(test_tbl)) %>%
  as.tibble()

ranked_predictions_tbl <- predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes, Attrition) %>%
  arrange(desc(Yes))

calculated_gain_lift_tbl <- ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarize(
    cases = n(),
    responses = sum(Attrition == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  mutate(
    group = row_number()
  ) %>%
  select(group, cases, responses) %>%
  mutate(
    cumulative_responses = cumsum(responses),
    pct_responses = responses / sum(responses),
    gain = cumsum(pct_responses),
    cumulative_pct_cases = cumsum(cases) / sum(cases),
    lift = gain / cumulative_pct_cases,
    gain_baseline = cumulative_pct_cases,
    lift_baseline = gain_baseline / cumulative_pct_cases
  )

performance_h2o <- h2o.performance(glm_h2o, newdata = as.h2o(test_tbl))

gain_lift_tbl <- performance_h2o %>%
  h2o.gainsLift() %>%
  as.tibble()

gain_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain = cumulative_capture_rate)

gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = gain)) +
  geom_line() +
  geom_point(alpha = 1/3) +
  geom_abline(slope = 1, linetype = "dashed", color = "grey") +
  scale_color_tq() +
  scale_y_continuous(limits = c(0,1)) + 
  labs(
    title = "Gain Chart",
    subtitle = "GLM Model",
    x = "Percentage of Samples",
    y = "Percentage of Attrition"
  )

lift_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  rename(lift = cumulative_lift)

lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = lift)) +
  geom_line() +
  geom_point(alpha = 1/3) +
  geom_abline(intercept = 1, slope = 0, linetype = "dashed", color = "grey") +
  scale_color_tq() +
  labs(
    title = "Lift Chart",
    subtitle = "GLM Model",
    x = "Percentage of Samples",
    y = "Lift"
  )

# test params
h2o_leaderboard <- automl_models_h2o@leaderboard
newdata = test_tbl

# 5. Performance Visualization ----

plot_h2o_performance <- function(h2o_leaderboard, newdata, 
                                 order_by = c("auc", "logloss"),
                                 max_models = 5, size = 0.5){
  
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as.tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)
  
  h2o.no_progress()
  
  # 1. Model Metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata)) %>%
    unnest() %>%
    mutate(
      model_id = as_factor(model_id) %>%
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc = auc %>% round(3),
      logloss = logloss %>% round(4),
      model = glue("{model_id} (auc: {auc}; logloss: {logloss})") %>% 
        as.character() %>% as_factor() %>%
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE))
    )
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes_string("fpr", "tpr", color = "model", linetype = "model")) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 linetype = "dashed", color = "grey", size = 0.2) +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    scale_color_tq() + 
    theme(legend.direction = "vertical") +
    labs(title = "ROC Plot",
         x = "False Positive Rate",
         y = "True Positive Rate"
    )
  
  # 1B. Precision vs. Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes_string(x = "recall", y = "precision", 
                      color = "model", linetype = "model")) +
    geom_line(size = size) +
    scale_color_tq() + 
    labs(title = "Precision vs. Recall",
         x = "Recall",
         y = "Precision"
    ) +
    theme(legend.position = "none")
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
      rename(
        gain = cumulative_capture_rate,
        lift = cumulative_lift
      )
  }
  
  gains_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata)) %>%
    unnest() %>%
    mutate(
      model_id = as_factor(model_id) %>%
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc = auc %>% round(3),
      logloss = logloss %>% round(4),
      model = glue("{model_id} (auc: {auc}; logloss: {logloss})") %>% 
        as.character() %>% as_factor() %>%
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE))
    )
  
  # 2A. Gain Plot
  p3 <- gains_lift_tbl %>%
    ggplot(aes_string(x = "cumulative_data_fraction", y = "gain", 
                      color = "model", linetype = "model")) +
    geom_line() +
    #geom_point(alpha = 1/3) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 linetype = "dashed", color = "grey", size = 0.2) +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) + 
    labs(
      title = "Gain Chart",
      x = "Percentage of Samples",
      y = "Percentage of Attrition"
    ) +
    theme(legend.position = "none")
  
  
  # 2B. Lift Plot
  p4 <- gains_lift_tbl %>%
    ggplot(aes_string(x = "cumulative_data_fraction", y = "lift", 
                      color = "model", linetype = "model")) +
    geom_line() +
    #geom_point(alpha = 1/3) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 linetype = "dashed", color = "grey", size = 0.2) +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1))+
    labs(
      title = "Lift Chart",
      x = "Percentage of Samples",
      y = "Lift"
    )+
    theme(legend.position = "none")
  
  # Create dashboard via wowplot
  p_legend <- get_legend(p1)
  
  p1 <- p1 + theme(legend.position = "none")
  
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
  
  p_title <- ggdraw() + 
    draw_label("H2O Model Metrics", size = "18", fontface = "bold",
               colour = palette_light()[[1]])
  
  p_subtitle <- ggdraw() + 
    draw_label(glue("Ordered by {toupper(order_by)}"), size = "10",
               colour = palette_light()[[1]])
  
  ret <- plot_grid(p_title, p_subtitle, p, p_legend,
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
}

automl_models_h2o@leaderboard %>%
  plot_h2o_performance(newdata = test_tbl, order_by = "logloss", max_models = 4)


