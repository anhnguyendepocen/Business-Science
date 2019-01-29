# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# load libraries
library(h2o)
library(tidyverse)
library(recipes)
library(tidyquant)
library(readxl)
library(lime)
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

# 2. Models ----

h2o.init()

auto_ml_leader <- 
  h2o.loadModel(path = "04_Modeling/h2o_models/GLM_grid_0_AutoML_20190105_132902_model_0")

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- auto_ml_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>% 
      select(Attrition, EmployeeNumber)
  )

test_tbl %>%
  slice(5) %>%
  glimpse()

# 3.2 Single Explanation ----

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model          = auto_ml_leader,
    bin_continuous = TRUE,
    n_bins         = 4,
    quantile_bins  = TRUE
  )
explainer

explaination <- test_tbl %>%
  slice(5) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer      = explainer,
    n_labels       = 1,
    n_features     = 8,
    n_permutations = 5000,
    kernel_width   = 1.5
  )

explaination %>%
  as.tibble() %>%
  select(feature:prediction)

plot_features(explanation = explaination, ncol = 1)

# 3.3 Multiple Explainations ----

explaination <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer      = explainer,
    n_labels       = 1,
    n_features     = 8,
    n_permutations = 5000,
    kernel_width   = 1.5
  )

explaination %>%
  as.tibble() %>%
  select(feature:prediction)

plot_features(explanation = explaination, ncol = 4)

plot_explanations(explaination)

# 3.5 Plot Features (TQ) ----

explaination_tq <- test_tbl %>%
  slice(5:8) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer      = explainer,
    n_labels       = 1,
    n_features     = 8,
    n_permutations = 5000,
    kernel_width   = 1.5
  )

explaination_tq %>%
  select(case, label, label_prob, model_r2, feature, feature_weight, feature_desc) %>%
  mutate(
    feature_desc = feature_desc %>% as_factor() %>% fct_reorder(feature_weight %>% abs()),
    label_prob = label_prob %>% round(2),
    model_r2 = model_r2 %>% round(2),
    weight_color = ifelse(feature_weight < 0, "Contradicts", "Supports") %>% as_factor(),
    case_info = glue("Case: {case}\nLabel: {label}\nProbability: {label_prob}\nExplanation Fit: {model_r2}")
  ) %>%
  ggplot(aes(x = feature_desc, y = feature_weight)) +
  geom_col(aes(fill = weight_color)) +
  coord_flip() +
  scale_fill_tq(labels = c("Supports", "Contradicts")) +
  facet_wrap(~ case_info, scales = "free_y", ncol = 1) +
  labs(x = "Feature",
       y = "Weight",
       fill = "")


plot_features_tq <- function(explanation, ncol = 1) {
  
  explanation_tbl <- explanation %>%
    as.tibble() %>%
    select(case, label, label_prob, model_r2, feature, feature_weight, feature_desc) %>%
    mutate(
      feature_weight = ifelse(label == "Yes", feature_weight, feature_weight * -1),
      feature_desc = as_factor(feature_desc) %>% fct_reorder(abs(feature_weight)),
      label_prob = label_prob %>% round(2),
      model_r2 = model_r2 %>% round(2),
      weight_color = ifelse(feature_weight < 0, "Stay", "Leave") %>% 
        as_factor() %>% fct_rev(),
      case_info = glue("Case: {case}\nLabel: {label}\nProbability: {label_prob}\nExplanation Fit: {model_r2}")
    )
  
  p <- explanation_tbl %>%
    ggplot(aes(x = feature_desc, y = feature_weight)) +
    geom_col(aes(fill = weight_color)) +
    coord_flip() +
    scale_fill_manual(values = c("#b2df8a", "#e31a1c")) +
    facet_wrap(~ case_info, scales = "free_y", ncol = ncol) +
    labs(x = "Feature",
         y = "Weight",
         fill = "")
  
  return(p)
}

explaination_tq %>%
  plot_features_tq(ncol = 2)


explaination_tq <- test_tbl %>%
  slice(1:30) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer      = explainer,
    n_labels       = 1,
    n_features     = 8,
    n_permutations = 5000,
    kernel_width   = 1.5
  )

explaination_tq %>%
  as.tibble() %>%
  select(case, label, feature, feature_value, feature_weight, feature_desc) %>%
  arrange(feature, feature_value) %>% 
  mutate(
    feature_desc = as_factor(feature_desc),
    case = case %>% as_factor(),
    feature_weight = ifelse(label == "Yes", 
                            feature_weight, feature_weight * -1),
    feature_desc = feature_desc %>% as.factor() %>% fct_rev()
  ) %>%
  ggplot(aes(x = case, y = feature_desc, fill = feature_weight)) +
  geom_tile() + 
  scale_fill_gradient2(low = "#b2df8a", high = "#e31a1c") +
  facet_wrap(~ label, ncol = 2) +
  labs(x = "Case",
       y = "Feature",
       fill = "") + 
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

plot_explanations_tq <- function(explanation, ncol = 2) {
  
  explanation_tbl <- explanation %>%
    as.tibble() %>%
    arrange(feature, feature_value) %>%
    select(case, label, feature_weight, feature_desc) %>%
    mutate(
      feature_desc = as_factor(feature_desc) %>% fct_rev(),
      feature_weight = ifelse(label == "Yes", 
                              feature_weight, feature_weight * -1),
      case = as_factor(case) %>% fct_reorder(as.numeric(case))
    )
  
  p <- explanation_tbl %>%
    ggplot(aes(x = case, y = feature_desc, fill = feature_weight)) +
    geom_tile() + 
    scale_fill_gradient2(low = "#b2df8a", high = "#e31a1c") +
    facet_wrap(~ label, ncol = ncol) +
    labs(x = "Case",
         y = "Feature",
         fill = "") + 
    theme(axis.text.x = element_text(angle = 0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  return(p)
  
}

explaination_tq %>%
  plot_explanations_tq()
