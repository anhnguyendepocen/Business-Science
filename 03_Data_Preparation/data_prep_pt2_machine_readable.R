# DATA PREPARATION ----
# Machine Readable ----

# Setup ----

# libraries
library(tidyverse)
library(recipes)
library(readxl)
library(tidyquant)

# load data
path_train <- "00_Data/telco_train.xlsx"
path_test  <- "00_Data/telco_test.xlsx"
path_train_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_train_definitions, 
                                  sheet = 1, 
                                  col_names = FALSE)

# Processing Pipline
source("00_Scripts/data_processing_pipeline.R")

train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)


# Plot Faceted Histogram Function ----

plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE, fct_rev = FALSE,
                            fill = palette_light()[[3]],
                            color = "white", scale = "free") {
  
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE)
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale) +
    theme_tq()
  
  return(g)
  
}

train_raw_tbl %>%
  plot_hist_facet(bins = 10, ncol = 5)

# Data Pre-Processing with Recipes ----

# Plan: Correlation Analysis

# 1. Impute / Zero Variance ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())
# 2. Transformations ----
skewed_feature_names <- train_readable_tbl %>%
  select_if(is.numeric) %>%
  map_df(skewness) %>%
  gather(factor_key = T) %>%
  arrange(desc(value)) %>%
  filter(value > 0.8) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>%
  select(skewed_feature_names) %>%
  plot_hist_facet()

skewed_feature_names <- train_readable_tbl %>%
  select_if(is.numeric) %>%
  map_df(skewness) %>%
  gather(factor_key = T) %>%
  arrange(desc(value)) %>%
  filter(value > 0.8) %>%
  filter(! key %in% c("JobLevel", "StockOptionLevel")) %>%
  pull(key) %>%
  as.character()

factor_names <- c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names)

recipe_obj %>%
  prep() %>%
  bake(train_readable_tbl) %>%
  select(skewed_feature_names) %>%
  plot_hist_facet()

# 3. Discretize ----
# Can hurt correlations and not recommended unless needed specfically

# 4. Center / Scale ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# 5. Dummy Variables ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal())

# 6. Interactions / Engineered Features ----
# Advanced topic not needed for this project

# 7. Multivariate Transformations ----
# Advanced topic not needed for this project (e.g. PCA)

# Final Recipe ----
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal(), one_hot = T) %>%
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)


# Correlation Analysis ----

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = TRUE, fct_rev = TRUE) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    mutate_if(is.character, is.factor) %>%
    mutate_if(is.factor, is.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)
  
  if (fct_reorder) {
    data_cor <- data_cor %>%
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if (fct_rev) {
    data_cor <- data_cor %>%
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
  return(data_cor)
  
}

plot_cor <- function(data, target, fct_reorder = TRUE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive", 
       TRUE                  ~ "Negative") %>% as.factor())
  
  g <- data_cor %>%
    ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    theme_tq() +
    scale_color_manual(values = c(color_neg, color_pos))
  
  if (include_lbl) {
    g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  }
  
  return(g)
  
}

train_tbl %>%
  #select(Attrition_Yes, contains("JobRole")) %>%
  plot_cor(Attrition_Yes, include_lbl = F)

# Correlation Evalaution ----

#  1. Personal/Descriptive: age, gender, marital status
train_tbl %>%
  select(Attrition_Yes, Age, contains("Gender"), contains("MaritalStatus"), 
         NumCompaniesWorked, DistanceFromHome) %>%
  plot_cor(Attrition_Yes)

#  2. Employment: department, job role, job level
train_tbl %>%
  select(Attrition_Yes, contains("employee"), contains("department"), contains("job")) %>%
  plot_cor(Attrition_Yes)

#  3. Compensation: hourly rate, montly compensation, stock options
train_tbl %>%
  select(Attrition_Yes, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_cor(Attrition_Yes)

#  4. Survey Results: satisfaction level, work-life balance
train_tbl %>%
  select(Attrition_Yes, contains("satisfaction"), contains("life")) %>%
  plot_cor(Attrition_Yes)

#  5. Performance: job involvement, performance rating
train_tbl %>%
  select(Attrition_Yes, contains("performance"), contains("involvement")) %>%
  plot_cor(Attrition_Yes)

#  6. Work-Life
train_tbl %>%
  select(Attrition_Yes, contains("overtime"), contains("travel")) %>%
  plot_cor(Attrition_Yes)

#  7. Training % Education
train_tbl %>%
  select(Attrition_Yes, contains("training"), contains("education")) %>%
  plot_cor(Attrition_Yes)

#  8. Training % Education
train_tbl %>%
  select(Attrition_Yes, contains("years")) %>%
  plot_cor(Attrition_Yes)
