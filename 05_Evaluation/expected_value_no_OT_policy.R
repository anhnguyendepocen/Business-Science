# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# NO OVERTIME POLICY ----

# 1. Setup ----

# load libraries
library(h2o)
library(tidyverse)
library(recipes)
library(tidyquant)
library(readxl)

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


# 3. Expected Value ----

# 3.1 Calculating Expected Value with OT ----

source("00_Scripts/assess_attrition.R")

predictions_with_OT_tbl <- auto_ml_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(test_tbl %>%
              select(EmployeeNumber, MonthlyIncome, OverTime)
  )

ev_with_OT_tbl <- predictions_with_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n = 1, 
      salary = MonthlyIncome * 12,
      net_revenue_per_employee = 250000)
  ) %>%
  mutate(
    cost_of_policy_change = 0
  ) %>%
  mutate(
    expected_attrition_cost = 
      Yes * (attrition_cost + cost_of_policy_change) +
      No * (cost_of_policy_change)
  )

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
  summarize(
    total_expected_attrtition_cost_0 = sum(expected_attrition_cost)
    )

# 3.2 Calculating Expected Value without OT ----

test_without_OT_tbl <- test_tbl %>%
  mutate(
    OverTime = fct_recode(OverTime, "No" = "Yes")
  )

predictions_without_OT_tbl <- auto_ml_leader %>%
  h2o.predict(newdata = as.h2o(test_without_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(test_tbl %>%
              select(EmployeeNumber, MonthlyIncome, OverTime),
            test_without_OT_tbl %>%
              select(OverTime)
  ) %>%
  rename(
    OverTime_0 = OverTime,
    OverTime_1 = OverTime1
    )

avg_OT_pct <- .10

ev_without_OT_tbl <- predictions_without_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n = 1, 
      salary = MonthlyIncome * 12,
      net_revenue_per_employee = 250000)
  ) %>%
  mutate(
    cost_of_policy_change = case_when(
      OverTime_0 == "Yes" & OverTime_1 == "No" ~ avg_OT_pct * attrition_cost,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    expected_attrition_cost = 
      Yes * (attrition_cost + cost_of_policy_change) +
      No * (cost_of_policy_change)
  )

total_ev_witout_OT_tbl <- ev_without_OT_tbl %>%
  summarize(
    total_expected_attrtition_cost_1 = sum(expected_attrition_cost)
  )

# 3.3 Savings Calculation ----

savings_tbl <- bind_cols(
  total_ev_with_OT_tbl, 
  total_ev_witout_OT_tbl
  ) %>%
  mutate(
    savings = total_expected_attrtition_cost_0 - total_expected_attrtition_cost_1,
    pct_savings = savings / total_expected_attrtition_cost_0
  )
  
