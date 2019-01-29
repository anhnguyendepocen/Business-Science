# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

# 1. Setup ----

# load libraries
library(h2o)
library(tidyverse)
library(recipes)
library(tidyquant)
library(readxl)
library(ggrepel)

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

auto_ml_leader

# 3. Primer: Working with Thresholds & Rates ----

performance_h2o <- auto_ml_leader %>%
  h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
  h2o.confusionMatrix()

rates_by_threshold_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble()

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr)

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>%
  mutate(
    key = fct_reorder2(key, threshold, value)
  ) %>%
  ggplot(aes(threshold, value, color = key)) +
  geom_point() +
  geom_smooth() +
  scale_color_tq() +
  theme(legend.position = "right") + 
  labs(
    title = "Expected Rates",
    x = "Threshold",
    y = "Value"
  )

# 4. Expected Value ----

# 4.1 Calculating Expected Value with OT ----

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

# 4.2 Calculating Expected Value with targeted OT ----

max_f1_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold

test_targeted_OT_tbl <- test_tbl %>%
  add_column(Yes = predictions_with_OT_tbl$Yes) %>%
  mutate(
    OverTime = case_when(
      Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)),
      TRUE ~ OverTime
    )
  ) %>%
  select(-Yes)


predictions_targeted_OT_tbl <- auto_ml_leader %>%
  h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(EmployeeNumber, MonthlyIncome, OverTime),
    test_targeted_OT_tbl %>%
      select(OverTime)
  ) %>%
  rename(
    OverTime_0 = OverTime,
    OverTime_1 = OverTime1
  )

avg_OT_pct <- .10

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
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
    cb_tn = cost_of_policy_change,
    cb_fp = cost_of_policy_change,
    cb_tp = cost_of_policy_change + attrition_cost,
    cb_fn = cost_of_policy_change + attrition_cost,
    
    expected_attrition_cost = 
      Yes * (tpr * cb_tp + fnr * cb_fn) +
      No  * (tnr * cb_tn + fpr * cb_fp)
  )

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
  summarize(
    total_expected_attrtition_cost_1 = sum(expected_attrition_cost)
  )

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
  total_ev_with_OT_tbl, 
  total_ev_targeted_OT_tbl
) %>%
  mutate(
    savings = total_expected_attrtition_cost_0 - total_expected_attrtition_cost_1,
    pct_savings = savings / total_expected_attrtition_cost_0
  )

# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

data <- test_tbl
h2o_model <- auto_ml_leader

calculate_savings_by_threshold <- function(data, h2o_model, 
                                           threshold = 0, 
                                           tnr = 0, fnr = 0, 
                                           tpr = 1, fpr = 1) {
  
  data_0_tbl <- as.tibble(data)

  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
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
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarize(
      total_expected_attrtition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value with targeted OT
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(pred_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes)
  
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1
    )
  
  avg_OT_pct <- .10
  
  ev_1_tbl <- pred_1_tbl %>%
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
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No  * (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarize(
      total_expected_attrtition_cost_1 = sum(expected_attrition_cost)
    )
  
  # 4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl, 
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrtition_cost_0 - total_expected_attrtition_cost_1,
      pct_savings = savings / total_expected_attrtition_cost_0
    )
  
  return(savings_tbl$savings)
  
}

# No OT Policy (default)

calculate_savings_by_threshold(data = test_tbl, h2o_model = auto_ml_leader)


# Targeted OT with max f1 score parameters

max_f1_savings <- calculate_savings_by_threshold(
  data = test_tbl, h2o_model = auto_ml_leader,
  threshold = max_f1_tbl$threshold,
  tnr = max_f1_tbl$tnr,
  fnr = max_f1_tbl$fnr,
  tpr = max_f1_tbl$tpr,
  fpr = max_f1_tbl$fpr
  )


# Targeted OT with min f1 score parameters

min_f1_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == min(f1)) %>%
  slice(1)

test_tbl %>% 
  calculate_savings_by_threshold(h2o_model = auto_ml_leader,
                                 threshold = min_f1_tbl$threshold,
                                 tnr = min_f1_tbl$tnr,
                                 fnr = min_f1_tbl$fnr,
                                 tpr = min_f1_tbl$tpr,
                                 fpr = min_f1_tbl$fpr)

# Do Nothing

test_tbl %>% 
  calculate_savings_by_threshold(h2o_model = auto_ml_leader,
                                 threshold = 1, # high threshold creates do nothing effect
                                 tnr = 1, fnr = 1,
                                 tpr = 0, fpr = 0)


# 5.2 Optimization ----

smpl <- seq(1, 220, length.out = 20) %>% round(0)

# preload function for non-changing paramters during iteration
partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = auto_ml_leader)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        threshold = threshold,
        tnr = tnr,
        fnr = fnr,
        tpr = tpr,
        fpr = fpr
      ),
      .f = partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = auto_ml_leader)
    )
  )

# 5.2.1 Expected Value visualization ----

rates_by_threshold_optimized_tbl %>%
  ggplot(aes(threshold, savings)) +
  geom_point(alpha = 1/2, color = palette_light()[[1]]) +
  geom_line(color = palette_light()[[1]]) +
  
  # optimal point
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(savings == max(savings))) +
  geom_label_repel(
    data = rates_by_threshold_optimized_tbl %>%
      filter(savings == max(savings)),
    aes(label = scales::dollar(savings)),
    color = palette_light()[[3]],
    nudge_y = max(rates_by_threshold_optimized_tbl$savings) * .1
  ) +
  
  # no OT policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == min(threshold))) +
  geom_label_repel(
    data = rates_by_threshold_optimized_tbl %>%
      filter(threshold == min(threshold)),
    aes(label = scales::dollar(savings)),
    color = palette_light()[[2]],
    nudge_y = -1e5 / 2
  ) +
  
  # do nothing policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == max(threshold))) +
  geom_label_repel(
    data = rates_by_threshold_optimized_tbl %>%
      filter(threshold == max(threshold)),
    aes(label = scales::dollar(savings)),
    color = palette_light()[[2]],
    nudge_y = 1e5
  ) +
  
  # f1 max
  geom_vline(
    xintercept = max_f1_tbl$threshold, 
    linetype = "dashed",
    color = palette_light()[[5]]
    ) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings),
           x = max_f1_tbl$threshold + .1, y = max_f1_savings, 
           color = palette_light()[[5]]) +
             
  # aesthetics
  expand_limits(x = c(-.1, 1.1), y = 6.5e5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, .2)
  ) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Optimization Results: Expected Savings Maximized at 14.7%",
    x = "Threshold",
    y = "Savings"
  )
  
  

# 6. Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

data <- test_tbl
h2o_model <- auto_ml_leader

calculate_savings_by_threshold_2 <- function(
  data, h2o_model, threshold = 0, 
  tnr = 0, fnr = 0,  tpr = 1, fpr = 1,
  avg_OT_pct = .10, net_revenue_per_employee = 250000
  ) {
  
  data_0_tbl <- as.tibble(data)
  
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1, 
        salary = MonthlyIncome * 12,
        # Changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(
      expected_attrition_cost = 
        Yes * (attrition_cost + cost_of_policy_change) +
        No * (cost_of_policy_change)
    )
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarize(
      total_expected_attrtition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value with targeted OT
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(pred_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes)
  
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1
    )
  
  # Changed in _2 ----
  avg_OT_pct <- avg_OT_pct
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1, 
        salary = MonthlyIncome * 12,
        # Changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_0 == "Yes" & OverTime_1 == "No" ~ avg_OT_pct * attrition_cost,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No  * (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarize(
      total_expected_attrtition_cost_1 = sum(expected_attrition_cost)
    )
  
  # 4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl, 
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrtition_cost_0 - total_expected_attrtition_cost_1,
      pct_savings = savings / total_expected_attrtition_cost_0
    )
  
  return(savings_tbl$savings)
  
}

test_tbl %>%
  calculate_savings_by_threshold_2(auto_ml_leader)

test_tbl %>%
  calculate_savings_by_threshold(auto_ml_leader)



# 6.2 Sensitivity Analysis ----

max_savings_rate_tbl <- rates_by_threshold_optimized_tbl %>%
  filter(savings == max(savings))



calculate_savings_by_threshold_2(
  data = test_tbl,
  auto_ml_leader, threshold = max_savings_rate_tbl$threshold,
  tnr = max_savings_rate_tbl$tnr, fnr = max_savings_rate_tbl$fnr,
  fpr = max_savings_rate_tbl$fpr, tpr = max_savings_rate_tbl$tpr
  )



calculate_savings_by_threshold_2_preloaded <- partial(
  calculate_savings_by_threshold_2,
  data = test_tbl,
  auto_ml_leader, threshold = max_savings_rate_tbl$threshold,
  tnr = max_savings_rate_tbl$tnr, 
  fnr = max_savings_rate_tbl$fnr,
  fpr = max_savings_rate_tbl$fpr, 
  tpr = max_savings_rate_tbl$tpr
)

calculate_savings_by_threshold_2_preloaded(
  avg_OT_pct = .1,
  net_revenue_per_employee = 280000
)

sensitivty_tbl <- list(
  avg_OT_pct = seq(0.05, .3, by = 0.05),
  net_revenue_per_employee = seq(200000, 400000, by = 50000)
) %>%
  cross_df() %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        avg_OT_pct = avg_OT_pct,
        net_revenue_per_employee = net_revenue_per_employee
      ),
      .f = calculate_savings_by_threshold_2_preloaded
    )
  )


sensitivty_tbl %>%
  ggplot(aes(avg_OT_pct, net_revenue_per_employee)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = savings %>% 
                   round(0) %>% scales::dollar())) +
  guides(
    fill = FALSE
  ) +
  scale_fill_gradient2(
    low  = palette_light()[[2]],
    mid  = "white",
    high = palette_light()[[1]],
    midpoint = 0
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.05, 30, .05)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(sccuracy = 1)
  ) +
  labs(
    title = "Profitability Heatmap: Expected Savings Sensitivty Analysis",
    subtitle = "Savings is more sensitive to average OT percentage",
    x = "Average Overtime Percentage",
    y = "Net Revenue Per Employee"
  )


# Challenge: People with NO Stock Options are Leaving ----

# Part 1: Find Optimal Threshold ----
avg_OT_pct <- .10
net_revenue_per_employee <- 250000
stock_option_cost <- 5000

data <- test_tbl
h2o_model <- auto_ml_leader

calculate_savings_by_threshold_3 <- function(
  data, h2o_model, threshold = 0, 
  tnr = 0, fnr = 0,  tpr = 1, fpr = 1,
  avg_OT_pct = .10,
  net_revenue_per_employee = 250000,
  stock_option_cost = 5000
) {
  
  data_0_tbl <- as.tibble(data)
  
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(data_0_tbl %>%
                # Changed in _3 ----
                select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1, 
        salary = MonthlyIncome * 12,
        # Changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(
      expected_attrition_cost = 
        Yes * (attrition_cost + cost_of_policy_change) +
        No * (cost_of_policy_change)
    )
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarize(
      total_expected_attrtition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value with targeted OT & stock
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(pred_0_tbl$OverTime)),
        TRUE ~ OverTime
      ),
      # Changed in _3 ----
      StockOptionLevel = case_when(
        (StockOptionLevel == 0) & (Yes >= threshold) ~ factor(1, levels = levels(pred_0_tbl$StockOptionLevel)),
        TRUE ~ StockOptionLevel
      )
    ) %>%
    select(-Yes)
  
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    # Changed in _3 ----
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel),
      data_1_tbl %>%
        select(OverTime, StockOptionLevel)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1,
      # Changed in _3 ----
      StockOptionLevel_0 = StockOptionLevel,
      StockOptionLevel_1 = StockOptionLevel1
    )
  
  # Changed in _2 ----
  avg_OT_pct <- avg_OT_pct
  # Changed in _3 ----
  stock_option_cost <- stock_option_cost
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1, 
        salary = MonthlyIncome * 12,
        # Changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    # Changed in _3 ----
    mutate(
      cost_OT = case_when(
        OverTime_0 == "Yes" & OverTime_1 == "No" ~ avg_OT_pct * attrition_cost,
        TRUE ~ 0
      ),
      cost_stock = case_when(
        StockOptionLevel_0 == 0 & StockOptionLevel_1 == 1 ~ stock_option_cost,
        TRUE ~ 0
      ),
      cost_of_policy_change = cost_OT + cost_stock
    ) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No  * (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarize(
      total_expected_attrtition_cost_1 = sum(expected_attrition_cost)
    )
  
  # 4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl, 
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrtition_cost_0 - total_expected_attrtition_cost_1,
      pct_savings = savings / total_expected_attrtition_cost_0
    )
  
  return(savings_tbl$savings)
  
}

# Do Nothing
test_tbl %>% 
  calculate_savings_by_threshold_3(h2o_model = auto_ml_leader,
                                   threshold = 1, # high threshold creates do nothing effect
                                   tnr = 1, fnr = 1,
                                   tpr = 0, fpr = 0)


max_f1_savings_2 <- calculate_savings_by_threshold_3(
  data = test_tbl, h2o_model = auto_ml_leader,
  threshold = max_f1_tbl$threshold,
  tnr = max_f1_tbl$tnr,
  fnr = max_f1_tbl$fnr,
  tpr = max_f1_tbl$tpr,
  fpr = max_f1_tbl$fpr
)


calculate_savings_by_threshold_3_preloaded <- 
  partial(calculate_savings_by_threshold_3, 
          data = test_tbl, 
          h2o_model = auto_ml_leader,
          avg_OT_pct = .10,
          net_revenue_per_employee = 250000,
          stock_option_cost = 5000
          )

rates_by_threshold_optimized_2_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        threshold = threshold,
        tnr = tnr,
        fnr = fnr,
        tpr = tpr,
        fpr = fpr
      ),
      .f = calculate_savings_by_threshold_3_preloaded
    )
  )



rates_by_threshold_optimized_2_tbl %>%
  ggplot(aes(threshold, savings)) +
  geom_point(alpha = 1/2, color = palette_light()[[1]]) +
  geom_line(color = palette_light()[[1]]) +
  
  # optimal point
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_2_tbl %>%
               filter(savings == max(savings))) +
  geom_label_repel(
    data = rates_by_threshold_optimized_2_tbl %>%
      filter(savings == max(savings)),
    aes(label = scales::dollar(savings)),
    color = palette_light()[[3]],
    nudge_y = max(rates_by_threshold_optimized_2_tbl$savings) * .1
  ) +
  
  # no OT policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_2_tbl %>%
               filter(threshold == min(threshold))) +
  geom_label_repel(
    data = rates_by_threshold_optimized_2_tbl %>%
      filter(threshold == min(threshold)),
    aes(label = scales::dollar(savings)),
    color = palette_light()[[2]],
    nudge_y = -1e5 / 2
  ) +
  
  # do nothing policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_2_tbl %>%
               filter(threshold == max(threshold))) +
  geom_label_repel(
    data = rates_by_threshold_optimized_2_tbl %>%
      filter(threshold == max(threshold)),
    aes(label = savings %>% round(0) %>% scales::dollar()),
    color = palette_light()[[2]],
    nudge_y = 1e5
  ) +
  
  # f1 max
  geom_vline(
    xintercept = max_f1_tbl$threshold, 
    linetype = "dashed",
    color = palette_light()[[5]]
  ) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings_2),
           x = max_f1_tbl$threshold + .1, y = max_f1_savings_2, 
           color = palette_light()[[5]]) +
  
  # aesthetics
  expand_limits(x = c(-.1, 1.1), y = 1e6) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, .2)
  ) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Optimization Results: Expected Savings Maximized at 14.7%",
    x = "Threshold",
    y = "Savings"
  )

# Part 2: Sensitivity Analysis ----

max_savings_rate_2_tbl <- rates_by_threshold_optimized_2_tbl %>%
  filter(savings == max(savings))


calculate_savings_by_threshold_3_preloaded <- partial(
  calculate_savings_by_threshold_3,
  data = test_tbl,
  auto_ml_leader, threshold = max_savings_rate_2_tbl$threshold,
  tnr = max_savings_rate_2_tbl$tnr, 
  fnr = max_savings_rate_2_tbl$fnr,
  fpr = max_savings_rate_2_tbl$fpr, 
  tpr = max_savings_rate_2_tbl$tpr
)

calculate_savings_by_threshold_3_preloaded(
)

sensitivty_2_tbl <- list(
  avg_OT_pct = seq(0.05, .3, by = 0.05),
  net_revenue_per_employee = 250000,
  stock_option_cost = seq(5000, 25000, by = 5000)
) %>%
  cross_df() %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        avg_OT_pct = avg_OT_pct,
        stock_option_cost = stock_option_cost,
        net_revenue_per_employee = net_revenue_per_employee
      ),
      .f = calculate_savings_by_threshold_3_preloaded
    )
  )


sensitivty_2_tbl %>%
  ggplot(aes(avg_OT_pct, stock_option_cost)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = savings %>% 
                   round(0) %>% scales::dollar())) +
  guides(
    fill = FALSE
  ) +
  scale_fill_gradient2(
    low  = palette_light()[[2]],
    mid  = "white",
    high = palette_light()[[1]],
    midpoint = 0
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.05, 30, .05)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(accuracy = 1),
    breaks = seq(5000, 25000, by = 5000)
  ) +
  labs(
    title = "Profitability Heatmap: Expected Savings Sensitivty Analysis",
    subtitle = "Savings is more sensitive to average stock options cost",
    x = "Average Overtime Percentage",
    y = "Average Stock Option Cost"
  )

# BONUS: Linear Regression Sensitivty Analysis ----

# lr <- lm(savings ~ I(avg_OT_pct * 100) + net_revenue_per_employee, data = sensitivty_tbl)
# summary(lr)
# 
# sensitivty_tbl
#
# sensitivty_2_tbl <- list(
#   avg_OT_pct = runif(10, 0.05, 0.3),
#   net_revenue_per_employee = rnorm(10, 250000, 250000 * .2)
# ) %>%
#   cross_df() %>%
#   mutate(
#     savings = pmap_dbl(
#       .l = list(
#         avg_OT_pct = avg_OT_pct,
#         net_revenue_per_employee = net_revenue_per_employee
#       ),
#       .f = calculate_savings_by_threshold_2_preloaded
#     )
#   )
#
# sens_recipe_obj <- recipe(
#   savings ~ avg_OT_pct + net_revenue_per_employee, data = sensitivty_2_tbl
#   ) %>%
#   step_center(all_predictors()) %>%
#   step_scale(all_predictors()) %>%
#   prep() 
# 
# sens_trans_tbl <- bake(sens_recipe_obj, new_data = sensitivty_2_tbl)
# lr_2 <- lm(savings ~ avg_OT_pct + net_revenue_per_employee, data = sens_trans_tbl)
# summary(lr_2)
  
