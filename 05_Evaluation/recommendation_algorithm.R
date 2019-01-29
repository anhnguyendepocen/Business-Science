# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or laters is installed. If not restart & install.packages("recipes") to update.

# set theme
theme_set(theme_tq(base_family = "Avenir", base_size = 10))

# Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)



# 2.0 Correlation Analysis - Machine Readable ----
source("00_Scripts/plot_cor.R")

# 2.1 Recipes ----
train_readable_tbl %>% glimpse()

# Factor Names
factor_names <- c("JobLevel", "StockOptionLevel")

# Recipe
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(factor_names) %>%
  step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  step_dummy(all_nominal(), one_hot = T) %>%
  prep()

recipe_obj

train_corr_tbl <- bake(recipe_obj, new_data = train_readable_tbl)

train_corr_tbl %>% glimpse()

# Retrieve binning strategy
tidy(recipe_obj)

tidy(recipe_obj, number = 3)

# 2.2 Correlation Visualization ----

# Manipulate Data
train_corr_tbl %>% glimpse()

corr_level <- 0.06

correlation_results_tbl <- train_corr_tbl %>%
  select(-Attrition_No) %>%
  get_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T) %>%
  filter(abs(Attrition_Yes) >= corr_level) %>%
  mutate(
    relationship = case_when(
      Attrition_Yes > 0 ~ "Supports",
      TRUE ~ "Contradicts"
    )
  ) %>%
  mutate(feature_text = as.character(feature)) %>%
  separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>%
  mutate(feature_base = as_factor(feature_base) %>% fct_rev())

# check level order
correlation_results_tbl %>%
  mutate(levels = as.numeric(feature_base))

# Create Visualization

length_unique_grps <- correlation_results_tbl %>%
  pull(feature_base) %>%
  unique() %>%
  length()

correlation_results_tbl %>%
  ggplot(aes(Attrition_Yes, feature_base, color = relationship)) +
  geom_point() +
  geom_label(aes(label = feature), vjust = -0.5, size = 4) +
  expand_limits(x = c(-0.3, 0.3), y = c(1, length_unique_grps + 1)) + 
  scale_color_tq() + 
  labs(
    title = "Correlation Analysis: Strategy Recommendation Development",
    subtitle = "Discretizing Features to Help Identify a Strategy"
  )

# 3.0 Recommendation Strategy Development Worksheet ----

# COMPLETED IN EXCEL


# 4.0 Recommendation Algorithm Development ----

# 4.1 Personal Development (Mentorship, Education) ----

# YearsAtCompany
#   More tenured employees are more likely to stay, newer employees are more likely to leave
#   Tie promotion to professional development for faster advancement, mentor if YAC is low

# TotalWorkingYears
#   Less experienced employees are more likely to leave, more experienced more likely to stay
#   Provide lower working years to training and development activities

# YearsInCurrentRole
#   Lower years in current working role are more likely to leave
#   Identify opportunities to incentivize specialization or promote / mentorship role

# JobInvolvement
#   Employees with lower job involvement more likely to leave
#   Create personal development plan to increase involvement

# JobSatisfaction
#   Employees with lower job satisfaction more likely to leave
#   Identify factors of lower job satisfaction, mentorship roles for highly satisfied employees

# PerformanceRating
#   Not included in dataset, but can be part of strategy
# Low: Personal Development Plan, High: Mentoriship or Leadership Roles

# Strategies:

# (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformanceRating

# Training and Formation: YearsAtCompany, TotalWorkingYears

# Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction

# (Best Case) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating

# develop logic
train_readable_tbl %>%
  select(YearsAtCompany, TotalWorkingYears, YearsInCurrentRole,
         JobInvolvement, JobSatisfaction, PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    personal_development_strategy = case_when(
      # (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformanceRating
      PerformanceRating == 1 | 
        JobSatisfaction == 1 |
        JobInvolvement  <= 2            ~ "Create Personal Development Plan",
      
      # Training and Formation: YearsAtCompany, TotalWorkingYears
      YearsAtCompany < 3 |
        TotalWorkingYears < 6           ~ "Promote Training and Formation",
      
      # Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
      (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
        PerformanceRating >= 3 &
        JobSatisfaction == 4            ~ "Seek Mentorship Role",
      
      # (Best Case) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
      JobInvolvement >= 3 &
        JobSatisfaction >= 3 &
        PerformanceRating >= 3          ~ "Seek Leadership Role",
      
      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  )


train_readable_tbl %>%
  pull(JobInvolvement) %>%
  levels()

# get bins
tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "YearsAt"))

# 4.2 Professional Development (Promotion Readiness) ----

# JobLevel
#   Employees with Job Level 1 are leaving / Job Level 2 staying
#   Promote faster for high performers

# YearsAtCompany
#   YAC - High - Likely to stay / YAC - LOW - Likely to leave
#   Tie promotion if low to advance faster / Mentor if YAC low

# YearsInCurrentRole
#   More time in current role related to lower attrition
#   Incentivize specialize or promote 

# Additional Features 
#   JobInvolvement - Important for promotion readiness, incentivizes involvment for leaders and early promotion
#   JobSatisfaction - Important for specialization, incentivizes satisfaction for mentors
#   PerformanceRating - Important for any promotion


# Good Better Best Approach

# Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)

# Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating


# Implement Strategy Into Code
train_readable_tbl %>%
  select(JobLevel, YearsInCurrentRole, 
         JobInvolvement, JobSatisfaction, PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    professional_development_strategy = case_when(
      
      # Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)
      YearsInCurrentRole >= 2 &
        JobSatisfaction <= 2              ~ "Ready for Rotation",
      
      # Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 1 &
        YearsInCurrentRole >= 2 &
        JobInvolvement >= 3 &
        PerformanceRating >= 3            ~ "Ready for Promotion",
      
      # Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 2 &
        YearsInCurrentRole >= 2 &
        JobInvolvement >= 4 &
        PerformanceRating >= 3            ~ "Ready for Promotion",
      
      # Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 3 &
        YearsInCurrentRole >= 3 &
        JobInvolvement >= 4 &
        PerformanceRating >= 3            ~ "Ready for Promotion",
      
      # Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 4 &
        YearsInCurrentRole >= 4 &
        JobInvolvement >= 4 &
        PerformanceRating >= 3            ~ "Ready for Promotion",
      
      # Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
      YearsInCurrentRole >= 4 &
        JobSatisfaction >= 4 &
        PerformanceRating >= 3            ~ "Incentivize Specialization",
      
      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  )

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "YearsInCurrentRole"))


# 4.3 Work Environment ----

# OverTime
#   Overtime leads to more attrition

# EnvironmentSatisfaction
#   Employees with low environment satisfaction are more likely to leave

# WorkLifeBalance
#   Employees with a bad worklife balance are more likely to leave

# BusinessTravel
#   Employees with frequent business travel are more likely to leave, employes with no travel more likely to stay

# DistanceFromHome
#   Employees further away from work are more likely to leave

# Strategy:
# Improve Worklife Balance: OverTime, WorkLifeBalance
# Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
# Review Job Assignment: EnviornmentSatisfaction, YearsInCurrentRole
# Promote Job Engagement: JobInvolvement

# Implement Strategy Into Code
train_readable_tbl %>%
  select(OverTime, EnvironmentSatisfaction, WorkLifeBalance, BusinessTravel, 
         DistanceFromHome, YearsInCurrentRole, JobInvolvement) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    work_enviornment_strategy = case_when(
      # Improve Worklife Balance: OverTime, WorkLifeBalance
      OverTime == 2 |
        WorkLifeBalance == 1         ~ "Improve Work-Life Balance",
      # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
      (BusinessTravel == 3 |
         DistanceFromHome > 10) &
        WorkLifeBalance == 2         ~ "Monitor Business Travel",
      # Review Job Assignment: EnviornmentSatisfaction, YearsInCurrentRole
      EnvironmentSatisfaction == 1 &
        YearsInCurrentRole >= 2      ~ "Review Job Assignment",
      # Promote Job Engagement: JobInvolvement
      JobInvolvement <= 2            ~ "Promote Job Engagement",
      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  ) %>%
  count(work_enviornment_strategy)

tidy(recipe_obj, number = 4)

train_readable_tbl %>%
  pull(WorkLifeBalance) %>%
  levels()

# 5.0 Recommendation Function ----

data <- train_readable_tbl
employee_number <-  19

recommend_strategies <- function(data, employee_number) {
  
  data %>%
    filter(EmployeeNumber == employee_number) %>%
    mutate_if(is.factor, as.numeric) %>%
  
    # Personal Development Strategy
    mutate(
      personal_development_strategy = case_when(
        # (Worst Case) Create Personal Development Plan: JobInvolvement, JobSatisfaction, PerformanceRating
        PerformanceRating == 1 | 
          JobSatisfaction == 1 |
          JobInvolvement  <= 2            ~ "Create Personal Development Plan",
        
        # Training and Formation: YearsAtCompany, TotalWorkingYears
        YearsAtCompany < 3 |
          TotalWorkingYears < 6           ~ "Promote Training and Formation",
        
        # Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
        (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
          PerformanceRating >= 3 &
          JobSatisfaction == 4            ~ "Seek Mentorship Role",
        
        # (Best Case) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
        JobInvolvement >= 3 &
          JobSatisfaction >= 3 &
          PerformanceRating >= 3          ~ "Seek Leadership Role",
        
        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    
    # Professional Development Strategy
    mutate(
      professional_development_strategy = case_when(
        
        # Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)
        YearsInCurrentRole >= 2 &
          JobSatisfaction <= 2              ~ "Ready for Rotation",
        
        # Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 1 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 3 &
          PerformanceRating >= 3            ~ "Ready for Promotion",
        
        # Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 2 &
          YearsInCurrentRole >= 2 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3            ~ "Ready for Promotion",
        
        # Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 3 &
          YearsInCurrentRole >= 3 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3            ~ "Ready for Promotion",
        
        # Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
        JobLevel == 4 &
          YearsInCurrentRole >= 4 &
          JobInvolvement >= 4 &
          PerformanceRating >= 3            ~ "Ready for Promotion",
        
        # Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
        YearsInCurrentRole >= 4 &
          JobSatisfaction >= 4 &
          PerformanceRating >= 3            ~ "Incentivize Specialization",
        
        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    
    # Work Enviornment Strategy
    mutate(
      work_enviornment_strategy = case_when(
        # Improve Worklife Balance: OverTime, WorkLifeBalance
        OverTime == 2 |
          WorkLifeBalance == 1         ~ "Improve Work-Life Balance",
        # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
        (BusinessTravel == 3 |
           DistanceFromHome > 10) &
          WorkLifeBalance == 2         ~ "Monitor Business Travel",
        # Review Job Assignment: EnviornmentSatisfaction, YearsInCurrentRole
        EnvironmentSatisfaction == 1 &
          YearsInCurrentRole >= 2      ~ "Review Job Assignment",
        # Promote Job Engagement: JobInvolvement
        JobInvolvement <= 2            ~ "Promote Job Engagement",
        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    select(EmployeeNumber, contains("strategy"))
    
}

train_readable_tbl %>%
  recommend_strategies(employee_number = 30)
