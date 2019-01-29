# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl

new_dept_jobrole_tbl <- dept_jobrole_tbl %>%
  left_join(productivity_cost_by_role_tbl, by = c("Department" = "Department",
                                                  "JobRole" = "JobRole")) 

# Q1: Which Job Role has the highest total cost of attrition? ----
# ANSWER: Sales Executive

new_dept_jobrole_tbl %>%
  count(Department, JobRole, Salary_Average, Revenue_Average, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(
    attrition_col = Attrition,
    attrition_value = "Yes",
    baseline_pct = kpi_industry_turnover_pct
  ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_Average)
  ) %>%
  arrange(desc(cost_of_attrition))


# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----
# ANSWER: 2276492

new_dept_jobrole_tbl %>%
  count(Department, JobRole, Salary_Average, Revenue_Average, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(
    attrition_col = Attrition,
    attrition_value = "Yes",
    baseline_pct = kpi_industry_turnover_pct
  ) %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_Average)
  ) %>%
  filter(str_c(Department, JobRole, sep = ": ") %in% c("Research & Development: Research Scientist"))

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
# ANSWER: 0.861

new_dept_jobrole_tbl %>%
  count(JobRole, Salary_Average, Revenue_Average, Attrition) %>%
  count_to_pct(JobRole) %>%
  filter(Attrition == "Yes") %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_Average)
  ) %>%
  mutate(pct_attrition = cost_of_attrition / sum(cost_of_attrition)) %>%
  top_n(cost_of_attrition, n = 4) %>%
  summarise(top_4 = sum(pct_attrition))

# Q4. Which Department has the highest total cost of attrition? ----
# ANSWER: Sales

new_dept_jobrole_tbl %>%
  count(Department, Salary_Average, Revenue_Average, Attrition) %>%
  count_to_pct(Department) %>%
  filter(Attrition == "Yes") %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_Average)
  ) %>%
  group_by(Department) %>%
  summarise(cost_of_attrition = sum(cost_of_attrition)) %>%
  top_n(cost_of_attrition, n = 1)

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----
# ANSWER: 0.372

new_dept_jobrole_tbl %>%
  count(Department, Salary_Average, Revenue_Average, Attrition) %>%
  count_to_pct(Department) %>%
  filter(Attrition == "Yes") %>%
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n,
                                                 salary = Salary_Average,
                                                 net_revenue_per_employee = Revenue_Average)
  ) %>%
  group_by(Department) %>%
  summarise(cost_of_attrition = sum(cost_of_attrition)) %>%
  mutate(pct_attrition = cost_of_attrition / sum(cost_of_attrition))
