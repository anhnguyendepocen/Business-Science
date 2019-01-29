# DATA UNDERSTANDING ----

library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)
library(DataExplorer)

#theme_set(hrbrthemes::theme_ipsum())
theme_set(theme_classic())

# load data
path_train <- "00_Data/telco_train.xlsx"
path_train_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_train_definitions, 
                                  sheet = 1, 
                                  col_names = FALSE)

# Exploratory Data Analysis (EDA) ----

# Step 1. Data Summarizaion ----

glimpse(train_raw_tbl)

skim(train_raw_tbl)

# Character Data Type

train_raw_tbl %>%
  select_if(is.character) %>%
  glimpse()

train_raw_tbl %>%
  select_if(is.character) %>%
  map(unique)

train_raw_tbl %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

# Numeric Data Type

train_raw_tbl %>%
  select_if(is.numeric) %>%
  glimpse()

train_raw_tbl %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())  

# possible dicrete variables
train_raw_tbl %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  filter( value <= 10)


# Step 2: Data Visualization ----

# Personal Attributes

personal_vars <- c("Attrition", "Age", "Gender", "MaritalStatus", 
                   "NumCompaniesWorked", "Over18", "DistanceFromHome")        

train_raw_tbl %>%
  select(personal_vars) %>%
  ggpairs()

# custom ggpairs
train_raw_tbl %>%
  select(personal_vars) %>%
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1, 
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) + 
  theme(legend.position = "bottom")

# customer ggpairs function

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank")
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", 
              legend = 1, 
              diag = list(continuous = wrap("densityDiag", alpha = 0.5))) + 
      theme(legend.position = "bottom")
    
  } 
  
  return(g)
  
}

train_raw_tbl %>%
  select(personal_vars) %>%
  plot_ggpairs()

# Explore Features by Category

#  1. Personal/Descriptive: age, gender, marital status
train_raw_tbl %>%
  select(personal_vars) %>%
  plot_ggpairs()

#  2. Employment: department, job role, job level
train_raw_tbl %>%
  select(Attrition, contains("employee"), contains("department"), contains("job")) %>%
  plot_ggpairs()

#  3. Compensation: hourly rate, montly compensation, stock options
train_raw_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

#  4. Survey Results: satisfaction level, work-life balance
train_raw_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)

#  5. Performance: job involvement, performance rating
train_raw_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

#  6. Work-Life
train_raw_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

#  7. Training % Education
train_raw_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

#  8. Training % Education
train_raw_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)
