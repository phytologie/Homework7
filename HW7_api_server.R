## HW7_api_server.R
library(readr)
library(dplyr)
library(lubridate)
library(plumber)
library(jsonlite)

# Load data
train <- read_csv('train_dataset.csv.gz')
test  <- read_csv('test_dataset.csv.gz')

# ------------------------------------
# Data Wrangling 

train <- train %>% mutate(across(c(provider_id, address, specialty), as.factor))
test  <- test  %>% mutate(across(c(provider_id, address, specialty), as.factor))

train$appt_time <- ymd_hms(train$appt_time)
train$appt_made <- ymd(train$appt_made)
train$appt_hour <- hour(train$appt_time)
train$appt_day  <- wday(train$appt_time, label = TRUE)

test$appt_time <- ymd_hms(test$appt_time)
test$appt_made <- ymd(test$appt_made)
test$appt_hour <- hour(test$appt_time)
test$appt_day  <- wday(test$appt_time, label = TRUE)

train$days_between <- as.numeric(difftime(train$appt_time, train$appt_made, units = "days"))
test$days_between  <- as.numeric(difftime(test$appt_time, test$appt_made, units = "days"))

train <- train %>%
  group_by(provider_id) %>%
  mutate(provider_no_show_rate = mean(no_show, na.rm = TRUE)) %>%
  ungroup()

test <- test %>%
  group_by(provider_id) %>%
  mutate(provider_no_show_rate = mean(no_show, na.rm = TRUE)) %>%
  ungroup()

# Logistic Regression
mylogit <- glm(no_show ~ provider_id + address + age + specialty +
                 days_between + appt_hour + appt_day,
               data = train,
               family = "binomial")

# -------------------------------------------------------------
fix_new_data <- function(df) {
  
  df$provider_id <- factor(df$provider_id, levels = levels(train$provider_id))
  df$address     <- factor(df$address, levels = levels(train$address))
  df$specialty   <- factor(df$specialty, levels = levels(train$specialty))
  
  df$appt_time <- ymd_hms(df$appt_time)
  df$appt_made <- ymd(df$appt_made)
  df$appt_hour <- hour(df$appt_time)
  df$appt_day  <- wday(df$appt_time, label = TRUE)
  
  df$days_between <- as.numeric(difftime(df$appt_time, df$appt_made, units = "days"))
  
  return(df)
}

# -------------------------------------------------------------
#* @post /predict_prob
#* @parser json
#* @serializer rds
predict_prob <- function(body) {
  
  df <- as.data.frame(body)
  df_fixed <- fix_new_data(df)
  
  probs <- predict(mylogit, newdata = df_fixed, type = "response")
  
  # return numeric vector (not wrapped)
  probs
}

# -------------------------------------------------------------
#* @post /predict_class
#* @parser json
#* @serializer rds
predict_class <- function(body) {
  
  df <- as.data.frame(body)
  df_fixed <- fix_new_data(df)
  
  probs <- predict(mylogit, newdata = df_fixed, type = "response")
  class <- ifelse(probs > 0.5, 1, 0)
  
  # return numeric vector (not wrapped)
  class
}
