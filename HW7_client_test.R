## HW7_client_test.R
library(httr)
library(jsonlite)
library(readr)

# Load the full test dataset
test <- read_csv("test_dataset.csv.gz")

# -------------------------------------------------------------
# Choose some rows to send to the API

test_sample <- test[1:5, ]

# -------------------------------------------------------------
# Convert to JSON
json <- toJSON(test_sample, dataframe = "rows", na = "string")

# -------------------------------------------------------------
# Test predict_prob
prob_res <- POST(
  url = "http://127.0.0.1:8000/predict_prob",
  body = json,
  encode = "raw",
  content_type_json()
)

probs <- unserialize(content(prob_res, "raw"))
print("Predicted probabilities:")
print(probs)

# -------------------------------------------------------------
# Test predict_class 
class_res <- POST(
  url = "http://127.0.0.1:8000/predict_class",
  body = json,
  encode = "raw",
  content_type_json()
)

classes <- unserialize(content(class_res, "raw"))
print("Predicted classes:")
print(classes)
