library(plumber)
pr <- plumb("HW7_api_server.R")
pr$run(port = 8000)
