setwd("D:/Microbiome Key Taxa AI")
tryCatch({
  source("global.R")
  cat("Shiny starting on port 7790...\n")
  shiny::runApp(".", port = 7790, host = "127.0.0.1", launch.browser = FALSE)
}, error = function(e) cat("FATAL:", conditionMessage(e), "\n"))
