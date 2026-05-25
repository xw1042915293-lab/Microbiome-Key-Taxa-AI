setwd("D:/Microbiome Key Taxa AI")
source("renv/activate.R")

install.packages(c("ggplot2", "vegan", "microeco"), repos = "https://cloud.r-project.org")

# Snapshot so renv.lock includes Phase 2 deps.
renv::snapshot(prompt = FALSE)

cat("deps ok\n")

