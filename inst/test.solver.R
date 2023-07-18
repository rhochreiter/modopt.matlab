# install.packages("ROI.plugin.ipop")
# source("test.quadprog.R")

library(ROI.plugin.ipop)

options <- list()
options$solver = "ipop"

solution <- quadprog(H, f, NULL, NULL, Aeq, beq, lb, ub, options = options)
portfolio <- solution$x
