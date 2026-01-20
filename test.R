# Clear workspace and console
rm(list = ls())
cat("\014") 
gc()

# Set working directory
setwd("C:/Users/jarno/GitHub/PinPath")

#devtools::create("PinPath")

devtools::document("C:/Users/jarno/GitHub/PinPath")
devtools::install("C:/Users/jarno/GitHub/PinPath")



devtools::test()