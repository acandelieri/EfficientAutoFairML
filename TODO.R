rm(list=ls()); graphics.off(); cat("\014")

library(tidyverse)
library(reticulate)
use_python(python="C:/Users/Antonio Candelieri/Documents/.conda/envs/py3.8",required=T)
conda_python("py3.8")


# Hyperparameters
py_run_string("hidden_layer_sizes = [16, 8, 4, 2]")
py_run_string("alpha = 0.1")
py_run_string("learning_rate_init = 0.01")
py_run_string("beta_1 = 0.99")
py_run_string("beta_2 = 0.99")
py_run_string("tol = 0.01")

py_run_file("kfold_stratified.py")


# .rs.restartR()