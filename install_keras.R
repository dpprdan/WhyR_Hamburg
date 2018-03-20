###-----------------------------------------------------------------------------
# Install libraries
packages <- c(
  "tidyverse",
  "jsonlite",
  "abind",
  "gridExtra",
  "pracma",
  "tokenizers",
  "stringr",
  "keras"
)
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
library(keras)
###-----------------------------------------------------------------------------
# Setup Keras (Make sure to install required prerequisites, before installing 
# Keras using the commands below)
# On Windows you should install Anaconda for Windows (Python 3.x version) prior
# to installing Keras: https://www.anaconda.com/download/
install_keras() # CPU version (recommended for beginners)
# install_keras(tensorflow = "gpu") # GPU version
