# _________________________________________________________________________
# 00-setupxx.R
# _________________________________________________________________________
# August 2025
# Setup script, clear environment, set working directory, load packages 
# _________________________________________________________________________

#rm(list = ls())

setwd("C:/Users/morga/OneDrive - The University of Waikato/Directed Study")
getwd()

# Load packages
library(tidyverse)     # Includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)        # Reading Excel files
library(janitor)       # Cleaning column names etc.
library(scales)        # Formatting axes in plots
library(skimr)         # Summary statistics
library(patchwork)     # Combining ggplot plots
library(glue)          # Interpolated strings
library(lubridate)     # Dates 
library(slider)        # Rolling window operations
library(cowplot) 
library(dplyr)
library(grid)
library(magick)        # GIF Maker
