# Analysis - moss flux
# By Emil A.S. Andersen
# 
#=======  ♣   Libraries     ♣ =======
library(tidyverse)
library(readxl)
library(lubridate)
library(car)
library(nlme)
#
#
#
#=======  ♠   Load data     ♠ =======
#
# Load each run
Run1 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 1")
Run2 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 2")
Run3 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 3")
Run4 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 4")
Run5 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 5")
Run6 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 6")
Run7 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 7")
Run8 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 8")
Run9 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 9")
Run10 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 10")
Run11 <- read_excel("Data_clean/Flux measures simplified.xlsx", col_names = TRUE, sheet = "Run 11")


# Can the Date variable (as numeric) be transformed to a Date? Yes
x <- Run1 %>%
  mutate(across(Date, ~ymd(.x)))

# Next: Combine all runs to one file
# Delete unnecessary variables
# pivot wide (?) to separate NEE and Respiration
# Calculate GPP from NEE and Respiration



#
#
#
#=======  ►   Functions     ◄ =======

#
#
#
#=======  ♦   Main data     ♦ =======

#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Q1            « -------

#
#
#
#=======  ♫♫  Graphs        ♫♫ =======
#-------  ♪   Environmental ♪ -------


#
#
#
#-------  ♪   Flux          ♪ -------

#
#
#
#-------  ♪   Outliers      ♪ -------

#
#
#
#=======  ■  { The End }    ■ =======