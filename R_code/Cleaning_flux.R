# Cleaning something from raw files
# By Emil A.S. Andersen
# 
# rm(list=ls())
#=======  ♣   Libraries     ♣ =======
library(tidyverse)
library(readxl)
library(lubridate)
#
#
#
#=======  ♠   Load data     ♠ =======
# Load tinytags with similar file structure: min, max and avg. temperature
flux_path <- "Data_raw/Flux/Flux/"
flux_folder <- dir(flux_path)
flux_list <- list()

#
# Loop through each file
for (file in flux_folder){

  # Load data: all 5TM soil temperature/moisture sensors
  flux_data <- read_xlsx(paste(flux_path, file, sep = ""), sheet = "Blad1", skip = 2, col_names = TRUE, col_types = "numeric")
  
  # Add file id to new column
  flux_data$id <- substr(file,0,8)
  
  # Name each file uniquely, based on filename. Add to list
  flux_list[[paste("Flux", substr(file,0,8), sep = "_")]] <- flux_data
  
  # Remove temp file
  rm(flux_data)
}
#
# Combine into one file
flux_all <- do.call(rbind, flux_list)
#
#
#
#=======  ♠   Clean    ♠ =======
#
# Correct NaN in round 1
# Best approximation of values
flux_all.1 <- flux_all %>%
  rename("Plot" = ";Plot",
         "CO2_Ref" = `CO2 Ref`, # DC, concentration in ppm
         "mb_Ref" = `mb Ref`, # RH, relative humidity sensor, if attached
         "mbR_Temp" = `mbR Temp`, # Temperature of RH sensor
         "InputA" = `Input A`, # PAR from PAR sensor
         "InputB" = `Input B`, # RH, from chamber
         "InputC" = `Input C`, # Temperature of soil
         "InputD" = `Input D`, # DC, change in concentration in ppm
         "InputE" = `Input E`, # DT, change in time in sec
         "InputF" = `Input F`, # SR rate, g (CO2) m^2 Hour^-1
         "InputG" = `Input G`, # Not used
         "InputH" = `Input H`, # +/- SR rate, 00 if respiration (CO2 increase), 01 if CO2 decrease
         "Probe_Type" = `Probe Type`) %>%
  mutate(id = case_when(id == "Round 1 " ~ "Round 01",
                        id == "Round 2 " ~ "Round 02",
                        id == "Round 3 " ~ "Round 03",
                        id == "Round 4 " ~ "Round 04",
                        id == "Round 5 " ~ "Round 05",
                        id == "Round 6 " ~ "Round 06",
                        id == "Round 7 " ~ "Round 07",
                        id == "Round 8 " ~ "Round 08",
                        id == "Round 9 " ~ "Round 09",
                        TRUE ~ id)) %>%
  mutate(Day = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & Day == "NaN", 2, Day),
         Month = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & Month == "NaN", 9, Month),
         Hour = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & Hour == "NaN", 12, Hour),
         Min = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & Min == "NaN", 13, Min),
         CO2_Ref = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & CO2_Ref == "NaN", 413, CO2_Ref),
         mb_Ref = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & mb_Ref == "NaN", 13.2, mb_Ref),
         mbR_Temp = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & mbR_Temp == "NaN", 25.7, mbR_Temp),
         # Rest of the inputs:
         # A = 0
         # B = 40
         # C = 0
         # D = 21
         # E = 52
         # F = 0.2
         # G = 2
         # H = 0
         ATMP = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & ATMP == "NaN", 983, ATMP),
         Probe_Type = if_else(id == "Round 01" & Plot == 72 & RecNo == 12 & Probe_Type == "NaN", 8, Probe_Type))
#



#
#
#
#=======  ♠ Date and Time ♠ =======
# Extract the date and time
flux_all.2 <- flux_all.1 %>%
  mutate(Year = if_else(id == "Round 01" | id == "Round 02" | id == "Round 03", 2020, 2021),
         Sec = 0) %>%
  unite(Year, Month, Day, col = "Date", sep = "-") %>%
  unite(Hour, Min, Sec, col = "Time", sep = ":") %>%
  mutate(Date = ymd(Date),
         Time = hms::as_hms(Time))

flux_time <- flux_all.2 %>%
  select(Plot, Date, Time) %>%
  distinct(Date, Time, .keep_all = T) #%>%
  pivot_wider(names_from = Date, values_from = Time)

flux_time.2 <- flux_time %>%
  group_by(Date) %>%
  mutate(Early = hms::as_hms(min(Time)),
         Late = hms::as_hms(max(Time))) %>%
  distinct(Date, Early, Late)
  

#
#
#
#=======  ■  { The End }  ■ =======