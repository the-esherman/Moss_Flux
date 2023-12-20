# Cleaning TinyTag and PAR data from each measurements
#
# Moss project - CO2 flux
# By Emil A.S. Andersen
#
#
#=======  ♣   Libraries   ♣ =======
library(tidyverse)
library(readxl)
library(lubridate)
#
#
#
#=======  ♠   TinyTag     ♠ =======
# Load TinyTag data
AirT_20200901 <- read_csv("Data_raw/Loggers/AirT 20200901 1510-1820.csv", skip = 5, col_names = c("Record", "Date_time", "AirT"))
AirT_20200902 <- read_csv("Data_raw/Loggers/AirT 20200902 1000-1600.csv", skip = 5, col_names = c("Record", "Date_time", "AirT"))
AirT_20200928 <- read_csv("Data_raw/Loggers/AirT 20200928 1340-1545.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20200929 <- read_csv("Data_raw/Loggers/AirT 20200929 1330-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20200930 <- read_csv("Data_raw/Loggers/AirT 20200930 1420-1740.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20201001 <- read_csv("Data_raw/Loggers/AirT 20201001 1500-1800.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20201103 <- read_csv("Data_raw/Loggers/AirT 20201103 1030-1530.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20201104 <- read_csv("Data_raw/Loggers/AirT 20201104 930-1500.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20201108 <- read_csv("Data_raw/Loggers/AirT 20201108 1200-1440.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210125 <- read_csv("Data_raw/Loggers/AirT 20210125-26 1200-1600.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210127 <- read_csv("Data_raw/Loggers/AirT 20210127 930-1600.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210128 <- read_csv("Data_raw/Loggers/AirT 20210128 900-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210129 <- read_csv("Data_raw/Loggers/AirT 20210129 900-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210130 <- read_csv("Data_raw/Loggers/AirT 20210130 900-1730.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210131 <- read_csv("Data_raw/Loggers/AirT 20210131 830-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210201 <- read_csv("Data_raw/Loggers/AirT 20210201 830-1400.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210322 <- read_csv("Data_raw/Loggers/AirT 20210322 730-1830.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210323 <- read_csv("Data_raw/Loggers/AirT 20210323 900-1630.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210324 <- read_csv("Data_raw/Loggers/AirT 20210324 700-1230.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210325 <- read_csv("Data_raw/Loggers/AirT 20210325 900-1800.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210326 <- read_csv("Data_raw/Loggers/AirT 20210326 900-1530.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210419 <- read_csv("Data_raw/Loggers/AirT 20210419 1130-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210420 <- read_csv("Data_raw/Loggers/AirT 20210420 1000-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210422 <- read_csv("Data_raw/Loggers/AirT 20210422 1100-1500.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210423 <- read_csv("Data_raw/Loggers/AirT 20210423 1100-1340.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210426 <- read_csv("Data_raw/Loggers/AirT 20210426-27 1000-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210428 <- read_csv("Data_raw/Loggers/AirT 20210428-29 1100-1230.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210430 <- read_csv("Data_raw/Loggers/AirT 20210430 930-1300.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210507 <- read_csv("Data_raw/Loggers/AirT 20210507 1200-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210510 <- read_csv("Data_raw/Loggers/AirT 20210510 830-1200.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210601 <- read_csv("Data_raw/Loggers/AirT 20210601-02 1430-1800.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210603 <- read_csv("Data_raw/Loggers/AirT 20210603 930-1730.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210604 <- read_csv("Data_raw/Loggers/AirT 20210604-05 1400-1500.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210605 <- read_csv("Data_raw/Loggers/AirT 20210605-06 1600-1800.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210702 <- read_csv("Data_raw/Loggers/AirT 20210702-03 1200-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210704 <- read_csv("Data_raw/Loggers/AirT 20210704 600-1500.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210705 <- read_csv("Data_raw/Loggers/AirT 20210705-07 1000-1600.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210827 <- read_csv("Data_raw/Loggers/AirT 20210827-28 1000-1600.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210829 <- read_csv("Data_raw/Loggers/AirT 20210829-30.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20210927 <- read_csv("Data_raw/Loggers/AirT 20210927-28 1100-1700.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20211103 <- read_csv("Data_raw/Loggers/AirT 20211103 830-1500.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20211104 <- read_csv("Data_raw/Loggers/AirT 20211104-06 900-1200.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
AirT_20211108 <- read_csv("Data_raw/Loggers/AirT 20211108-09 830-1430.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))

#
#
AirT_flux <- bind_rows(AirT_20200901, AirT_20200902, AirT_20200928, AirT_20200929, AirT_20200930, AirT_20201001, AirT_20201103, AirT_20201104, AirT_20201108, AirT_20210125, AirT_20210127, AirT_20210128, AirT_20210129, AirT_20210130, AirT_20210131, AirT_20210201, AirT_20210322, AirT_20210323, AirT_20210324, AirT_20210325, AirT_20210326, AirT_20210419, AirT_20210420, AirT_20210422, AirT_20210423, AirT_20210426, AirT_20210428, AirT_20210430, AirT_20210507, AirT_20210510, AirT_20210601, AirT_20210603, AirT_20210604, AirT_20210605, AirT_20210702, AirT_20210704, AirT_20210705, AirT_20210827, AirT_20210829, AirT_20210927, AirT_20211103, AirT_20211104, AirT_20211108)
#
#
AirT_flux <- AirT_flux %>% # Split temperature from the unit and Date and time. Set temperature as numeric
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  #unite(Date, Time, col = "Day_ID", sep = " ") %>%
  select(!"sec") %>%
  mutate(across(c(AirT), as.numeric))
#
write_csv(AirT_flux, "Data_clean/AirT_flux.csv", na = "NA")
#
#
#
#=======  ☼   PAR         ☼ =======
# Load PAR data

#
#
#
#=======  ■  { The End }  ■ =======