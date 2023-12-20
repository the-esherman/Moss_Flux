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
# Combine all TinyTag files
AirT_flux <- bind_rows(AirT_20200901, AirT_20200902, AirT_20200928, AirT_20200929, AirT_20200930, AirT_20201001, AirT_20201103, AirT_20201104, AirT_20201108, AirT_20210125, AirT_20210127, AirT_20210128, AirT_20210129, AirT_20210130, AirT_20210131, AirT_20210201, AirT_20210322, AirT_20210323, AirT_20210324, AirT_20210325, AirT_20210326, AirT_20210419, AirT_20210420, AirT_20210422, AirT_20210423, AirT_20210426, AirT_20210428, AirT_20210430, AirT_20210507, AirT_20210510, AirT_20210601, AirT_20210603, AirT_20210604, AirT_20210605, AirT_20210702, AirT_20210704, AirT_20210705, AirT_20210827, AirT_20210829, AirT_20210927, AirT_20211103, AirT_20211104, AirT_20211108)
#
# Clean files by isolating mean temperature from unit and splitting date and time
AirT_flux <- AirT_flux %>%
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  #unite(Date, Time, col = "Day_ID", sep = " ") %>%
  select(!"sec") %>%
  mutate(across(c(AirT), as.numeric)) # Set temperature as numeric
#
# Save to csv
write_csv(AirT_flux, "Data_clean/AirT_flux.csv", na = "NA")
#
#
#
#=======  ☼   PAR         ☼ =======
# Load PAR data
PAR_20210603 <- read_xls("Data_raw/Loggers/PAR flux/[20210603]EM14943 3jun21-1824.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_20210702_03 <- read_xls("Data_raw/Loggers/PAR flux/[20210702-03]EM14946 3jul21-1749.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_20210704 <- read_xls("Data_raw/Loggers/PAR flux/[20210704]EM14946 4jul21-1457.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_20210705_07 <- read_xls("Data_raw/Loggers/PAR flux/[20210705-07]EM14943 7jul21-1657.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_20210705_07 <- read_xls("Data_raw/Loggers/PAR flux/[20210705-07]EM14946 7jul21-1706.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210322 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210322]EM14979 22mar21-2009.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210323 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210323]EM14979 23mar21-1731.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210324 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210324]EM14979 24mar21-1329.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210325 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210325]EM14979 25mar21-1830.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210326 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210326]EM14979 26mar21-1613.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_FIeld_20210419 <- read_xls("Data_raw/Loggers/PAR flux/[FIeld 20210419]EM14979 19apr21-1738.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210420 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210420]EM14979 20apr21-1718.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210422 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210422]EM14979 22apr21-1511.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210423 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210423]EM14979 23apr21-1455.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210426_27 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210426-27]EM14979 27apr21-1727.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210428_29 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210428-29]EM14979 29apr21-1305.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210430 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210430]EM14979 30apr21-1406.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210507 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210507]EM14979 7maj21-1726.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210510 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210510]EM14979 10maj21-1243.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210601_02 <- read_xls("Data_raw/Loggers/PAR flux/[Field 20210601-02]EM14979 2jun21-1739.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_20210927_28 <- read_xls("Data_raw/Loggers/PAR flux/[Field Flux 20210927-28]EM14943 28sep21-1707.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_H_20210827_30 <- read_xls("Data_raw/Loggers/PAR flux/[Field H 20210827-30]EM14943 30aug21-1724.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_H_20210929_30 <- read_xls("Data_raw/Loggers/PAR flux/[Field H 20210929-30]EM14946 30sep21-1504.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_H_20211103_09 <- read_xls("Data_raw/Loggers/PAR flux/[Field H 20211103-09]EM14946 9nov21-1506.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_M_20210827_30 <- read_xls("Data_raw/Loggers/PAR flux/[Field M 20210827-30]EM14946 30aug21-1732.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_M_20210929_30 <- read_xls("Data_raw/Loggers/PAR flux/[Field M 20210929-30]EM14943 30sep21-1423.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Field_M_20211104_09 <- read_xls("Data_raw/Loggers/PAR flux/[Field M 20211104-09]EM14943 9nov21-1526.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
PAR_Myran_20210604_06 <- read_xls("Data_raw/Loggers/PAR flux/[Myran 20210604-06]EM14946 6jun21-1803.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
# 5TM also set, but not used, only PAR on port 4 and 5!
PAR_Myran_202103_04 <- read_xls("Data_raw/Loggers/PAR flux/[Myran 202103-04]EM14943 7apr21-1217.xls", skip = 3, col_names = c("Date_time", "X1", "X1_2", "X2", "X2_2", "X3", "X3_1", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
# Combine all PAR files
PAR_flux <- bind_rows(PAR_20210603, PAR_20210702_03, PAR_20210704, PAR_20210705_07, PAR_20210705_07, PAR_Field_20210322, PAR_Field_20210323, PAR_Field_20210324, PAR_Field_20210325, PAR_Field_20210326, PAR_FIeld_20210419, PAR_Field_20210420, PAR_Field_20210422, PAR_Field_20210423, PAR_Field_20210426_27, PAR_Field_20210428_29, PAR_Field_20210430, PAR_Field_20210507, PAR_Field_20210510, PAR_Field_20210601_02, PAR_Field_20210927_28, PAR_Field_H_20210827_30, PAR_Field_H_20210929_30, PAR_Field_H_20211103_09, PAR_Field_M_20210827_30, PAR_Field_M_20210929_30, PAR_Field_M_20211104_09, PAR_Myran_202103_04, PAR_Myran_20210604_06)
#
# Clean
# Remove
PAR_flux <- PAR_flux %>%
  select(Date_time, PAR1, PAR2, PAR3, PAR4, PAR5) %>%
  mutate(PAR1 = replace_na(PAR1, 0),
         PAR2 = replace_na(PAR2, 0),
         PAR3 = replace_na(PAR3, 0),
         PAR4 = replace_na(PAR4, 0),
         PAR5 = replace_na(PAR5, 0)) %>%
  mutate(PAR = case_when(PAR1 != 0 & PAR2 == 0 & PAR3 == 0 & PAR4 == 0 & PAR5 == 0 ~ PAR1,
                         PAR1 == 0 & PAR2 != 0 & PAR3 == 0 & PAR4 == 0 & PAR5 == 0 ~ PAR2,
                         PAR1 == 0 & PAR2 == 0 & PAR3 != 0 & PAR4 == 0 & PAR5 == 0 ~ PAR3,
                         PAR1 == 0 & PAR2 == 0 & PAR3 == 0 & PAR4 != 0 & PAR5 == 0 ~ PAR4,
                         PAR1 == 0 & PAR2 == 0 & PAR3 == 0 & PAR4 == 0 & PAR5 != 0 ~ PAR5,
                         TRUE ~ 0)) %>%
  select(Date_time, PAR) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  select(!"sec")
#
# Save to csv
write_csv(PAR_flux, "Data_clean/PAR_flux.csv", na = "NA")
#
#
#
#=======  ■  { The End }  ■ =======