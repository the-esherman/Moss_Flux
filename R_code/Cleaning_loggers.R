# Cleaning TinyTag and PAR data from each measurements
#
# Moss project - CO2 flux
# By Emil A.S. Andersen
#
#
#=======  ♣   Libraries   ♣ =======
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
#
#
#
#=======  ♠   TinyTag     ♠ =======
# Load TinyTag data
# Files with only average temperature
AirT_20200901 <- read_csv("Data_raw/Loggers/AirT 20200901 1510-1820.csv", skip = 5, col_names = c("Record", "Date_time", "AirT"))
AirT_20200902 <- read_csv("Data_raw/Loggers/AirT 20200902 1000-1600.csv", skip = 5, col_names = c("Record", "Date_time", "AirT"))
#
# Load tinytags with similar file structure: min, max and avg. temperature
airT_path <- "Data_raw/Loggers/AirT/"
airT_folder <- dir(airT_path)
airT_list <- list()
#
# Loop through each file
for (file in airT_folder){
  
  # Load data: all 5TM soil temperature/moisture sensors
  airT_data <- read_csv(paste(airT_path,file, sep = ""), skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
  
  # Add file id to new column
  airT_data$id <- substr(file,6,13)
  
  # Name each file uniquely, based on filename. Add to list
  airT_list[[paste("AirT", substr(file,6,13), sep = "_")]] <- airT_data
  
  # Remove temp file
  rm(airT_data)
}
#
# Combine into one file
#airT_all_wide <- airT_list %>% reduce(full_join, by = "Date_time")
airT_all <- do.call(rbind, airT_list)
#
# Combine with 
AirT_flux <- bind_rows(airT_all, AirT_20200901, AirT_20200902)
#
# Clean files by isolating mean temperature from unit and splitting date and time
AirT_flux <- AirT_flux %>%
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  #unite(Date, Time, col = "Day_ID", sep = " ") %>%
  select(!c("sec", "id")) %>%
  mutate(across(c(AirT), as.numeric)) # Set temperature as numeric
#
# How many unique timepoints are there?
AirT_flux.2 <- AirT_flux %>%
  unite(Date, Time, col = "Day_ID", sep = " ") %>%
  distinct(Day_ID)
# Are there more or less than the original data?
length(AirT_flux.2$Day_ID) == length(AirT_flux$Date)
# Equal length, thus all temperature datapoints are from a unique timepoint.
#
# Remove last couple of data points from each file, as they were logging inside! - NOT DONE
# Does not matter if only used to couple measurement at specific time
#
# Graph data
AirT_flux %>%
  unite(Date, Time, col = "Date_time", sep = "T") %>%
  mutate(Date_time = ymd_hm(Date_time)) %>%
  ggplot(aes(x = Date_time, y = AirT)) + geom_point()
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
PAR_20210705_07 <- read_xls("Data_raw/Loggers/PAR flux/[20210705-07]EM14943 7jul21-1657.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric")) # Wetland, based on when retrieved
PAR_20210705_07.2 <- read_xls("Data_raw/Loggers/PAR flux/[20210705-07]EM14946 7jul21-1706.xls", skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric")) # Heath, based on when retrieved
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
PAR_Myran_202103_04 <- read_xls("Data_raw/Loggers/PAR flux/Odd format/[Myran 202103-04]EM14943 7apr21-1217.xls", skip = 3, col_names = c("Date_time", "X1", "X1_2", "X2", "X2_2", "X3", "X3_1", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
# Combine all PAR files
PAR_flux <- bind_rows(list(PAR_20210603, PAR_20210702_03, PAR_20210704, PAR_20210705_07, PAR_20210705_07.2, PAR_Field_20210322, PAR_Field_20210323, PAR_Field_20210324, PAR_Field_20210325, PAR_Field_20210326, PAR_FIeld_20210419, PAR_Field_20210420, PAR_Field_20210422, PAR_Field_20210423, PAR_Field_20210426_27, PAR_Field_20210428_29, PAR_Field_20210430, PAR_Field_20210507, PAR_Field_20210510, PAR_Field_20210601_02, PAR_Field_20210927_28, PAR_Field_H_20210827_30, PAR_Field_H_20210929_30, PAR_Field_H_20211103_09, PAR_Field_M_20210827_30, PAR_Field_M_20210929_30, PAR_Field_M_20211104_09, PAR_Myran_202103_04, PAR_Myran_20210604_06), .id = "id")
#
# Clean
# Remove unused sensors ports and separate date and time
PAR_flux <- PAR_flux %>%
  select(id, Date_time, PAR1, PAR2, PAR3, PAR4, PAR5) %>%
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
  select(id, Date_time, PAR) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  select(!"sec")
#
# For each set, keep only measurements from actual day of measure
# First set PAR on days outside interest at a negative value (PAR cannot be less than 0)
PAR_flux.2 <- PAR_flux %>%
  mutate(across(id, ~as.numeric(.x))) %>%
  mutate(PAR = case_when(id == 1 & Date != ymd(20210603) ~ -50,
                         id == 2 & Date != ymd(20210702) & Date != ymd(20210703) ~ -50,
                         id == 3 & Date != ymd(20210704) ~ -50,
                         id == 4 & (Date != ymd(20210705) & Date != ymd(20210706) & Date != ymd(20210707)) ~ -50,
                         id == 5 & (Date != ymd(20210705) & Date != ymd(20210706) & Date != ymd(20210707)) ~ -50, 
                         id == 6 & Date != ymd(20210322) ~ -50,
                         id == 7 & Date != ymd(20210323) ~ -50,
                         id == 8 & Date != ymd(20210324) ~ -50,
                         id == 9 & Date != ymd(20210325) ~ -50,
                         id == 10 & Date != ymd(20210326) ~ -50,
                         id == 11 & Date != ymd(20210419) ~ -50,
                         id == 12 & Date != ymd(20210420) ~ -50,
                         id == 13 & Date != ymd(20210422) ~ -50,
                         id == 14 & Date != ymd(20210423) ~ -50,
                         id == 15 & (Date != ymd(20210426) & Date != ymd(20210427)) ~ -50,
                         id == 16 & (Date != ymd(20210428) & Date != ymd(20210429)) ~ -50,
                         id == 17 & Date != ymd(20210430) ~ -50,
                         id == 18 & Date != ymd(20210507) ~ -50,
                         id == 19 & Date != ymd(20210510) ~ -50,
                         id == 20 & (Date != ymd(20210601) & Date != ymd(20210602)) ~ -50,
                         id == 21 & (Date != ymd(20210927) & Date != ymd(20210928)) ~ -50,
                         id == 22 & (Date != ymd(20210827) & Date != ymd(20210828) & Date != ymd(20210829) & Date != ymd(20210830)) ~ -50,
                         id == 23 & (Date != ymd(20210929) & Date != ymd(20210930)) ~ -50,
                         id == 24 & (Date != ymd(20211103) & Date != ymd(20211104) & Date != ymd(20211105) & Date != ymd(20211106) & Date != ymd(20211107) & Date != ymd(20211108) & Date != ymd(20211109)) ~ -50,
                         id == 25 & (Date != ymd(20210827) & Date != ymd(20210828) & Date != ymd(20210829) & Date != ymd(20210830)) ~ -50,
                         id == 26 & (Date != ymd(20210929) & Date != ymd(20210930)) ~ -50,
                         id == 27 & (Date != ymd(20211104) & Date != ymd(20211105) & Date != ymd(20211106) & Date != ymd(20211107) & Date != ymd(20211108) & Date != ymd(20211109)) ~ -50,
                         id == 28 & (Date != ymd(20210329) & Date != ymd(20210407)) ~ -50,
                         id == 29 & (Date != ymd(20210604) & Date != ymd(20210605) & Date != ymd(20210606)) ~ -50,
                         TRUE ~ PAR)) 
#
# Remove days outside interest (PAR < 0) and add habitat where it was measured
PAR_flux.3 <- PAR_flux.2 %>%
  filter(PAR >= 0) %>%
  # A few times a separate PAR sensor was placed on each site, rather than using one for both
  mutate(Habitat = case_when(id == 5 | id == 22 | id == 23 | id == 24 ~ "H", # id 5 or EM14943 is most likely wetland based on when it was retrieved (when 0-measurements begin compared to id 4)
                             id == 4 | id == 25 | id == 26 | id == 27 | id == 28 | id == 29 ~ "M", # id 4 or EM14946 is most likely heath based on when it was retrieved (when 0-measurements begin compared to id 5)
                             TRUE ~ "Both"))
#
# Check unique values (by habitat)
PAR_flux.4 <- PAR_flux.3 %>%
  unite(Date, Time, col = "Day_ID", sep = "_") %>%
  group_by(Habitat) %>%
  distinct(Day_ID, .keep_all = TRUE) %>%
  ungroup()
# No duplicates
#
# Plot
PAR_flux.3 %>%
  mutate(across(Date, ~ymd(.x))) %>%
  ggplot(aes(y = PAR, x = Date)) + geom_point() + facet_wrap(~Habitat)
#
# Save to csv
write_csv(PAR_flux.3, "Data_clean/PAR_flux.csv", na = "NA")
#
#
#
#=======  ☼   Other       ☼ =======
# Verify sensor habitat
#
# Two PAR sensors (connected to two different loggers) were measuring on the same days. Most likely one was placed in the wetland and another was placed in the heath. Trying to figure out which one was where by comparing them to eachother and to the "permanent" sensors placed at each location.
# This gave no conclusive results, but the time of retrieval hinted at which belonged where. Namely:
# EM14943 (or PAR_20210705_07) being in the Wetland
# EM14946 (or PAR_20210705_07.2) being in the Heath
PAR_habitat_202107.1 <- PAR_20210705_07 %>%
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
  select(!"sec") %>%
  filter(Date == ymd(20210705) | Date == ymd(20210706) | Date == ymd(20210707))
PAR_habitat_202107.2 <- PAR_20210705_07.2 %>%
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
  select(!"sec") %>%
  filter(Date == ymd(20210705) | Date == ymd(20210706) | Date == ymd(20210707))
# Combine
PAR_habitat_202107 <- PAR_habitat_202107.1 %>%
  left_join(PAR_habitat_202107.2, by = join_by(Date, hour, min)) %>%
  rename("PAR_EM14943" = PAR.x,
         "PAR_EM14946.2" = PAR.y)
# Summarise per hour to compare (in excel) with hourly measurements from "permanent" sensors
x <- PAR_habitat_202107 %>%
  summarise(EM14943 = mean(PAR_EM14943), EM14946.2 = mean(PAR_EM14946.2), .by = c(Date, hour))
# Save it to check against other loggers
write_csv(x, "Data_clean/PAR_20210705_07.csv", na = "NA")

#
#
#
#=======  ■  { The End }  ■ =======