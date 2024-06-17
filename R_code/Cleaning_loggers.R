# Bryophyte experiment - CO2 flux
# Script author: Emil A.S. Andersen
#
# Cleaning TinyTag and PAR data from each measurements
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
  
  # Load data: TinyTag with min and max
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
# PAR with similar file structure: PAR on all 5 sensors
PAR_path <- "Data_raw/Loggers/PAR flux/"
PAR_folder <- dir(PAR_path)
PAR_list <- list()
#
# Loop through each file
for (file in PAR_folder){
  # Load data: all PAR sensors
  PAR_data <- read_xls(paste(PAR_path, file, sep = ""), skip = 3, col_names = c("Date_time", "PAR1", "PAR2", "PAR3", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric"))
  
  # Add file id to new column
  PAR_data$id <- str_replace_all(str_extract(file, ".*\\.xls"), c("\\s" = "_", "\\[" = "", "\\]" = "_",  "\\-" = ".", "\\.xls" = ""))
  
  # Name each file uniquely, based on filename. Add to list
  PAR_list[[str_replace_all(str_extract(file, ".*\\.xls"), c("\\s" = "_", "\\[" = "", "\\]" = "_",  "\\-" = ".", "\\.xls" = ""))]] <- PAR_data
  
  # Remove temp file
  rm(PAR_data)
}
#
# Combine into one file
#airT_all_wide <- airT_list %>% reduce(full_join, by = "Date_time")
PAR_all <- do.call(bind_rows, PAR_list)
#
# 5TM also set, but not used, only PAR on port 4 and 5!
PAR_Myran_202103_04 <- read_xls("Data_raw/Loggers/Odd format PAR flux/[Myran 202103-04]EM14943 7apr21-1217.xls", skip = 3, col_names = c("Date_time", "X1", "X1_2", "X2", "X2_2", "X3", "X3_1", "PAR4", "PAR5"), col_types = c("guess", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(Date_time, PAR4, PAR5) %>%
  mutate(id = "Myran_202103.04_EM14943_7apr21.1217")
#
# Combine
PAR_flux <- bind_rows(PAR_all, PAR_Myran_202103_04)
#
# Clean
# Remove unused sensors ports and separate date and time
PAR_flux.1 <- PAR_flux %>%
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
  select(Date_time, PAR, id) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  select(!"sec")
#
# For each set, keep only measurements from actual day of measure
# First set PAR on days outside interest at a negative value (PAR cannot be less than 0)
PAR_flux.2 <- PAR_flux.1 %>%
  mutate(PAR = case_when(id == "20210603_EM14943_3jun21.1824" & Date != ymd(20210603) ~ -50,
                         id == "20210702.03_EM14946_3jul21.1749" & Date != ymd(20210702) & Date != ymd(20210703) ~ -50,
                         id == "20210704_EM14946_4jul21.1457" & Date != ymd(20210704) ~ -50,
                         id == "20210705.07_EM14943_7jul21.1657" & (Date != ymd(20210705) & Date != ymd(20210706) & Date != ymd(20210707)) ~ -50,
                         id == "20210705.07_EM14946_7jul21.1706" & (Date != ymd(20210705) & Date != ymd(20210706) & Date != ymd(20210707)) ~ -50, 
                         id == "Field_20210322_EM14979_22mar21.2009" & Date != ymd(20210322) ~ -50,
                         id == "Field_20210323_EM14979_23mar21.1731" & Date != ymd(20210323) ~ -50,
                         id == "Field_20210324_EM14979_24mar21.1329" & Date != ymd(20210324) ~ -50,
                         id == "Field_20210325_EM14979_25mar21.1830" & Date != ymd(20210325) ~ -50,
                         id == "Field_20210326_EM14979_26mar21.1613" & Date != ymd(20210326) ~ -50,
                         id == "FIeld_20210419_EM14979_19apr21.1738" & Date != ymd(20210419) ~ -50,
                         id == "Field_20210420_EM14979_20apr21.1718" & Date != ymd(20210420) ~ -50,
                         id == "Field_20210422_EM14979_22apr21.1511" & Date != ymd(20210422) ~ -50,
                         id == "Field_20210423_EM14979_23apr21.1455" & Date != ymd(20210423) ~ -50,
                         id == "Field_20210426.27_EM14979_27apr21.1727" & (Date != ymd(20210426) & Date != ymd(20210427)) ~ -50,
                         id == "Field_20210428.29_EM14979_29apr21.1305" & (Date != ymd(20210428) & Date != ymd(20210429)) ~ -50,
                         id == "Field_20210430_EM14979_30apr21.1406" & Date != ymd(20210430) ~ -50,
                         id == "Field_20210507_EM14979_7maj21.1726" & Date != ymd(20210507) ~ -50,
                         id == "Field_20210510_EM14979_10maj21.1243" & Date != ymd(20210510) ~ -50,
                         id == "Field_20210601.02_EM14979_2jun21.1739" & (Date != ymd(20210601) & Date != ymd(20210602)) ~ -50,
                         id == "Field_Flux_20210927.28_EM14943_28sep21.1707" & (Date != ymd(20210927) & Date != ymd(20210928)) ~ -50,
                         id == "Field_H_20210827.30_EM14943_30aug21.1724" & (Date != ymd(20210827) & Date != ymd(20210828) & Date != ymd(20210829) & Date != ymd(20210830)) ~ -50,
                         id == "Field_H_20210929.30_EM14946_30sep21.1504" & (Date != ymd(20210929) & Date != ymd(20210930)) ~ -50,
                         id == "Field_H_20211103.09_EM14946_9nov21.1506" & (Date != ymd(20211103) & Date != ymd(20211104) & Date != ymd(20211105) & Date != ymd(20211106) & Date != ymd(20211107) & Date != ymd(20211108) & Date != ymd(20211109)) ~ -50,
                         id == "Field_M_20210827.30_EM14946_30aug21.1732" & (Date != ymd(20210827) & Date != ymd(20210828) & Date != ymd(20210829) & Date != ymd(20210830)) ~ -50,
                         id == "Field_M_20210929.30_EM14943_30sep21.1423" & (Date != ymd(20210929) & Date != ymd(20210930)) ~ -50,
                         id == "Field_M_20211104.09_EM14943_9nov21.1526" & (Date != ymd(20211104) & Date != ymd(20211105) & Date != ymd(20211106) & Date != ymd(20211107) & Date != ymd(20211108) & Date != ymd(20211109)) ~ -50,
                         id == "Myran_20210604.06_EM14946_6jun21.1803" & (Date != ymd(20210604) & Date != ymd(20210605) & Date != ymd(20210606)) ~ -50,
                         id == "Myran_202103.04_EM14943_7apr21.1217" & (Date != ymd(20210329) & Date != ymd(20210407)) ~ -50,
                         TRUE ~ PAR)) 
#
# Remove days outside interest (PAR < 0) and add habitat where it was measured
PAR_flux.3 <- PAR_flux.2 %>%
  filter(PAR >= 0) %>%
  # A few times a separate PAR sensor was placed on each site, rather than using one for both
  mutate(Habitat = case_when(id == "20210705.07_EM14946_7jul21.1706" | id == "Field_H_20210827.30_EM14943_30aug21.1724" | id == "Field_H_20210929.30_EM14946_30sep21.1504" | id == "Field_H_20211103.09_EM14946_9nov21.1506" ~ "H", # EM14946 is most likely heath based on when it was retrieved (when 0-measurements begin compared to EM14943)
                             id == "20210705.07_EM14943_7jul21.1657" | id == "Field_M_20210827.30_EM14946_30aug21.1732" | id == "Field_M_20210929.30_EM14943_30sep21.1423" | id == "Field_M_20211104.09_EM14943_9nov21.1526" | id == "Myran_202103.04_EM14943_7apr21.1217" | id == "Myran_20210604.06_EM14946_6jun21.1803" ~ "M", # EM14943 is most likely wetland based on when it was retrieved (when 0-measurements begin compared to id EM14946)
                             TRUE ~ "Both")) %>%
  select(!id)
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
#=======  ■  { The End }  ■ =======