# Bryophyte experiment - CO2 flux
# Script author: Emil A.S. Andersen
#
# Analysis - bryophyte flux
# 
#=======  ♣   Libraries     ♣ =======
#
# To get months in the right format (i.e. not in whatever local the computer has, e.g. Swedish)
Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
#
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
library(car)
library(nlme)
library(vegan)
library(gridExtra)
library(cowplot)
library(ggrepel)
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
#
# Time interval
Time_flux <- read_csv("Data_clean/Flux_time.csv", col_names = TRUE)
#
#
# Environmental data
# Air temperature at flux measurements
AirT_flux <- read_csv("Data_clean/AirT_flux.csv", col_names = TRUE)
# Air temperature in Wetland habitat
AirT_wetland <- read_csv("Data_clean/AirT_wetland.csv", col_names = TRUE)
#
# PAR at measurement
PAR_flux <- read_csv("Data_clean/PAR_flux.csv", col_names = TRUE)
#
# EM50 logger data (Soil temperature and moisture and PAR)
# Cleaned from the N2-fixation script
EM50_Heath <- read_csv("Data_clean/Heath_EM50.csv", col_names = TRUE)
EM50_Mire <- read_csv("Data_clean/Wetland_EM50.csv", col_names = TRUE)
#
# MP, or Round in months
measuringPeriod <- c("Sept",	"Oct",	"Nov",	"Feb",	"Mar",	"May",	"Jun",	"Jul",	"Sept",	"Oct",	"Nov")
#
#
#
#=======  ►   Functions     ◄ =======
# From http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm),
                           max  = max    (xx[[col]], na.rm=na.rm),
                           min  = min    (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult 
  return(datac)
}
#
# Inverse hyperbolic sine function
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}
#
#
#
#=======  ♦   Main data     ♦ =======
#
# Combine
Flux_data <- bind_rows(list(Run1, Run2, Run3, Run4, Run5, Run6, Run7, Run8, Run9, Run10, Run11), .id = "id") %>%
  mutate(across(Date, ~ymd(.x))) %>%
  # Fix a few mistakes
  mutate(Block = case_when(id == 10 & (New_plot == 59 | New_plot == 60 | New_plot == 61 | New_plot == 62) ~ "P",
                           TRUE ~ Block))
#  
# Extract snow-depths
Flux_data_snow <- Flux_data %>%
  filter(Block != "None") %>%
  select(Round, Date, Block, Species, Snow_depth_cm) %>%
  filter(!is.na(Snow_depth_cm)) %>%
  group_by(Date, Block, Species) %>%
  distinct(Snow_depth_cm, .keep_all = T) %>%
  ungroup()
#
# Get light and dark flux in each their columns (NEE and Respiration respectively)
Flux_data.1 <- Flux_data %>%
  # Remove bad values
  filter(is.na(Remove)) %>%
  select(Round, Date, Block, Species, Light_Dark, f1_lin_umol) %>%
  filter(Block != "Green" & Block != "None") %>%
  pivot_wider(names_from = Light_Dark, values_from = f1_lin_umol) %>%
  rename("NEE" = L,
         "Resp" = D) %>%
  mutate(Resp = if_else(Resp < 0, 0, Resp)) %>%
  #mutate(diff = if_else(NEE > Resp, 1, 0)) %>%
  mutate(GPP = Resp - NEE)


x <- Flux_data.1 %>%
  mutate(GPP = if_else(GPP <= 0, 0, GPP),
         GPP2 = if_else(NEE > Resp, 0, Resp - NEE)) #%>%
  #mutate(Round = fct_inorder(as.factor(Round)))


x %>%
  ggplot() +
  geom_point(aes(x = Round, y = Resp), color = "#D55E00") +
  geom_point(aes(x = Round, y = NEE), color = "#009E73") +
  geom_point(aes(x = Round, y = GPP), color = "#E69F00") +
  geom_point(aes(x = Round, y = GPP2), color = "#000000") +
  facet_wrap(~Species*Block)


#
# Negative GPP values make no sense. 
Flux_data.2 <- Flux_data.1 %>%
  mutate(GPP = if_else(GPP <= 0, 0, GPP),
         MP = case_when(Round == 1 ~ "September_2020",
                        Round == 2 ~ "October_2020",
                        Round == 3 ~ "November_2020",
                        Round == 4 ~ "February",
                        Round == 5 ~ "March",
                        Round == 6 ~ "May",
                        Round == 7 ~ "June",
                        Round == 8 ~ "July",
                        Round == 9 ~ "September",
                        Round == 10 ~ "October",
                        Round == 11 ~ "November")) %>%
  relocate(MP, .after = Round)
#
#
#
# Environmental ----
# PAR measurements at flux measuring time have both heath and mire later. Combine to get only one value per time
# Isolate where both sites was measured by one
PAR_flux.both <- PAR_flux %>%
  filter(Habitat == "Both") %>%
  select(Date, Time, PAR)
#
# Average heath and mire PAR measurements
PAR_flux.combi <- PAR_flux %>%
  filter(Habitat != "Both") %>%
  summarise(PAR = mean(PAR, na.rm = TRUE), .by = c(Date, Time))
#
# Combine
PAR_flux.2 <- bind_rows(PAR_flux.both, PAR_flux.combi)
#
# Simplify Air temperature
AirT_flux.2 <- AirT_flux %>%
  select(Date, Time, AirT)
#
# Combine Air temperature and PAR values
Environ_flux <- full_join(AirT_flux.2, PAR_flux.2, by = join_by(Date, Time)) %>%
  left_join(Time_flux, by = join_by(Date)) %>%
  rename("AirT_flux" = AirT,
         "PAR_flux" = PAR)
#
# Keep only time interval where measurements were taken
Environ_flux.2 <- Environ_flux %>%
  # Introduce DST, as it was used in the field measurements
  # The fine-details of exactly hour when it shifts does not matter, as no measurements were done from 2-3 am when the time shifts
  mutate(Time = case_when(Date < ymd("20201025") ~ Time+hms::hms(3600),
                          Date == ymd("20201025") & Time < hms::hms(3*3600) ~ Time+hms::hms(3600),
                          Date > ymd("20210328") & Date < ymd("20211031") ~ Time+hms::hms(3600),
                          Date > ymd("20220327") ~ Time+hms::hms(3600),
                          TRUE ~ Time)) %>%
  group_by(Date) %>%
  mutate(Remov = case_when(Time >= Early & Time <= Late ~ "No",
                           TRUE ~ "Yes")) %>%
  ungroup() %>%
  filter(Remov == "No") %>%
  select(!Remov)
#
# Average over each day
Environ_flux.3 <- Environ_flux.2 %>%
  summarise(AirT_flux = mean(AirT_flux, na.rm = T),
            PAR_flux = mean(PAR_flux, na.rm = T),
            .by = Date) %>%
  mutate(PAR_flux = if_else(is.nan(PAR_flux), NA, PAR_flux))
  
#
# AirT from wetland habitat
AirT_wetland <- AirT_wetland %>%
  select(Date, Time, AirT_C) %>%
  rename("AirT" = AirT_C)
#
# EM50 logger from heath
EM50_Heath.1 <- EM50_Heath %>%
  rowwise() %>%
  mutate(Soil_moisture = mean(c(Soil_moisture_B, Soil_moisture_P, Soil_moisture_R, Soil_moisture_W, Soil_moisture_Y, Soil_moisture_G), na.rm = TRUE),
         Soil_temperature = mean(c(Soil_temperature_B, Soil_temperature_P, Soil_temperature_R, Soil_temperature_W, Soil_temperature_Y, Soil_temperature_G), na.rm = TRUE)) %>%
  mutate(Soil_moisture = Soil_moisture*100) %>%
  ungroup() %>%
  select(Date_time, Soil_moisture, Soil_temperature, PAR) %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Time"), too_few = "debug", too_many = "debug") %>%
  mutate(Date = ymd(Date),
         Time = hms::as_hms(Time)) %>%
  select(Date, Time, Soil_moisture, Soil_temperature, PAR) %>%
  filter(!is.na(Soil_moisture) & !is.na(Soil_temperature))
#
# EM50 logger from wetland
EM50_Mire.1 <- EM50_Mire %>%
  rowwise() %>%
  mutate(Soil_moisture_Mwet = mean(c(Soil_moisture_Bwet, Soil_moisture_Pwet, Soil_moisture_Wwet, Soil_moisture_Ywet), na.rm = TRUE),
         Soil_temperature_Mwet = mean(c(Soil_temperature_Bwet, Soil_temperature_Pwet, Soil_temperature_Wwet, Soil_temperature_Ywet), na.rm = TRUE),
         Soil_moisture_M = mean(c(Soil_moisture_B, Soil_moisture_P, Soil_moisture_R, Soil_moisture_W, Soil_moisture_Y, Soil_moisture_G), na.rm = TRUE),
         Soil_temperature_M = mean(c(Soil_temperature_B, Soil_temperature_P, Soil_temperature_R, Soil_temperature_W, Soil_temperature_Y, Soil_temperature_G), na.rm = TRUE)) %>%
  mutate(Soil_moisture_M = Soil_moisture_M*100,
         Soil_moisture_Mwet = Soil_moisture_Mwet*100) %>%
  ungroup() %>%
  select(Date_time, Soil_moisture_Mwet, Soil_temperature_Mwet, Soil_moisture_M, Soil_temperature_M, PAR) %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Time"), too_few = "debug", too_many = "debug") %>%
  mutate(Date = ymd(Date),
         Time = hms::as_hms(Time)) %>%
  select(Date, Time, Soil_moisture_Mwet, Soil_temperature_Mwet, Soil_moisture_M, Soil_temperature_M, PAR) %>%
  rename("PAR_M" = PAR) %>%
  filter(!is.na(Soil_moisture_M) & !is.na(Soil_temperature_M))
#
# Combine loggers from site
Environ <- reduce(list(EM50_Heath.1, EM50_Mire.1, AirT_wetland), full_join, by = join_by(Date, Time)) %>%
  left_join(Time_flux, by = join_by(Date))
#
# Graph some comparisons:
#
# Compare the different temperature measurements
plot_ly(Environ, x = ~Date, y = ~Soil_temperature_M, name = "Mire", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~Date, y = ~Soil_temperature, name = "Heath",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Date, y = ~Soil_temperature_Mwet, name = "Wet mire",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  layout(title = "Soil temperatures", yaxis = list(title = "°C"), margin = list(l = 100))
#
# Temperature is very close to similar for all three sensor locations
#
# Compare the different temperature measurements
plot_ly(Environ, x = ~Date, y = ~PAR_M, name = "Mire", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~Date, y = ~PAR, name = "Heath",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  layout(title = "PAR", yaxis = list(title = "µmol pr m3 pr s"), margin = list(l = 100))
#
# PAR also very similar, but with differences in snow melt-out
#
# Compare the different moisture measurements
plot_ly(Environ, x = ~Date, y = ~Soil_moisture_M, name = "Mire", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~Date, y = ~Soil_moisture, name = "Heath",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Date, y = ~Soil_moisture_Mwet, name = "Wet mire",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  layout(title = "Soil moisture", yaxis = list(title = "VWC"), margin = list(l = 100))
#
# It is very much worth it to have different moisture sensors (not surprising)
#
#
# Keep only time interval where measurements were taken
# But change time to DST for "summer" measurements as flux measurements were with DST
Environ.2 <- Environ  %>%
  # Introduce DST, as it was used in the field measurements
  # The fine-details of exactly hour when it shifts does not matter, as no measurements were done from 2-3 am when the time shifts
  mutate(Time = case_when(Date < ymd("20201025") ~ Time+hms::hms(3600),
                          Date == ymd("20201025") & Time < hms::hms(3*3600) ~ Time+hms::hms(3600),
                          Date > ymd("20210328") & Date < ymd("20211031") ~ Time+hms::hms(3600),
                          Date > ymd("20220327") ~ Time+hms::hms(3600),
                          TRUE ~ Time)) %>%
  group_by(Date) %>%
  # Designate measurements to remove from outside the daily measured times
  mutate(Remov = case_when(Time >= hms::round_hms(Early, 3600) & Time <= hms::round_hms(Late, 3600) ~ "No",
                           TRUE ~ "Yes")) %>%
  ungroup() %>%
  filter(Remov == "No") %>%
  select(!Remov)
#
# Average over each day
Environ.3 <- Environ.2 %>%
  summarise(SoilT_Mwet = mean(Soil_temperature_Mwet, na.rm = T),
            SoilM_Mwet = mean(Soil_moisture_Mwet, na.rm = T),
            SoilT_M = mean(Soil_temperature_M, na.rm = T),
            SoilM_M = mean(Soil_moisture_M, na.rm = T),
            PAR_M = mean(PAR_M, na.rm = T),
            SoilT = mean(Soil_temperature, na.rm = T),
            SoilM = mean(Soil_moisture, na.rm = T),
            AirT = mean(AirT, na.rm = T),
            PAR = mean(PAR, na.rm = T),
            .by = Date)
  # PAR logger was placed on the 23rd of September 2020 (2020-09-23), but another project had logger out from before
#
#
# Combine AirT and PAR measurements with flux data
Flux_data.3 <- Flux_data.2 %>%
  left_join(Environ.3, by = join_by(Date)) %>%
  left_join(Environ_flux.3, by = join_by(Date)) %>%
  # Reduce Soil moisture and temperature to one value, by per species
  mutate(PAR = case_when(Species == "S" | Species == "Sf" | Species == "Sli" ~ PAR_M,
                         TRUE ~ PAR),
         SoilT = case_when(Species == "Sli" ~ SoilT_Mwet,
                           Species == "Sf" | Species == "S" ~ SoilT_M,
                           TRUE ~ SoilT),
         SoilM = case_when(Species == "Sli" ~ SoilM_Mwet,
                           Species == "Sf" | Species == "S" ~ SoilM_M,
                           TRUE ~ SoilM)) %>%
  select(!c(PAR_M, SoilT_Mwet, SoilT_M, SoilM_Mwet, SoilM_M))



Flux_data.3 %>%
  ggplot(aes(x = AirT, y = AirT_flux)) + geom_point()

Flux_data.3 %>%
  ggplot(aes(x = PAR, y = PAR_flux)) + geom_point()

# 
# Next:
# Statistics



# Save flux data to share.
Flux_data_export <- Flux_data.3 %>%
  select(!MP)
# write_csv(Flux_data_export, "export/Q1_Flux.csv", na = "NA")
# Flux_data.3 <- read_csv("export/Q1_Flux.csv", col_names = T)

#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Environmental NMDS        « -------
# 
# Extract Resp, GPP, NEE and environmental data
Flux_data_NMDS <- Flux_data.3 %>%
  select(!c(MP, Date)) %>%
  # Temperature in Kelvin to avoid negative values
  mutate(AirT = AirT + 273,
         SoilT = SoilT + 273,
         AirT_flux = AirT_flux + 273,
         Round = as.factor(Round))
#
# Average as some species in the same block were measured on a different day
Flux_data_NMDS.mean <- Flux_data.3 %>%
  summarise(AirT = mean(AirT, na.rm = T),
            SoilT = mean(SoilT, na.rm = T),
            SoilM = mean(SoilM, na.rm = T),
            PAR = mean(PAR, na.rm = T),
            AirT_flux = mean(AirT_flux, na.rm = T),
            .by = c(Round, Block)) %>%
  mutate(AirT = AirT + 273,
         SoilT = SoilT + 273,
         AirT_flux = AirT_flux + 273)
#
# GPP
Flux_data_NMDS.GPP <- Flux_data.2 %>%
  select(Round, Block, Species, GPP) %>%
  pivot_wider(values_from = GPP, names_from = Species, names_prefix = "GPP_") %>%
  left_join(Flux_data_NMDS.mean, by = join_by(Round, Block))
#
# NEE
Flux_data_NMDS.NEE <- Flux_data.2 %>%
  select(Round, Block, Species, NEE) %>%
  pivot_wider(values_from = NEE, names_from = Species, names_prefix = "NEE_") %>%
  left_join(Flux_data_NMDS.mean, by = join_by(Round, Block))
#
# Resp
Flux_data_NMDS.Resp <- Flux_data.2 %>%
  select(Round, Block, Species, Resp) %>%
  pivot_wider(values_from = Resp, names_from = Species, names_prefix = "Resp_") %>%
  left_join(Flux_data_NMDS.mean, by = join_by(Round, Block))
#
# all in one chaos
Flux_data_NMDS.one <- reduce(list(Flux_data_NMDS.GPP, Flux_data_NMDS.NEE, Flux_data_NMDS.Resp), left_join, by = join_by(Round, Block, AirT, SoilT, SoilM, PAR, AirT_flux)) %>%
  relocate(c(AirT, SoilT, SoilM, PAR, AirT_flux), .after = Block)
# Not really useful

#
# Correlation plot
corrplot::corrplot(cor(Flux_data_NMDS[4:11], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot::corrplot(cor(Flux_data_NMDS.GPP[3:17], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot::corrplot(cor(Flux_data_NMDS.NEE[3:17], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot::corrplot(cor(Flux_data_NMDS.Resp[3:17], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot::corrplot(cor(Flux_data_NMDS.one[3:37], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#
# Pairwise correlation comparison
pairs(x = Flux_data_NMDS[4:11], gap = 0, cex.labels = 0.5)
# Not much correlation with environmental data and flux
pairs(x = Flux_data_NMDS.GPP[3:17], gap = 0, cex.labels = 0.5)
pairs(x = Flux_data_NMDS.NEE[3:17], gap = 0, cex.labels = 0.5)
pairs(x = Flux_data_NMDS.Resp[3:17], gap = 0, cex.labels = 0.5)
pairs(x = Flux_data_NMDS.one[3:37], gap = 0, cex.labels = 0.5)
#
# NMDS
NMDS_environ <- metaMDS(Flux_data_NMDS.GPP[13:17], distance = "bray")#, scaling = 1, autotransform = TRUE) # Does a sqrt transformation and Wisconsin standardization
NMDS_sp_GPP <- metaMDS(Flux_data_NMDS.GPP[3:12], distance = "bray")
NMDS_sp_NEE <- metaMDS(Flux_data_NMDS.NEE[3:12], distance = "bray")
NMDS_sp_Resp <- metaMDS(Flux_data_NMDS.Resp[3:12], distance = "bray")
NMDS_sp_one <- metaMDS(Flux_data_NMDS.one[8:37], distance = "bray")
#
# Species ethylene production fit
envfit.flux <- envfit(NMDS_environ,
                    Flux_data_NMDS.GPP[3:12],
                    permutations = 9999, na.rm = TRUE)
#
envfit.flux.GPP <- envfit(NMDS_sp_GPP,
                      Flux_data_NMDS.GPP[13:17],
                      permutations = 9999, na.rm = TRUE)
envfit.flux.NEE <- envfit(NMDS_sp_NEE,
                          Flux_data_NMDS.NEE[13:17],
                          permutations = 9999, na.rm = TRUE)
envfit.flux.Resp <- envfit(NMDS_sp_Resp,
                          Flux_data_NMDS.Resp[13:17],
                          permutations = 9999, na.rm = TRUE)
envfit.flux.one <- envfit(NMDS_sp_one,
                           Flux_data_NMDS.one[3:7],
                           permutations = 9999, na.rm = TRUE)


#
# Plot
# Standard with little fancyness
par (mfrow = c(1,2))
plot(NMDS_environ, type = "n")
points(NMDS_environ, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_environ, display = "spec", cex=0.7, col="blue")
plot(envfit.flux)
stressplot(NMDS_environ)
par (mfrow = c(1,1))
#
#
par (mfrow = c(1,2))
plot(NMDS_sp_GPP, type = "n")
points(NMDS_sp_GPP, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_sp_GPP, display = "spec", cex=0.7, col="blue")
plot(envfit.flux.GPP)
stressplot(NMDS_sp_GPP)
par (mfrow = c(1,1))
#
par (mfrow = c(1,2))
plot(NMDS_sp_NEE, type = "n")
points(NMDS_sp_NEE, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_sp_NEE, display = "spec", cex=0.7, col="blue")
plot(envfit.flux.NEE)
stressplot(NMDS_sp_NEE)
par (mfrow = c(1,1))
#
par (mfrow = c(1,2))
plot(NMDS_sp_Resp, type = "n")
points(NMDS_sp_Resp, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_sp_Resp, display = "spec", cex=0.7, col="blue")
plot(envfit.flux.Resp)
stressplot(NMDS_sp_Resp)
par (mfrow = c(1,1))
#
par (mfrow = c(1,2))
plot(NMDS_sp_one, type = "n")
points(NMDS_sp_one, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_sp_one, display = "spec", cex=0.7, col="blue")
plot(envfit.flux.one)
stressplot(NMDS_sp_one)
par (mfrow = c(1,1))
#
#
# Extract points to create graph
# Main plot with NMDS scores
NMDS_environ.plot <- Flux_data_NMDS
NMDS_environ.plot$NMDS1 <- NMDS_environ$points[,1]
NMDS_environ.plot$NMDS2 <- NMDS_environ$points[,2]
NMDS_environ.plot <- NMDS_environ.plot %>%
  mutate(season = case_when(Round == "1" | Round == "2" | Round == "3" ~ "Fall1",
                            Round == "10" | Round == "11" ~ "Fall2",
                            Round == "4" ~ "Winter",
                            Round == "5" | Round == "6" ~ "Spring",
                            Round == "7" | Round == "8" | Round == "9" ~ "Summer"),
         snowS = if_else(str_detect(Round, "3|4|5|6"), "Snow", "Free"),
         # snowS = case_when(str_detect(Round, "3|4|5") ~ "Snow",
         #                   Round == "6" & Block == "W" ~ "Snow",
         #                   Round == "6" & Block == "P" ~ "Snow", 
         #                   Round == "6" & Block == "R" ~ "Snow",
         #                   TRUE ~ "Free"),
         month = case_when(Round == "1" ~ "Sep20",
                           Round == "2" ~ "Oct20",
                           Round == "3" ~ "Nov20",
                           Round == "4" ~ "Feb",
                           Round == "5" ~ "Mar",
                           Round == "6" ~ "May",
                           Round == "7" ~ "Jun",
                           Round == "8" ~ "Jul",
                           Round == "9" ~ "Aug",
                           Round == "10" ~ "Sep",
                           Round == "11" ~ "Nov"))
#
# For text
# Environmental factors
NMDS_environ.scores.env <- as.data.frame(scores(NMDS_environ, "species"))
NMDS_environ.scores.env$Factors <- rownames(NMDS_environ.scores.env)
#
NMDS_environ.scores.env <- NMDS_environ.scores.env %>%
  mutate(Factors = case_when(Factors == "Soil_temperature" ~ "Soil temperature",
                             Factors == "Soil_moisture" ~ "Soil moisture",
                             Factors == "AirT" ~ "Air temperature",
                             TRUE ~ Factors))
# Species directions
NMDS_environ.scores.sp <- as.data.frame(scores(envfit.flux$vectors$arrows))
NMDS_environ.scores.sp$Species <- rownames(NMDS_environ.scores.sp)
# Decrease arrow size, so direction can be plotted with the main NMDS plot
NMDS_environ.scores.sp <- NMDS_environ.scores.sp %>%
  mutate(NMDS1 = NMDS1/3,
         NMDS2 = NMDS2/4)
#
#
# Plot the NMDS
ggplot() +
  #  geom_polygon(data = NMDS_environ.plot, aes(x = NMDS1, y = NMDS2, fill = snowS, group = snowS), alpha = 0.30) +
  #  geom_polygon(data = NMDS_environ.plot, aes(x = NMDS1, y = NMDS2, fill = season, group = season), alpha = 0.30) +
  geom_text(data = NMDS_environ.scores.env, aes(x = NMDS1, y = NMDS2, label = Factors)) +
  geom_point(data = NMDS_environ.plot, aes(x = NMDS1, y = NMDS2, shape = Block, color = month), size = 3) +
  geom_segment(data = NMDS_environ.scores.sp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), color = "#8fa3a5") +
  geom_text_repel(data = NMDS_environ.scores.sp, aes(x = NMDS1, y = NMDS2, label  = Species), size = 5, color = "#4D495A") + 
  coord_equal() +
  theme_classic() +
  theme(legend.text = element_text(size = 21), legend.title = element_text(size = 21))



# 
#
#
#-------  »   Q1            « -------
# 
#
Q1_flux <- Flux_data.3 %>%
  mutate(across(Round, ~as.character(.x)),
         across(c(Block, Species, Round), ~as.factor(.x)),
         Habitat = if_else(Species == "S" | Species == "Sf" | Species == "Sli", "Mire", "Heath")) %>%
  relocate(Habitat, .after = Species)
#
# Quick check of data
#
# GPP
hist(Q1_flux$GPP) # Several 0s
hist(sqrt(Q1_flux$GPP)) # Right skewed
hist(log(Q1_flux$GPP)) # Left skewed
hist(ihs(Q1_flux$GPP)) # Right skewed
#
# NEE
hist(Q1_flux$NEE) # Several negative values, but normal distribution around 0.5
hist(Q1_flux$NEE+5) # Slight right skew
hist(sqrt(Q1_flux$NEE+5)) # Still slightly skewed
hist(log(Q1_flux$NEE+5)) # one point very frequent
#
# Resp
hist(Q1_flux$Resp) # Right skew
hist(sqrt(Q1_flux$Resp)) # Sort of ok
hist(log(Q1_flux$Resp)) # Left skew



Q1_flux %>%
  ggplot(aes(x = factor(Round, levels = order(levels(Round))), y = GPP, color = Block)) + geom_point() + facet_wrap(~Species)






# GPP
#
# Transform data
Q1_flux_GPP <- Q1_flux %>%
  select(Round, Block, Species, Habitat, GPP, AirT, PAR, SoilT, SoilM) %>%
  mutate(logGPP = log(GPP),
         sqrtGPP = sqrt(GPP),
         cubeGPP = GPP^(1/3),
         sqGPP = GPP^2,
         ashinGPP = log(GPP + sqrt(GPP^2 + 1)), # inverse hyperbolic sine transformation
         arcGPP = asin(sqrt(((GPP)/10000))),
         ihsGPP = ihs(GPP))
#
lme1_GPP <- lme(sqrtGPP ~ Species*Round,
                random = ~1|Block/Species,
                data = Q1_flux_GPP, na.action = na.exclude, method = "REML")
lme1_GPP2 <- lme(sqrtGPP ~ Species * AirT * PAR * SoilT * SoilM,
            random = ~1|Block/Species,
            data = Q1_flux_GPP, na.action = na.exclude, method = "REML")
#
# Using lme4 package:
# lmer(logEt_prod ~ Round*Species + (1 | Block/Species), data = Q1_ARA, na.action = na.exclude)
#
# Checking assumptions:
par(mfrow = c(1,2))
plot(fitted(lme1_GPP), resid(lme1_GPP), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1_GPP), main = "Normally distributed?")                 
qqline(resid(lme1_GPP), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1_GPP)
par(mfrow = c(1,1))
#
# model output
Anova(lme1_GPP, type=2)
Anova(lme1_GPP2, type=2)

lme1_GPP_test <- lme(sqrtGPP ~ Species * SoilM * PAR,
                     random = ~1|Block/Species,
                     data = Q1_flux_GPP, na.action = na.exclude, method = "REML")

Anova(lme1_GPP_test, type=3)


# NEE
#
# Transform data
Q1_flux_NEE <- Q1_flux %>%
  select(Round, Block, Species, NEE, AirT, PAR, SoilT, SoilM) %>%
  mutate(plusNEE = NEE+5,
         logNEE = log(NEE+2),
         sqrtNEE = sqrt(NEE+2),
         cubeNEE = NEE^(1/3),
         sqNEE = NEE^2,
         ashinNEE = log(NEE + sqrt(NEE^2 + 1)), # inverse hyperbolic sine transformation
         arcNEE = asin(sqrt(((NEE+2)/10000))),
         ihsNEE = ihs(NEE))
#
lme1_NEE <- lme(NEE ~ Round*Species,
            random = ~1|Block/Species,
            data = Q1_flux_NEE, na.action = na.exclude, method = "REML")
lme1_NEE2 <- lme(NEE ~ Species * AirT * PAR * SoilT * SoilM,
                 random = ~1|Block/Species,
                 data = Q1_flux_NEE, na.action = na.exclude, method = "REML")
#
#
# Using lme4 package:
# lmer(logEt_prod ~ Round*Species + (1 | Block/Species), data = Q1_ARA, na.action = na.exclude)
#
# Checking assumptions:
par(mfrow = c(1,2))
plot(fitted(lme1_NEE), resid(lme1_NEE), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1_NEE), main = "Normally distributed?")                 
qqline(resid(lme1_NEE), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1_NEE)
par(mfrow = c(1,1))
#
# model output
Anova(lme1_NEE, type=2)
Anova(lme1_NEE2, type=2)



# Resp
#
# Transform data
Q1_flux_Resp <- Q1_flux %>%
  select(Round, Block, Species, Habitat, Resp, AirT, PAR, SoilT, SoilM) %>%
  mutate(Resp = if_else(Resp < 0, 0, Resp)) %>%
  mutate(logResp = log(Resp+1),
         sqrtResp = sqrt(Resp),
         cubeResp = Resp^(1/3),
         sqResp = Resp^2,
         ashinResp = log(Resp + sqrt(Resp^2 + 1)), # inverse hyperbolic sine transformation
         arcResp = asin(sqrt(((Resp)/10000))),
         ihsResp = ihs(Resp))
#
lme1_Resp <- lme(sqrtResp ~ Round*Species,
                 random = ~1|Block/Species,
                 data = Q1_flux_Resp, na.action = na.exclude, method = "REML")
lme1_Resp2 <- lme(sqrtResp ~ Species * AirT * PAR * SoilT * SoilM,
                 random = ~1|Block/Species,
                 data = Q1_flux_Resp, na.action = na.exclude, method = "REML")
#
#
# Using lme4 package:
# lmer(logEt_prod ~ Round*Species + (1 | Block/Species), data = Q1_ARA, na.action = na.exclude)
#
# Checking assumptions:
par(mfrow = c(1,2))
plot(fitted(lme1_Resp), resid(lme1_Resp), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1_Resp), main = "Normally distributed?")                 
qqline(resid(lme1_Resp), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1_Resp)
par(mfrow = c(1,1))
#
# model output
Anova(lme1_Resp, type=2)
Anova(lme1_Resp2, type=2)

#
# Remove Sphagnum from model
Q1_flux_Resp_noSphagn <- Q1_flux_Resp %>%
  filter(Habitat == "Heath") %>%
  droplevels() %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
#
lme1_Resp_noSphag <- lme(sqrtResp ~ Round*Species,
                         random = ~1|Block/Species,
                         data = Q1_flux_Resp_noSphagn, na.action = na.exclude, method = "REML")
#
par(mfrow = c(1,2))
plot(fitted(lme1_Resp_noSphag), resid(lme1_Resp_noSphag), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1_Resp_noSphag), main = "Normally distributed?")                 
qqline(resid(lme1_Resp_noSphag), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1_Resp_noSphag)
par(mfrow = c(1,1))
#
Anova(lme1_Resp_noSphag, type=2)
#
# Only Sphagnum species
Q1_flux_Resp_Sphagn <- Q1_flux_Resp %>%
  filter(Habitat == "Mire") %>%
  droplevels() %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
#
lme1_Resp_Sphag <- lme(sqrtResp ~ Round*Species,
                         random = ~1|Block/Species,
                         data = Q1_flux_Resp_Sphagn, na.action = na.exclude, method = "REML")
#
par(mfrow = c(1,2))
plot(fitted(lme1_Resp_Sphag), resid(lme1_Resp_Sphag), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1_Resp_Sphag), main = "Normally distributed?")                 
qqline(resid(lme1_Resp_Sphag), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1_Resp_Sphag)
par(mfrow = c(1,1))
#
Anova(lme1_Resp_Sphag, type=2)



#
#
#
#
#

#-------  »   Q2            « -------

# 
#
#
#=======  ♫♫  Graphs        ♫♫ =======
#-------  ♪   Environmental ♪ -------
#
# Environmental data and flux air temperature and PAR. Restricted to time of interest
Environ.plot <- Environ %>%
  left_join(Environ_flux, by = join_by(Date, Time)) %>%
  select(!c(Early.x, Late.x, Early.y, Late.y)) %>%
  summarise(SoilT = mean(Soil_temperature, na.rm = T), # Not sure if averaging by day is the best way about this
            SoilM = mean(Soil_moisture, na.rm = T),
            SoilT_M = mean(Soil_temperature_M, na.rm = T),
            SoilT_Mwet = mean(Soil_temperature_Mwet, na.rm = T),
            SoilM_M = mean(Soil_moisture_M, na.rm = T),
            SoilM_Mwet = mean(Soil_moisture_Mwet, na.rm = T),
            AirT = mean(AirT, na.rm = T),
            PAR = mean(PAR, na.rm = T),
            PAR_M = mean(PAR_M, na.rm = T),
            AirT_flux = mean(AirT_flux, na.rm = T),
            PAR_flux = mean(PAR_flux, na.rm = T),
            .by = Date) %>%
  # unite(Date, Time, col = "Date_time", sep = "T") %>%
  # mutate(Date_time = ymd_hms(Date_time)) %>%
  filter(Date <= ymd("2021-12-01"))
#
# Alternative with all hourly data points
Environ.plot.x <- Environ %>%
  left_join(Environ_flux, by = join_by(Date, Time)) %>%
  select(!c(Early.x, Late.x, Early.y, Late.y)) %>%
  unite(Date, Time, col = "Date_time", sep = "T") %>%
  mutate(Date_time = ymd_hms(Date_time)) %>%
  filter(Date_time <= ymd("2021-12-01"))
#
# Plot air and soil temperature with each other
Environ.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 1) +
  geom_line(aes(x = Date, y = AirT, lty = "Air temperature")) +
  geom_line(aes(x = Date, y = SoilT, lty = "Soil temperature"))
#
# Plot PAR and PAR from the flux measurements
Environ.plot %>%
  ggplot() +
  geom_point(aes(x = Date, y = PAR, shape = "PAR")) +
  geom_point(aes(x = Date, y = PAR_flux, shape = "flux"))
#
# Plot in Plotly
# Temperatures
plot_ly(Environ.plot.x, x = ~Date_time, y = ~AirT, name = "Air temperature", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~Date_time, y = ~AirT_flux, name = "Air temperature flux measurements",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Date_time, y = ~Soil_temperature, name = "Soil temperature",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  layout(title = "Temperature measurements", xaxis = list(title = "Date"), yaxis = list(title = "Temperature (°C)"), margin = list(l = 100))
#
# PAR
plot_ly(Environ.plot.x, x = ~Date_time, y = ~PAR, name = "PAR", type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>% 
  add_trace(x = ~Date_time, y = ~PAR_flux, name = "PAR flux measurements",type = 'scatter', mode = "markers", marker = list(color = "#F0E442")) %>%
  layout(title = "PAR measurements", xaxis = list(title = "Date"), yaxis = list(title = "PAR (µmol m-2 s-1"), margin = list(l = 100))
#
#
# <><><><><> MAIN ENVIRONMENTAL PLOT - FIG X <><><><><>
#
#
# Cut-off dates
measureDays <- c(as.Date("2020-09-10"),as.Date("2021-11-20"))
#
# Air temperature
airT_plot <- Environ.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 1) +
  geom_line(aes(x = Date, y = AirT)) +
  scale_y_continuous(breaks = c(-20, -10, 0, 10, 20), minor_breaks = c(-25, -15, -5, 5, 15, 25)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
  coord_cartesian(xlim = measureDays) +
  labs(x = NULL, y = "Air temperature (°C)", x = "Time of year") +
  theme_bw(base_size = 25) +
  theme(legend.position = "top", axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))
#
# Soil temperature
soilT_plot <- Environ.plot %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "#999999", linewidth = 1) +
  geom_line(aes(x = Date, y = SoilT, lty = "Heath")) +
  geom_line(aes(x = Date, y = SoilT_M, lty = "Mire")) +
  geom_line(aes(x = Date, y = SoilT_Mwet, lty = "Wet Mire")) +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15), minor_breaks = c(-7.5, -2.5, 2.5, 7.5, 12.5)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
  coord_cartesian(xlim = measureDays) +
  labs(x = NULL, y = "Soil temperature (°C)", x = "Time of year") +
  guides(lty = guide_legend(title = "Habitat")) +
  theme_bw(base_size = 25) +
  theme(legend.position = "top", axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))
#
soilT_legend <- get_legend(soilT_plot)
soilT_plot.2 <- soilT_plot + theme_bw(base_size = 25) + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) 
#soilT_plot <- soilT_plot + guides(lty = NULL)

# Air and soil temperature in one
# airT_plot <- Environ.plot %>%
#   ggplot() +
#   geom_hline(yintercept = 0, color = "#999999", linewidth = 1) +
#   geom_line(aes(x = Date, y = AirT, lty = "Air temperature")) +
#   geom_line(aes(x = Date, y = SoilT, lty = "Soil temperature")) +
#   scale_y_continuous(breaks = c(-20, -10, 0, 10, 20), minor_breaks = c(-25, -15, -5, 5, 15, 25)) +
#   scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
#   coord_cartesian(xlim = c(as.Date("2020-09-01"),as.Date("2021-12-01"))) +
#   labs(x = NULL, y = "Temperature (°C)", x = "Time of year") +
#   guides(lty = guide_legend(title = "Mean diel temperature")) +
#   theme_bw(base_size = 25) +
#   theme(legend.position = "top", axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))
# #
# airT_legend <- get_legend(airT_plot)
# airT_plot.2 <- airT_plot + theme_bw(base_size = 25) + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 15))
#
# Soil moisture
soilM_plot <- Environ.plot %>%
  ggplot() +
  geom_line(aes(x = Date, y = SoilM, lty = "Heath")) +
  geom_line(aes(x = Date, y = SoilM_M, lty = "Mire")) +
  geom_line(aes(x = Date, y = SoilM_Mwet, lty = "Wet Mire")) +
  #scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90), minor_breaks = c(-5, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), minor_breaks = c( 10, 20, 30, 50, 70, 90)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
  coord_cartesian(xlim = measureDays) +
  labs(x = NULL, y = "VWC (%)", x = "Time of year") +
  theme_bw(base_size = 25) +
  theme(legend.position = "top", axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))
#
soilM_legend <- get_legend(soilM_plot)
soilM_plot.2 <- soilM_plot + theme_bw(base_size = 25) + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) 
#soilM_plot <- soilM_plot + guides(lty = NULL)
#
# PAR 
PAR_plot <- Environ.plot %>%
  ggplot() +
  geom_line(aes(x = Date, y = PAR, lty = "Heath")) +
  geom_line(aes(x = Date, y = PAR_M, lty = "Mire")) +
  scale_y_continuous(breaks = c(0, 200, 400, 600), minor_breaks = c(100, 300, 500, 700)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day", date_labels = "%d-%b") +
  coord_cartesian(xlim = measureDays) +
  labs(x = "Time of year", y = expression("PAR (µmol  "*m^-2*" "*s^-1*")"), x = "Time of year") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15))
#
PAR_plot.2 <- PAR_plot + theme_bw(base_size = 25) + theme(legend.position = "none", axis.text.x = element_text(size = 15), axis.title.x = element_blank(), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 15)) 
#
# Plot graph
# Align main graphs
plot_grid(airT_plot, soilT_plot, soilM_plot, PAR_plot, align = "v", ncol = 1, rel_heights = c(3,3,2.5,3.5))
#
# With added mire environmental values
# Make all plots align, then add legend
env_plot <- plot_grid(airT_plot, soilT_plot.2, soilM_plot.2, PAR_plot.2, align = "v", ncol = 1, rel_heights = c(3,3,2.5,3.5))
plot_grid(env_plot, soilT_legend, ncol = 1, rel_heights = c(9, 1))
#
#
# <><><><><> END - FIG X <><><><><>
#
#

#
#
#
#-------  ♪   Flux          ♪ -------
#
# Summary data
NEE_sum <- summarySE(Flux_data.3, measurevar = "NEE", groupvars = c("Round", "Species"))
Resp_sum <- summarySE(Flux_data.3, measurevar = "Resp", groupvars = c("Round", "Species"))
GPP_sum <- summarySE(Flux_data.3, measurevar = "GPP", groupvars = c("Round", "Species"))
SoilT_sum <- summarySE(Flux_data.3, measurevar = "SoilT", groupvars = c("Round", "Species"))
SoilM_sum <- summarySE(Flux_data.3, measurevar = "SoilM", groupvars = c("Round", "Species"))
AirT_sum <- summarySE(Flux_data.3, measurevar = "AirT", groupvars = c("Round", "Species"))
PAR_sum <- summarySE(Flux_data.3, measurevar = "PAR", groupvars = c("Round", "Species"))
#
# Combine environmental drivers
Environ_sum <- reduce(list(SoilT_sum, SoilM_sum, AirT_sum, PAR_sum, GPP_sum), left_join, by = join_by("Round", "Species"))



# NEE
NEE_sum %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum flexuosum",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  mutate(Month = case_when(Round == 1 ~ "Sept20",
                           Round == 2 ~ "Oct20",
                           Round == 3 ~ "Nov20",
                           Round == 4 ~ "Feb21",
                           Round == 5 ~ "Mar21",
                           Round == 6 ~ "May21",
                           Round == 7 ~ "Jun21",
                           Round == 8 ~ "Jul21",
                           Round == 9 ~ "Sept21",
                           Round == 10 ~ "Oct21",
                           Round == 11 ~ "Nov21")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = NEE, ymin = NEE, ymax = NEE+se), position = position_dodge(.9)) +
  geom_col(aes(x = Month, y = NEE, fill = BFG)) +
  scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 3, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Measuring period (Month)", y = expression("NEE (µmol "*m^-2*s^-1*")"), title = "Net Ecosystem Exchange") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"), axis.text.x=element_text(angle = 60, hjust = 1), legend.position = "bottom")
#
# Respiration
Resp_sum %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum flexuosum",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  mutate(Habitat = if_else(BFG == "Sphagnum", "Mire", "Heath")) %>%
  mutate(Month = case_when(Round == 1 ~ "Sept20",
                           Round == 2 ~ "Oct20",
                           Round == 3 ~ "Nov20",
                           Round == 4 ~ "Feb21",
                           Round == 5 ~ "Mar21",
                           Round == 6 ~ "May21",
                           Round == 7 ~ "Jun21",
                           Round == 8 ~ "Jul21",
                           Round == 9 ~ "Sept21",
                           Round == 10 ~ "Oct21",
                           Round == 11 ~ "Nov21")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = Resp, ymin = Resp, ymax = Resp+se), position=position_dodge(.9)) +
  geom_col(aes(x = Month, y = Resp, fill = BFG)) +
  #scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 3, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Measuring period (Month)", y = expression(R[e]*" (µmol "*m^-2*s^-1*")"), title = "Ecosystem respiration") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"), axis.text.x=element_text(angle = 60, hjust = 1), legend.position = "bottom")

#
# GPP
GPP_sum %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum flexuosum",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  mutate(Month = case_when(Round == 1 ~ "Sept20",
                           Round == 2 ~ "Oct20",
                           Round == 3 ~ "Nov20",
                           Round == 4 ~ "Feb21",
                           Round == 5 ~ "Mar21",
                           Round == 6 ~ "May21",
                           Round == 7 ~ "Jun21",
                           Round == 8 ~ "Jul21",
                           Round == 9 ~ "Sept21",
                           Round == 10 ~ "Oct21",
                           Round == 11 ~ "Nov21")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = GPP, ymin = GPP, ymax = GPP+se), position = position_dodge(.9)) +
  geom_col(aes(x = Month, y = GPP, fill = BFG)) +
  #scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 3, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Measuring period (Month)", y = expression("GPP (µmol "*m^-2*s^-1*")"), title = "Bryophyte gross primary production") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"), axis.text.x=element_text(angle = 60, hjust = 1), legend.position = "bottom")

#
# GPP with environmental factors
Flux_data.3 %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum flexuosum",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  mutate(Month = case_when(Round == 1 ~ "Sept20",
                           Round == 2 ~ "Oct20",
                           Round == 3 ~ "Nov20",
                           Round == 4 ~ "Feb21",
                           Round == 5 ~ "Mar21",
                           Round == 6 ~ "May21",
                           Round == 7 ~ "Jun21",
                           Round == 8 ~ "Jul21",
                           Round == 9 ~ "Sept21",
                           Round == 10 ~ "Oct21",
                           Round == 11 ~ "Nov21")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  ggplot() +
  geom_col(aes(x = Month, y = GPP, fill = SoilM)) +
  #scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 3, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Measuring period (Month)", y = expression("GPP (µmol "*m^-2*s^-1*")"), title = "Bryophyte gross primary production") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"), axis.text.x=element_text(angle = 60, hjust = 1), legend.position = "bottom")


#
#
#
#-------  ♪   Outliers      ♪ -------
# Check flux data structure
#
# Boxplot
Flux_data.2 %>%
  mutate(MP = fct_inorder(MP)) %>%
  ggplot(aes(x = MP, y = GPP)) + geom_boxplot() + facet_wrap(~Species, scales = "free")
#
# Histogram
Flux_data.2 %>%
  ggplot(aes(x = GPP)) + geom_histogram() + facet_wrap(~Species)
#
# Check for negative values
# Per Block
Flux_data_GPP_block <- Flux_data.1 %>%
  mutate(across(Round, ~as.factor(.x))) %>%
  select(Round, Block, Species, GPP) %>%
  pivot_wider(names_from = Block, values_from = GPP)
#
plot_ly(Flux_data_GPP_block, x = ~B, y = ~Round, name = "Blue", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~P, y = ~Round, name = "Purple",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~R, y = ~Round, name = "Red",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~W, y = ~Round, name = "White",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Y, y = ~Round, name = "Yellow",type = 'scatter', mode = "markers", marker = list(color = "#F0E442")) %>%
  layout(title = "Photosynthesis", xaxis = list(title = "GPP (µmol)"), margin = list(l = 100))
#
# Per species
Flux_data_GPP_species <- Flux_data.1 %>%
  mutate(across(Round, ~as.factor(.x))) %>%
  select(Round, Block, Species, GPP) %>%
  pivot_wider(names_from = Species, values_from = GPP)
#
plot_ly(Flux_data_GPP_species, x = ~Au, y = ~Round, name = "Aulacomnium turgidum", type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Di, y = ~Round, name = "Dicranum scoparium",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Hy, y = ~Round, name = "Hylocomium splendens",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Pl, y = ~Round, name = "Pleurozium schreberi",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Po, y = ~Round, name = "Polytrichum commune",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Pti, y = ~Round, name = "Ptilidium ciliare",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Ra, y = ~Round, name = "Racomitrium lanuginosum",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~S, y = ~Round, name = "Sphagnum sp",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Sf, y = ~Round, name = "Sphagnum fuscum",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Sli, y = ~Round, name = "Sphagnum lindbergii",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  layout(title = "Photosynthesis per species", xaxis = list(title = "GPP (µmol)"), margin = list(l = 100))
#
# Some environmental data checks


PAR_flux %>%
  unite(Date, Time, col = "Date_Time", sep = "T") %>%
  mutate(Date_time = ymd_hms(Date_Time)) %>%
  ggplot(aes(x = Date_time, y = PAR)) + geom_point() + facet_wrap(~Habitat)

PAR_flux %>%
  filter(Habitat != "Both") %>%
  distinct(Date, Time)

PAR_flux %>%
  filter(Habitat == "Both") %>%
  distinct(Date)
PAR_flux %>%
  filter(Habitat == "H") %>%
  distinct(Date)
PAR_flux %>%
  filter(Habitat == "M") %>%
  distinct(Date)


Flux_data.3 %>%
  ggplot(aes(x = AirT, y = AirT_flux)) + geom_point()
Flux_data.3 %>%
  ggplot(aes(x = PAR, y = PAR_flux)) + geom_point()

Flux_data.3 %>%
  select(Date, Block, Species, AirT, AirT_flux) %>%
  pivot_longer(cols = c(AirT, AirT_flux), names_to = "Sensor", values_to = "AirT") %>%
  ggplot(aes(x = Date, y = AirT, shape = Sensor, color = Block)) + geom_point()

Flux_data.3 %>%
  select(Date, Block, Species, PAR, PAR_flux) %>%
  pivot_longer(cols = c(PAR, PAR_flux), names_to = "Sensor", values_to = "PAR") %>%
  ggplot(aes(x = Date, y = PAR, shape = Sensor, color = Block)) + geom_point()

y <- Flux_data.3 %>%
  select(Date, Block, Species, AirT, AirT_flux) %>%
  mutate(Air_Temp = AirT - AirT_flux) %>%
  ggplot(aes(x = Date, y = Air_Temp, color = Block)) + geom_point()


x.environ_flux <- Environ_flux %>%
  mutate(Time2 = hour(Time)) %>%
  mutate(Time2 = hms::as_hms(Time2*60*60)) %>%
  group_by(Date, Time2) %>%
  summarise(AirT_flux = mean(AirT_flux, na.rm = T),
            PAR_flux = mean(PAR_flux, na.rm = T)) %>%
  ungroup() %>%
  rename("Time" = Time2)



x <- left_join(Environ, Environ_flux, by = join_by(Date, Time, Early, Late))


xx <- x %>%
  mutate(diff = if_else(is.na(AirT_flux), 0, AirT - AirT_flux)) %>%
  ggplot(aes(x = Date, y = diff)) + geom_point()









Flux_data.3 %>%
  ggplot() +
  geom_point(aes(x = Date, y = GPP, color = Species), shape = 1) 

Flux_data.3 %>%
  ggplot() +
  geom_point(aes(x = Date, y = SoilM, color = Species), shape = 1) 



#
#
#
#=======  ■  { The End }    ■ =======