# Analysis - moss flux
# By Emil A.S. Andersen
# 
#=======  ♣   Libraries     ♣ =======
library(plotly)
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
EM50_Heath <- read_csv("Data_clean/Heath_EM50_simple.csv", col_names = TRUE)
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
  mutate(GPP = Resp - NEE)
#
# Negative values make no sense. 
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
  left_join(Time_flux, by = join_by(Date))
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
  summarise(AirT = mean(AirT, na.rm = T),
            PAR = mean(PAR, na.rm = T),
            .by = Date) %>%
  rename("AirT_flux" = AirT,
         "PAR_flux" = PAR) %>%
  mutate(PAR_flux = if_else(is.nan(PAR_flux), NA, PAR_flux))
  
#
# AirT from wetland habitat
AirT_wetland <- AirT_wetland %>%
  select(Date, Time, AirT_C) %>%
  rename("AirT" = AirT_C)
#
# EM50 logger from heath
EM50_Heath <- EM50_Heath %>%
  rename("Time" = Tid)
#
# Combine loggers from site
Environ <- full_join(EM50_Heath, AirT_wetland, by = join_by(Date, Time)) %>%
  left_join(Time_flux, by = join_by(Date))
#
# Keep only time interval where measurements were taken
Environ.2 <- Environ  %>%
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
Environ.3 <- Environ.2 %>%
  summarise(AirT = mean(AirT, na.rm = T),
            PAR = mean(PAR, na.rm = T),
            .by = Date) %>%
  # PAR logger only seems to have been placed on the 23rd of September 2020 (2020-09-23)
  mutate(PAR = if_else(Date == ymd("20200901") | Date == ymd("20200902"), NA, PAR))
#
#
# Combine AirT and PAR measurements with flux data
Flux_data.3 <- Flux_data.2 %>%
  left_join(Environ.3, by = join_by(Date)) %>%
  left_join(Environ_flux.3, by = join_by(Date))


# 
# Next:
# Statistics



# Save flux data to share.
Flux_data_export <- Flux_data.3 %>%
  select(!MP)
write_csv(Flux_data_export, "export/Q1_Flux.csv", na = "NA")

#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Q1            « -------
# 
#
Q1_flux <- Flux_data.2 %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
# Transform data
Q1_flux <- Q1_flux %>%
  select(Round, Block, Species, GPP) %>%
  mutate(logGPP = log(GPP+1),
         sqrtGPP = sqrt(GPP),
         cubeGPP = GPP^(1/9),
         sqGPP = GPP^2,
         ashinGPP = log(GPP + sqrt(GPP^2 + 1)), # inverse hyperbolic sine transformation
         arcGPP = asin(sqrt(((GPP)/10000))))
#
lme1 <- lme(logGPP ~ Round*Species,
            random = ~1|Block/Species,
            data = Q1_flux, na.action = na.exclude, method = "REML")
#
# Using lme4 package:
# lmer(logEt_prod ~ Round*Species + (1 | Block/Species), data = Q1_ARA, na.action = na.exclude)
#
# Checking assumptions:
par(mfrow = c(1,2))
plot(fitted(lme1), resid(lme1), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1), main = "Normally distributed?")                 
qqline(resid(lme1), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1)
par(mfrow = c(1,1))
#
# model output
Anova(lme1, type=2)
#
#
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


#
#
#
#=======  ■  { The End }    ■ =======