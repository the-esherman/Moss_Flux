# Bryophyte experiment - CO2 flux
# Script author: Emil A.S. Andersen
#
# Analysis - bryophyte flux
# 
#=======  ♣   Libraries     ♣ =======
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
library(car)
library(nlme)
library(vegan)
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
  summarise(SoilT = mean(Soil_temperature, na.rm = T),
            SoilM = mean(Soil_moisture, na.rm = T),
            AirT = mean(AirT, na.rm = T),
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
# write_csv(Flux_data_export, "export/Q1_Flux.csv", na = "NA")

#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Q1            « -------
# 
#
Q1_flux <- Flux_data.3 %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
# Quick check of data
#
# GPP
hist(Q1_flux$GPP) # Several 0s
hist(sqrt(Q1_flux$GPP)) # Right skewed
hist(log(Q1_flux$GPP)) # Left skewed
#
# NEE
hist(Q1_flux$NEE) # Several negative values, but normal distribution around 0.5
hist(Q1_flux$NEE+5) # Slight right skew
hist(sqrt(Q1_flux$NEE+5)) # Still slightly skewed
hist(log(Q1_flux$NEE+5)) # one overrepresented point
#
# Resp
hist(Q1_flux$Resp) # Right skew
hist(sqrt(Q1_flux$Resp)) # Sort of ok
hist(log(Q1_flux$Resp)) # Left skew



Q1_flux %>%
  ggplot(aes(x = factor(Round, levels = order(levels(Round))), y = GPP, color = Block)) + geom_point()






# GPP
#
# Transform data
Q1_flux_GPP <- Q1_flux %>%
  select(Round, Block, Species, GPP, AirT, PAR, SoilT, SoilM) %>%
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
lme1_NEE <- lme(plusNEE ~ Round*Species + AirT + PAR + SoilT + SoilM,
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



# Resp
#
# Transform data
Q1_flux_Resp <- Q1_flux %>%
  select(Round, Block, Species, Resp, AirT, PAR, SoilT, SoilM) %>%
  mutate(Resp = if_else(Resp < 0, 0, Resp)) %>%
  mutate(logResp = log(Resp+1),
         sqrtResp = sqrt(Resp),
         cubeResp = Resp^(1/3),
         sqResp = Resp^2,
         ashinResp = log(Resp + sqrt(Resp^2 + 1)), # inverse hyperbolic sine transformation
         arcResp = asin(sqrt(((Resp)/10000))),
         ihsResp = ihs(Resp))
#
lme1_Resp <- lme(sqrtResp ~ Round*Species + AirT + PAR + SoilT + SoilM,
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
# Summary data
NEE_sum <- summarySE(Flux_data.3, measurevar = "NEE", groupvars = c("Round", "Species"))
Resp_sum <- summarySE(Flux_data.3, measurevar = "Resp", groupvars = c("Round", "Species"))
GPP_sum <- summarySE(Flux_data.3, measurevar = "GPP", groupvars = c("Round", "Species"))

# NEE
NEE_sum %>%
  ggplot() +
  geom_errorbar(aes(x = Round, y = NEE, ymin=NEE, ymax=NEE+se), position=position_dodge(.9)) +
  geom_col(aes(x = Round, y = NEE), color = "black") +
  scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 5, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Round", y = expression("NEE (µmol "*m^-2*s^-1*")"), title = "Net Ecosystem Exchange") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"),axis.text.x=element_text(angle=60, hjust=1))
#
# Respiration
Resp_sum %>%
  ggplot() +
  geom_errorbar(aes(x = Round, y = Resp, ymin=Resp, ymax=Resp+se), position=position_dodge(.9)) +
  geom_col(aes(x = Round, y = Resp), color = "black") +
  #scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 5, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Round", y = expression(R[e]*" (µmol "*m^-2*s^-1*")"), title = "Ecosystem respiration") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"),axis.text.x=element_text(angle=60, hjust=1))

#
# GPP
GPP_sum %>%
  ggplot() +
  geom_errorbar(aes(x = Round, y = GPP, ymin=GPP, ymax=GPP+se), position=position_dodge(.9)) +
  geom_col(aes(x = Round, y = GPP), color = "black") +
  #scale_x_discrete(labels = measuringPeriod) +
  facet_wrap( ~ Species, ncol = 5, scales = "free") + 
  #coord_cartesian(ylim = c(0,150)) +
  labs(x = "Round", y = expression("GPP (µmol "*m^-2*s^-1*")"), title = "Ecosystem gross primary production") + 
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(2, "lines"),axis.text.x=element_text(angle=60, hjust=1))
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