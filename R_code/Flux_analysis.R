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
#



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
# Boxplot
Flux_data.2 %>%
  mutate(MP = fct_inorder(MP)) %>%
  ggplot(aes(x = MP, y = GPP)) + geom_boxplot() + facet_wrap(~Species, scales = "free")
#
# Histogram
Flux_data.2 %>%
  ggplot(aes(x = GPP)) + geom_histogram() + facet_wrap(~Species)


# Next: 
# Combine with environmental data?
# 

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
  select(1:4, GPP) %>%
  mutate(logGPP = log(GPP+1),
         sqrtGPP = sqrt(GPP),
         cubeGPP = GPP^(1/9),
         sqEt_prod = GPP^2,
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

#
#
#
#=======  ■  { The End }    ■ =======