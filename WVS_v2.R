#-----------------loading packages and data -----------------------------
library(tidyverse)
library(dplyr)
#detach("package:dplyr", unload = TRUE)
library(readxl)
library(labelled)
library(ggplot2)
library(RColorBrewer)
library(stats)
#library(lme4)
#detach("package:lme4", unload = TRUE)
library(haven)
library(nlme)
library(multigroup)

library(naniar)
install.packages("VIM")
library(VIM)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("missMDA")
library(missMDA)

WVS <- readRDS("WVS_TimeSeries_R_v1_6.rds")
WVS_meta <- read_excel("F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx",skip = 1)
worldbank_org <- read_csv("fd2a9196-5642-4895-828d-e460731ed79c_Data.csv")
worldbank_meta <- read_csv("fd2a9196-5642-4895-828d-e460731ed79c_Series - Metadata.csv")

#--------------------Wranggling meta data to select desired variables-----------------------------------
#count the number of NAs for each variable
WVS_meta$numNAs <- WVS_meta %>% 
  select(starts_with("WVS wave")) %>% 
  apply(1, function(x) {sum(is.na(x))})

#keep only rows with less than 3 NAs
WVS_meta_temp <- WVS_meta[-c(1:23),] %>% 
  filter(numNAs < 3)

WVS_meta_reduced <- rbind(WVS_meta[c(1:23),], WVS_meta_temp)
View(WVS_meta_reduced)

##Now we can see all variables measured in at least 4 waves
##To investigate change in values over time, we want to use the measures that were used in
##enough time points

#------------------Wranggling WVS data for longitudinal analysis----------------------------------------------------

##from the reduced meta data we can select the variables that we are interested in to compare across time


#Variables start with "s" are administrative variables,select the ones we want:
admin_keep <- c("S002","S003", "S007", "S020")

#Variables start with "A"  and "F" pertain to attitudes in the personal realm
#select: importance in life, happiness, important child qualities, 
#        generalized trust

imp_in_life <- c(paste0("A00", 1:6))
imp_in_life_rel <- c("A006")
life_meaning <- c("F001")
Religiosity <- c("F034")
Justify <- c("F114A", paste0("F1", 15:23))


#Variable categories B to E pertain more to people's beliefs about society
#select: environment vs economy, priority to job, family structure, gender beliefs
#        future changes, political orientation
envlist <- c("B008")
#fam_struct <- c("D018", "D022", "D023")
#job_priority <- c("C001", "C002")
gend_belief <- c("D057", "D059", "D060")



#Variable category X is demographics
dmgraphics <- c(paste0("X00", 1:3))
education <- c("X025")


#subset the WVS data
WVS_acstime <- WVS %>% select(admin_keep,dmgraphics, education, imp_in_life,hap, neighbours, gen_trust,
                              life_meaning,Religiosity, Justify, envlist, fam_struct,
                              gend_belief)
WVS_missing <- WVS %>% select(admin_keep,imp_in_life_rel,
                              Religiosity, Justify, envlist,
                              gend_belief)


#rename some variables so that they make more sense
WVS_acstime <- WVS_acstime %>%
  rename("Wave" = S002, "Country" = S003, "respondent_no" = S007, "Year" = S020, "Sex" = X001, "Age" = X003,
         "Education" = X025, "ImpinLife_Family" = A001, "ImpinLife_Friends" = A002, "ImpinLife_Leisure" = A003,
         "ImpinLife_Politics" = A004, "ImpinLife_Work" = A005, "ImpinLife_Religion" = A006,
         "Happiness" = A008, "gen_trust" = A165, "Religiosity" = F034, "Environment" = B008)
#country label instead of code
WVS_acstime$Country <- as_factor(WVS_acstime$Country)
#create a variable to show the number of waves each country was in
count_by_wave <- WVS_acstime %>% group_by(Wave) %>% summarise(Country = unique(Country))
freq_country <- as.data.frame(table(count_by_wave$Country))
freq_country <-rename(freq_country,Country = Var1, Country_counts = Freq)
WVS_acstime <- merge(WVS_acstime, freq_country, by = "Country")

#let's have an overview of all the variables, check variable types and see if recoding is needed
summary(WVS_acstime)

#There are a few topics we are interested in: attitudes and beliefs about religion, minority groups,
#female's role, the environment; as a bonus we can look at generalized trust and how much people think
#about the purpose of life as well.


#------------------------ Wrangling world bank data -----------------------------------------------

wvs_worldbank_country_names <- read_csv("wvs_worldbank_countrynames_match.csv")

worldbank$Country[worldbank$Country %in% wvs_worldbank_country_names$worldbank_name
] <- wvs_worldbank_country_names$wvs_name[match(worldbank$Country[worldbank$Country %in% wvs_worldbank_country_names$worldbank_name], wvs_worldbank_country_names$worldbank_name)]

#select desired variables
worldbank <- worldbank %>% filter(`Series Name` %in% c("Urban population growth (annual %)","GDP per capita (current US$)","GDP per capita growth (annual %)","CO2 emissions (metric tons per capita)") )
worldbank <- worldbank %>% rename(Series_Name = `Series Name`)

worldbank <- worldbank %>% select(c("Series_Name", "Country", "1994 [YR1994]", "1998 [YR1998]", "2004 [YR2004]", "2009 [YR2009]", "2014 [YR2014]", "2019 [YR2019]"))
worldbank <- pivot_longer(worldbank, cols = c("1994 [YR1994]", "1998 [YR1998]", "2004 [YR2004]", "2009 [YR2009]", "2014 [YR2014]", "2019 [YR2019]") , names_to = "Wave", values_to = "value")
worldbank <- pivot_wider(worldbank, names_from = Series_Name, values_from = value)
worldbank <- rename(worldbank, urb_pop_growth = "Urban population growth (annual %)", GDP_per_cap = "GDP per capita (current US$)", CO2_emission = "CO2 emissions (metric tons per capita)", GDP_growth = "GDP per capita growth (annual %)")
worldbank$Wave <- recode(worldbank$Wave,"1994 [YR1994]" = "2","1998 [YR1998]" = "3", "2004 [YR2004]" = "4", "2009 [YR2009]" = "5", "2014 [YR2014]" = "6", "2019 [YR2019]" = "7")


#--------------------------------missing value analysis for WVS--------------------------------------------------------

WVS_missing <- WVS_missing %>% rename("Wave" = S002, "Country" = S003)

WVS_missing$Country <- as_factor(WVS_missing$Country)

gg_miss_var(WVS_missing)

                    
missing_val <- WVS_missing %>%
               group_by(Wave, Country) %>%
                miss_var_summary()

missing_val <- as.data.frame(missing_val)
                    
missing_val <- missing_val %>% arrange(desc(pct_miss))
View(missing_val)
                    
matrixplot(WVS_missing, sortby = 2)            
                    
#---------------------------------Religion--------------------------------------------------------------
##We have 3 measures related to religion, let's have a look at them 

##importance in life: religion
summary(WVS_acstime$ImpinLife_Religion) #no need to recode for NA's, but will make more sense to reverse the scale

WVS_acstime$ImpinLife_Religion = as.numeric(5-WVS_acstime$ImpinLife_Religion)

impReligion <- WVS_acstime %>% select(Wave, Country, ImpinLife_Religion)




country_count_impReligion <- impReligion %>% filter(Wave != 1) %>% group_by(Wave) %>% summarise(Country = unique(Country))
freq_country_impReligion <- as.data.frame(table(country_count_impReligion$Country))
freq_country_impReligion <-rename(freq_country_impReligion,Country = Var1, Country_counts_impReligion = Freq)
impReligion <- merge(impReligion,freq_country_impReligion, by = "Country")

bp_impReligion_wave <- ggplot(filter(impReligion,Country_counts_impReligion == 6),aes(y = ImpinLife_Religion, x = Wave, group = Wave))+
  geom_violin(colour = "skyblue")
bp_impReligion_wave #wave 7 seems negatively skewed, but other waves seem fine, perhaps using mean is better than median
#because each wave contains a different set of countries, so not a good idea to look at
#the distribution of the entire wave
bp_impReligion <- ggplot(filter(impReligion,Country_counts_impReligion > 4),aes(y = ImpinLife_Religion, x = Wave, group = Wave))+
  geom_boxplot(colour = "skyblue")+
  facet_wrap(~Country)+
  theme_classic()

bp_impReligion # some countries are extremely skewed some are not
              #outliers not concerning because they are reasonable response to the question


#all good. let's calculate the mean

impReligion_stats_byWave <- impReligion %>% filter(Country_counts_impReligion == 6) %>%
  group_by(Wave) %>%
  summarise(Importance_of_Religion = mean(ImpinLife_Religion,na.rm = TRUE),SD_Religion = sd(ImpinLife_Religion,na.rm = TRUE))

impReligion_stats_byBoth <- impReligion %>% filter(Country_counts_impReligion >5) %>%
  group_by(Wave,Country) %>%
  summarise(Importance_of_Religion = mean(ImpinLife_Religion,na.rm = TRUE),SD_Religion = sd(ImpinLife_Religion,na.rm = TRUE))


View(impReligion_stats_byWave)
View(impReligion_stats_byBoth)

#let's plot the trend
#by wave
lp_impReligion_wave <- ggplot(impReligion_stats_byWave, aes(x = Wave,y = Importance_of_Religion)) +
  geom_line()+ylim(1,4)
lp_impReligion_wave

#by wave and country
lp_impReligion_both <- ggplot(impReligion_stats_byBoth, aes(x = Wave,y = Importance_of_Religion, colour = Country), labs = TRUE) +
  geom_line()
lp_impReligion_both #looks like religiosity is only decreasing in some countries but not others

lp_impReligion_both_withwave <- ggplot(impReligion_stats_byBoth, aes(x = Wave,y = Importance_of_Religion, colour = Country), labs = TRUE) +
  geom_line() + geom_line(data = cbind(impReligion_stats_byWave, Country="Overall"))
lp_impReligion_both_withwave

# let's do a correlation to see if these trends are statistically significant
#only meaningful for countries measured in enough number of years
rel_for_cor <- impReligion %>% filter(Country_counts_impReligion == 6)
cor_Religion <- cor.test(rel_for_cor$ImpinLife_Religion,rel_for_cor$Wave) 
cor_Religion #there is a significant negative correlation, but we need to take into account the multilevel structure

multilevel_Religion <- lme(Importance_of_Religion ~ Wave, random = ~ Wave | Country, data = impReligion_stats_byBoth, na.action = na.omit)
summary(multilevel_Religion)

##Religiosity
summary(WVS_acstime$Religiosity)

#categorical data, transform to factor
WVS_acstime$Religiosity <- factor(WVS_acstime$Religiosity, labels = c("religious","not religious", "convinced athiest"))
summary(WVS_acstime$Religiosity)
#again let's make a variable out of it
Religiosity <- WVS_acstime %>% select(Wave, Country, Religiosity)

country_count_Religiosity <- Religiosity  %>% group_by(Wave) %>% summarise(Country = unique(Country))
freq_country_Religiosity <- as.data.frame(table(country_count_Religiosity$Country))
freq_country_Religiosity <-rename(freq_country_Religiosity,Country = Var1, Country_counts_Religiosity = Freq)
Religiosity <- merge(Religiosity,freq_country_Religiosity, by = "Country")

max(Religiosity$Country_counts_Religiosity)

#for categorical variables, compute rate
Religiosity_stats_by_both <- Religiosity %>% filter(Country_counts_Religiosity >5) %>% group_by(Wave, Country) %>% summarise(religious = sum(Religiosity == "religious", na.rm = TRUE)/n())
Religiosity_stats_by_wave <- Religiosity %>% filter(Country_counts_Religiosity == 7) %>% group_by(Wave) %>% summarise(religious = sum(Religiosity == "religious", na.rm = TRUE)/n())


#too many countries per wave for this variable, let's just inspect the table and check the few countries that seem strange
strange <- Religiosity_stats_by_both %>% filter(Country %in% c("South Korea", "Poland", "China", "Pakistan", "United Kingdom", "Israel", "Morocco", "Singapore"))
View(strange)
summary(filter(Religiosity,Country == "South Korea", Wave == "1")$Religiosity)
#remove countries with 0 percentage
Religiosity_stats_by_both <- Religiosity_stats_by_both %>% filter(religious != 0)
View(Religiosity_stats_by_both)

Religiosity_stats_by_wave <- Religiosity_stats_by_wave %>% filter(religious != 0)
View(Religiosity_stats_by_wave)

#good. let's see the trend
lp_Religiosity_wave <- ggplot(Religiosity_stats_by_wave, aes(x = Wave, y = religious))+
  geom_line() + ylim(0.2,0.6)
lp_Religiosity_wave

lp_Religiosity_both <- ggplot(Religiosity_stats_by_both, aes(x = Wave,y = religious, colour = Country), labs = FALSE)+
  geom_line()
lp_Religiosity_both

##Religious tolerance
# Rel_tor <- WVS_acstime %>% select(Wave, Country, A124_12)
# Rel_tor <- WVS_acstime %>% rename(tolerance = A124_12)
# summary(Rel_tor$tolerance)
# Rel_tor$tolerance <- as_factor(Rel_tor$tolerance)
# summary(Rel_tor$tolerance)
# 
# country_counts_tor <- Rel_tor  %>% group_by(Wave) %>% summarise(Country = unique(Country))
# freq_country_RelTor <- as.data.frame(table(country_counts_tor$Country))
# freq_country_RelTor <-rename(freq_country_RelTor,Country = Var1, Country_counts_tolerance = Freq)
# Rel_tor <- merge(Rel_tor,freq_country_RelTor, by = "Country")
# 
# max(Rel_tor$Country_counts_tolerance)
# 
# tolerance_stats_by_both <- Rel_tor %>% filter(Country_counts_tolerance > 5) %>% group_by(Wave, Country) %>% summarise(tolerant = sum(tolerance == 0, na.rm = TRUE)/n())
# tolerance_stats_by_wave <- Rel_tor %>% filter(Country_counts_tolerance == 7) %>% group_by(Wave) %>% summarise(tolerant = sum(tolerance == 0, na.rm = TRUE)/n())
# 
# View(tolerance_stats_by_both)


#---------------------------environmental concern-----------------------------------------------------------

env <- WVS_acstime %>% select(Wave, Country, Environment)
summary(env$Environment) #recode 3(other answer) to NA
env$Environment <- na_if(env$Environment,3)


country_count_env <- env  %>% group_by(Wave) %>% summarise(Country = unique(Country))
freq_country_env <- as.data.frame(table(country_count_env$Country))
freq_country_env <-rename(freq_country_env,Country = Var1, Country_counts_env = Freq)
env <- merge(env,freq_country_env, by = "Country")

max(env$Country_counts_env)
env$Environment<- factor(env$Environment, labels = c("environment", "economy"))
#for categorical variables, compute rate
env_stats_by_both <- env %>% filter(Country_counts_env == 5) %>% group_by(Wave, Country) %>% summarise(environment = sum(Environment == "environment", na.rm = TRUE)/n())
env_stats_by_wave <- env %>% filter(Country_counts_env == 5) %>% group_by(Wave) %>% summarise(environment = sum(Environment == "environment", na.rm = TRUE)/n())
env_stats_by_both <- env_stats_by_both %>% filter(environment != 0)
env_stats_by_wave <- env_stats_by_wave %>% filter(environment != 0)
View(env_stats_by_wave)
View(env_stats_by_both)

#check distribution
env_plot <- env %>% filter(Wave %in% c(3:7))
env_plot <- env_plot %>% filter(Country_counts_env > 5)
dist_env <- ggplot(env_plot,aes(x = Environment))+
  geom_histogram(stat = "count", alpha=0.6, binwidth = 3)+
  facet_grid(rows = vars(Country), cols = vars(Wave))
dist_env
#plot the trend

lp_env_wave <- ggplot(env_stats_by_wave, aes(x = Wave, y = environment))+
  geom_line() + ylim(0.2,0.6)
lp_env_wave

lp_env_both <- ggplot(env_stats_by_both, aes(x = Wave,y = environment, colour = Country), labs = FALSE)+
  geom_line()
lp_env_both

#----------------------------------------Justifiable---------------------------------------------------------
Just <- WVS_acstime %>% select(Wave, Country, Justify)
#reverse code so that high value means more tolerance
Just <- Just %>% rename(F114 = F114A)

summary(Just)

country_count_Just <- Just  %>% group_by(Wave) %>% summarise(Country = unique(Country))
freq_country_Just <- as.data.frame(table(country_count_Just$Country))
freq_country_Just <-rename(freq_country_Just,Country = Var1, Country_counts_Just = Freq)
Just <- merge(Just,freq_country_Just, by = "Country")


#good. let's do a PCA
Just_pca <- Just %>% filter(Wave == 7) %>% drop_na() 
# View(Just_pca)
# 
# just.pca <- mgPCA(Just_pca[,-c(1,2)],Just_pca$Country)
#just.pca <- princomp(Just_pca[,-c(1,2)], cor = TRUE, scores = TRUE)
pca_just_rotated <- psych::principal(Just_pca[,-c(1,2)], rotate="varimax", nfactors=2, scores=TRUE)
pca_just_rotated$values
barplot(pca_just_rotated$values, main = "Scree Plot")

pca_just_rotated$loadings

#from the PCA it is clear that the variables load on two components
#PC1 pertains to tolerance on legal offences - let's call it legal
#PC2 pertains to moral judgment about minority groups or debated issues - let's call it moral

Just$legal <- rowMeans(Just[c("F114", "F115", "F116", "F117")], na.rm = TRUE)
Just$moral <- rowMeans(Just[c("F118", "F119", "F120", "F121", "F122", "F123")], na.rm = TRUE)
View(Just)

summary(Just$legal)
summary(Just$moral) #there's lots of NAs, but they mainly come from missing values in items
                    #since we have enough data, we can just remove the NAs

#check boxplot

 long_Just <- pivot_longer(Just, col = c("legal", "moral"), names_to = "judgment", values_to = "value")
# bp_just <- ggplot(filter(long_Just,Country_counts_Just > 5),aes(y = value, x = judgment, group = judgment))+
#   geom_boxplot()+
#   facet_wrap(~Country)
# bp_just

dist_just <- ggplot(filter(long_Just,Country_counts_Just > 6),aes(x = value, fill = judgment))+
  geom_histogram(alpha=0.6)+
  facet_grid(rows = vars(Country), cols = vars(Wave))
dist_just
#let's get the means
Just_stats_bywave<- Just %>% filter (Country_counts_Just > 6) %>% group_by(Wave) %>% summarise(MeanLegal = mean(legal, na.rm = TRUE),MeanMoral = mean(moral,na.rm = TRUE)) %>% ungroup()
Just_stats_byboth<- Just %>% filter (Country_counts_Just > 5) %>% group_by(Wave,Country) %>% summarise(MeanLegal = mean(legal, na.rm = TRUE),MeanMoral = mean(moral,na.rm = TRUE)) %>% ungroup()

long_Just_stats_bywave <- pivot_longer(Just_stats_bywave, col = c("MeanLegal", "MeanMoral"), names_to = "judgment", values_to = "value")
long_Just_stats_byboth <- pivot_longer(Just_stats_byboth, col = c("MeanLegal", "MeanMoral"), names_to = "judgment", values_to = "value")

line_just_bywave <- ggplot(long_Just_stats_bywave,aes(x = Wave, y = value, colour = judgment))+
  geom_line()
line_just_bywave #there is a clear trend suggesting that moral oppennes has increased

line_just_byboth <- ggplot(long_Just_stats_byboth,aes(x = Wave, y = value, colour = judgment))+
  geom_line()+
  facet_wrap(~Country)
line_just_byboth

cor_for_Just <- Just %>% filter(Country_counts_Just ==7)
reg_for_Just <- Just %>% filter(Country_counts_Just == 7)
cor_moral <- cor.test(cor_for_Just$moral,cor_for_Just$Wave) 
cor_moral #there is a significant negative correlation, but we need to take into account the multilevel structure


cor_legal <- cor.test(cor_for_Just$legal,cor_for_Just$Wave) 
cor_legal

multilevel_moral <- lme(MeanMoral ~ Wave, random = ~ Wave | Country, data = Just_stats_byboth, na.action = na.omit)
summary(multilevel_moral)

multilevel_legal <- lme(MeanLegal ~ Wave, random = ~ Wave | Country, data = Just_stats_byboth, na.action = na.omit)
summary(multilevel_legal)


#--------------------------------------Gender belief---------------------------------------------------------------------
gend <- WVS_acstime %>% select(Wave, Country, gend_belief)

summary(gend)

#-----------------------creating country-level WVS data--------------------------------------------------
WVS_country <- merge(Just_stats_byboth,Religiosity_stats_by_both, by = (c("Country", "Wave")), all = TRUE)
WVS_country <- merge(WVS_country, impReligion_stats_byBoth, by = (c("Country", "Wave")), all = TRUE)
WVS_country <- merge(WVS_country, env_stats_by_both, by = (c("Country", "Wave")), all = TRUE)

view(WVS_country)

#let's create some w7 data
impReligion_stats_W7 <- impReligion %>% filter(Wave == 7) %>%
  group_by(Country) %>%
  summarise(Importance_of_Religion = mean(ImpinLife_Religion,na.rm = TRUE),SD_Religion = sd(ImpinLife_Religion,na.rm = TRUE))

Religiosity_stats_W7 <- Religiosity %>% filter(Wave == 7) %>% group_by(Country) %>% summarise(religious = sum(Religiosity == "religious", na.rm = TRUE)/n())
Religiosity_stats_W7 <- Religiosity_stats_W7 %>% filter(religious != 0)

Just_stats_W7<- Just %>% filter (Wave == 7) %>% group_by(Country) %>% summarise(MeanLegal = mean(legal, na.rm = TRUE),MeanMoral = mean(moral,na.rm = TRUE)) %>% ungroup()


env_stats_W7 <- env %>% filter(Wave == 7) %>% group_by(Country) %>% summarise(environment = sum(Environment == "environment", na.rm = TRUE)/n())
env_stats_W7 <- env_stats_W7 %>% filter(environment != 0)

env_stats_W6 <- env %>% filter(Wave == 6) %>% group_by(Country) %>% summarise(environment = sum(Environment == "environment", na.rm = TRUE)/n())
env_stats_W6 <- env_stats_W6 %>% filter(environment != 0)

WVS_W7 <- merge(Just_stats_W7, impReligion_stats_W7, by = "Country")
WVS_W7 <- merge(WVS_W7, Religiosity_stats_W7,by = "Country")
WVS_W7 <- merge(WVS_W7, env_stats_W7,by = "Country")
View(WVS_W7)


#------------------------country-level--------------------------------------------------------------
#let's have a look at the worldbank data

worldbank$GDP_per_cap <- as.numeric(worldbank$GDP_per_cap)
worldbank$CO2_emission <- as.numeric(worldbank$CO2_emission)
worldbank$urb_pop_growth <- as.numeric(worldbank$urb_pop_growth)
worldbank$GDP_growth <- as.numeric(worldbank$GDP_growth)

#let's look at changes across time
GDP <- ggplot(filter(worldbank, 
                     Country %in% c("Argentina", "Chile", "Japan", "China", "United States", "South Korea", "Mexico", "South Africa", "Turkey")), 
              aes(x = Wave, y = GDP_per_cap, group = Country, colour = Country))+
  geom_line()
  #facet_wrap(~Country)
GDP

CO2 <- ggplot(filter(worldbank, 
                     Country %in% c("Argentina", "Chile", "Japan", "China", "United States", "South Korea", "Mexico", "South Africa", "Turkey")), 
              aes(x = Wave, y = CO2_emission, group = Country, colour = Country))+
  geom_line()
  #facet_wrap(~Country)
CO2

# GDP_growth <- ggplot(filter(worldbank, 
#                      Country %in% c("Argentina", "Chile", "Japan", "China", "United States", "South Korea", "Mexico", "South Africa", "Turkey")), 
#               aes(x = Wave, y = GDP_growth, group = Country, colour = Country))+
#   geom_line()
# GDP_growth

# urbran_pop <- ggplot(filter(worldbank, 
#                             Country %in% c("Argentina", "Chile", "Japan", "China", "United States", "South Korea", "Mexico", "South Africa", "Turkey")), 
#                      aes(x = Wave, y = urb_pop_growth, group = Country, colour = Country))+
#   geom_line()
# urbran_pop 
#facet_wrap(~Country)

#------------------------------country-level x WVS-----------------------------------------------------
worldbank_reduced <- worldbank %>% filter(Country%in% c("Argentina", "Chile", "Japan", "China", "United States", 

                                                          "South Korea", "Mexico", "South Africa", "Turkey" ))
worldbank_for_env <- worldbank %>% filter(Country %in% c("Australia","Brazil", "India","Nigeria",
                                                           "Peru", "Russia", "Spain", "Sweden"))
                                                          
worldbank_W7 <- worldbank %>% filter(Wave == 7)
worldbank_W6 <- worldbank %>% filter(Wave == 6)
worldbank_W7$Wave <- NULL
WVS_WB <- merge(WVS_country,worldbank, by = c("Wave","Country"))
WVS_WB_W7 <- merge(WVS_W7,worldbank_W7, by = "Country")
env_WB_W6 <- merge(env_stats_W6,worldbank_W6, by = "Country")
env_WB <- merge(env_stats_by_both, worldbank_for_env, by = c("Country","Wave"))
#-------------------------------------correlating gdp and moral openness------------------------------------------
#across time
gdp_moral <- ggplot(WVS_WB, aes(x = GDP_per_cap, y = MeanMoral, colour = Country, group = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Wave)

gdp_moral

#within each country
gdp_moral_wthincount <- ggplot(WVS_WB, aes(x = GDP_per_cap, y = MeanMoral, colour = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Country)
gdp_moral_wthincount #using this we can directly address the second question, within each country (except China), gdp is positively related to moral openness
#let's do a multilevel regression on this
multilevel_moral_gdp <- lme(MeanMoral ~ GDP_per_cap, random = ~ GDP_per_cap | Country, data = WVS_WB, na.action = na.omit)
summary(multilevel_moral_gdp)



#at w7
gdp_moral_W7 <- ggplot(WVS_WB_W7, aes(x = GDP_per_cap, y = MeanMoral))+
  geom_point(colour = "skyblue")+
  geom_smooth(method = "lm",colour = "navy")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1)
gdp_moral_W7

#we have enough data points now, let's do a correlation
cor_moral_gdp <- cor.test(WVS_WB_W7$MeanMoral,WVS_WB_W7$GDP_per_cap)
cor_moral_gdp # highly correlated

#cluster analysis
moral_for_clust <- WVS_WB_W7 %>% select(MeanMoral,GDP_per_cap)%>% drop_na()
#rel_for_clust <- scale(rel_for_clust[-1])
moral_fit <- kmeans(moral_for_clust, 4)
moral_fit
moral_cluster_df <- data.frame(moral_for_clust)
moral_cluster <- factor(moral_fit$cluster)

cluster_moral <- ggplot(data = moral_cluster_df, aes(
  x = GDP_per_cap, y = MeanMoral, colour = moral_cluster)) + 
  geom_point()+ 
  scale_fill_brewer(palette = "Set3")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1, data = WVS_WB_W7)

cluster_moral
# gdp_moral_W7_lowend <- ggplot(filter(WVS_WB_W7,GDP_per_cap < 5000), aes(x = GDP_per_cap, y = MeanMoral))+
#   geom_point(colour = "skyblue")+
#   geom_text(aes(label = Country),
#             color = "black", size = 2.5, hjust = 1.1)
# 
# 
# 
# gdp_moral_W7_lowend

#-----------------------------correlating gdp and tolerance to legal offences-----------------------------------
#across time
gdp_legal <- ggplot(WVS_WB, aes(x = GDP_per_cap, y = MeanLegal, colour = Country, group = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Wave)
gdp_legal 

#within each country
gdp_legal_wthincount <- ggplot(WVS_WB, aes(x = GDP_per_cap, y = MeanLegal, colour = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Country)
gdp_legal_wthincount #using this we can directly address the second question, within each country (except China), gdp is positively related to moral openness


#at w7
gdp_legal_W7 <- ggplot(WVS_WB_W7, aes(x = GDP_per_cap, y = MeanLegal))+
  geom_point(colour = "skyblue")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1)
gdp_legal_W7

cor_legal_gdp <- cor.test(WVS_WB_W7$MeanLegal,WVS_WB_W7$GDP_per_cap)
cor_legal_gdp # highly correlated though smaller effect

#---------------------------------------------GDP with religiosity--------------------------------------------------
#across time
gdp_religious <- ggplot(WVS_WB, aes(x = GDP_per_cap, y = religious, colour = Country, group = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Wave)
gdp_religious 

#within each country
gdp_religious_wthincount <- ggplot(WVS_WB, aes(x = GDP_per_cap, y = religious, colour = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Country)
gdp_religious_wthincount #using this we can directly address the second question, within each country (except China), gdp is positively related to moral openness


#at w7
gdp_religious_W7 <- ggplot(WVS_WB_W7, aes(x = GDP_per_cap, y = religious))+
  geom_point(colour = "skyblue")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1)
gdp_religious_W7

cor_religious_gdp <- cor.test(WVS_WB_W7$religious,WVS_WB_W7$GDP_per_cap)
cor_religious_gdp # highly correlated though smaller effect

#cluster analysis
rel_for_clust <- WVS_WB_W7 %>% select(religious,GDP_per_cap, GDP_growth, CO2_emission)%>% drop_na()
#rel_for_clust <- scale(rel_for_clust[-1])
rel_fit <- kmeans(rel_for_clust, 4, nstart = 25)
rel_fit
rel_cluster_df <- data.frame(rel_for_clust)
rel_cluster <- factor(rel_fit$cluster)

cluster_religious <- ggplot(data = rel_cluster_df, aes(
  x = GDP_per_cap, y = religious, colour = rel_cluster)) + 
  geom_point()+ 
  scale_fill_brewer(palette = "Set3")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1, data = filter(WVS_WB_W7, Country != "Macau SAR"))


cluster_religious

###

WVS_WB_wider <- select(WVS_WB,Country, Wave, religious) %>% pivot_wider(names_from = Wave, values_from = religious)
View(WVS_WB_wider)
write_csv(WVS_WB_wider,"WVS_WB_wider.csv")
WVS_WB_wider <- read_csv("WVS_WB_wider.csv")
WVS_WB_wider <- WVS_WB_wider %>% select(Country, Rel_def)
WVS_WB_W7 <- merge(WVS_WB_W7, WVS_WB_wider, by = "Country")

rel_for_clust <- WVS_WB_W7 %>% select(Rel_def,GDP_growth)%>% drop_na()
#rel_for_clust <- scale(rel_for_clust[-1])
rel_fit <- kmeans(rel_for_clust, 3)
rel_fit
rel_cluster_df <- data.frame(rel_for_clust)
rel_cluster <- factor(rel_fit$cluster)

cluster_religious <- ggplot(data = rel_cluster_df, aes(
  x = GDP_growth, y = Rel_def, colour = rel_cluster)) + 
  geom_point()+ 
  scale_fill_brewer(palette = "Set3")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1, data = WVS_WB_W7)


cluster_religious

#-------------------------------------C02 and environment--------------------------------------------------
#across time
env_WB <- env_WB %>% filter(Wave != 7)
co2_env <- ggplot(env_WB, aes(x = CO2_emission, y = environment, colour = Country, group = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Wave)
co2_env 
View(env_WB)

#within each country
co2_env_wthincount <- ggplot(env_WB, aes(x = CO2_emission, y = environment, colour = Wave))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Country)
co2_env_wthincount

#at w6
co2_env_W6 <- ggplot(env_WB_W6_filtered , aes(x = CO2_emission, y = environment))+
  geom_point(colour = "skyblue")+
  geom_text(aes(label = Country),
            color = "black", size = 2.5, hjust = 1.1)
co2_env_W6

cor_env_co2 <- cor.test(env_WB_W6_filtered$environment,env_WB_W6_filtered$CO2_emission)
cor_env_co2 #
 
env_WB_W6_filtered <- env_WB_W6 %>% filter(!Country %in% c("Qatar", "Kuwait"))


